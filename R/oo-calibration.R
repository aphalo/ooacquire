#' Convert an OO calibration
#'
#' Convert an irradiance calibration as supplied by Ocean Optics into the
#' format used by the functions in this package. #'
#'
#' @param x generic_spct object with variables \code{w.length} and \code{oo.cal}.
#' @param area numeric area of the cosine diffuser (mm2).
#' @param diff.type character value giving type of diffuser as in OO's documents, case
#'   insensitive.
#' @param verbose Logical indicating the level of warnings wanted.
#'
#' @export
#'
#' @return a \code{generic_spct} object  of the same length as \code{x} containing
#' the re-scaled calibration factors in variable \code{coeffs}.
#'
oo_calib2irrad_mult <-
  function(x,
           area = NULL,
           diff.type = NULL,
           verbose = getOption("photobiology.verbose", default = FALSE)) {
    stopifnot(is.generic_spct(x))
    stopifnot(xor(is.null(area), is.null(diff.type)))
    if (is.null(area)) {
      area <-
        switch(toupper(diff.type),
               "CC-3-DA" = pi * (7.14 / 2)^2,
               "CC-3" = pi * (3.9 / 2)^2,
               "CC-3-UV-S" = pi * (3.9 / 2)^2,
               "CC-3-UV-T" = pi * (3.9 / 2)^2,
               NA_real_)
    }
    wl.steps <- diff(x[["w.length"]])
    wl.steps <- c(wl.steps[1], wl.steps)
    wl.steps <- caTools::runmean(wl.steps, k = 2)
    x[["irrad.mult"]] <- x[["oo.cal"]] / (area * wl.steps)
    if (!verbose) {
      x[["oo.cal"]] <- NULL
    }
    setCalibrationSpct(x)
  }

#' Read OO irradiance calibration.
#'
#' Reads and parses the header of a calibration data file as supplied by
#' Ocean Optics. The whole header is stored as a comment. The time is
#' retrieved and decoded.
#'
#' @param file character string
#' @param time a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} no date variable is added
#' @param geocode A data frame with columns \code{lon} and \code{lat}.
#' @param label character string, but if \code{NULL} the value of \code{file} is
#'   used, and if \code{NA} the "what.measured" attribute is not set.
#' @param descriptor list A list describing the instrument used.
#' @param tz character Time zone is by default read from the file.
#' @param locale	The locale controls defaults that vary from place to place. The
#'   default locale is US-centric (like R), but you can use
#'   \code{\link[readr]{locale}} to create your own locale that controls things
#'   like the default time zone, encoding, decimal mark, big mark, and day/month
#'   names.
#' @param verbose Logical indicating the level of warnings wanted.
#'
#' @return A generic_spct object.
#' @export
#'
read_oo_caldata <-
  function(file,
           time = NULL,
           geocode = NULL,
           label = NULL,
           descriptor = NULL,
           tz = NULL,
           locale = NULL,
           verbose = getOption("photobiology.verbose", default = FALSE)) {

    label <- paste("File: ", basename(file), label, sep = "")

    line01 <- scan(file = file, nlines = 1, skip = 0,
                   what = "character", quiet = !verbose)
    if (line01[1] != "Date") {
      warning("Input file may not be a calibration file as expected: skipping")
      return(NA)
    }
    file_header <- scan(file = file, nlines = 9,
                        skip = 0, what = "character", sep = "\n",
                        quiet = !verbose)

    data.rows <- c(10, Inf)
    names(data.rows) <- c("skip", "npixels")

    if (length(locale) == 0) {
      locale <- readr::default_locale()
      if (is.null(tz)) {
        tz <- Sys.timezone()
      }
      locale[["tz"]] <- tz
    }
    row01 <- scan(file = file, nlines =  1, skip = data.rows[["skip"]],
                  what = "character", quiet = !verbose)
    if (grepl("\\.", row01[1]) && locale[["decimal_mark"]] != ".") {
      if (verbose) {
        warning("Replacing locale's decimal mark with '.'")
      }
      locale[["decimal_mark"]] <- "."
      locale[["grouping_mark"]] <- ","
    } else if (grepl("\\,", row01[1])) {
      if (verbose) {
        warning("Replacing locale's decimal mark with ','")
      }
      locale[["decimal_mark"]] <- ","
      locale[["grouping_mark"]] <- "."
    }
    if (is.null(tz) && !is.null(time)) {
      tz <- locale$tz
    }

    if (is.null(time)) {
      line01 <- sub("Date: [[:alpha:]]{3} ", "", file_header[1])
      if (is.null(tz)) {
        tz <- sub("^(.{16})([[:upper:]]{3,4})(.{5})$", "\\2", line01)
        if (nchar(tz) == 4) {
          tz <- sub("S", "", tz)
        }
      }
      time <- lubridate::parse_date_time(line01, "mdHMSy", tz = tz, locale = "C")
      if (verbose) {
        message("File '", basename(file), "' with header time: ", time)
      }
    } else if (is.na(time)) {
      time <- as.POSIXct(NA_real_, origin = lubridate::origin)
    } else if (verbose) {
      message("File '", basename(file), "' with user time: ", time)
    }

    old.opts <- options(readr.num_columns = ifelse(verbose, 6, 0))
    z <- readr::read_table2(
      file = file,
      col_names = c("w.length", "oo.cal"),
      skip =  data.rows[["skip"]],
      n_max = data.rows[["npixels"]],
      locale = locale
    )
    options(old.opts)

    old.opts <- options("photobiology.strict.range" = NA)
    z <- photobiology::as.generic_spct(z)
    options(old.opts)

    comment(z) <-
      paste(paste(file_header[1:(data.rows[["skip"]] - 1)], collapse = "\n"),
            "^^^^ end of file header ^^^^",
            paste("Ocean Optics irradiance calibration file '", file, "' imported on ",
                  lubridate::now(tzone = "UTC"), " UTC", sep = ""),
            sep = "\n")

    photobiology::setWhenMeasured(z, time)
    photobiology::setWhereMeasured(z, geocode)
    photobiology::setWhatMeasured(z, label)
    set_oo_ssdata_descriptor(z,
                             descriptor = descriptor,
                             action = ifelse(is.null(descriptor), "overwrite", "merge"))
  }
