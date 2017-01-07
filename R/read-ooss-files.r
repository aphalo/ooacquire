#' Read File Saved by Ocean Optics' SpectraSuite.
#'
#' Reads and parses the header of a processed data file as output by
#' SpectraSuite to extract the whole header remark field. The time field is
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
#' @return A raw_spct object.
#' @export
#'
read_oo_ssdata <- function(file,
                           time = NULL,
                           geocode = NULL,
                           label = NULL,
                           descriptor = NULL,
                           tz = NULL,
                           locale = NULL,
                           verbose = getOption("photobiology.verbose", default = FALSE)) {

  label <- paste("File: ", basename(file), label, sep = "")

  line01 <- scan(file = file, nlines =  1, skip = 0,
                 what = "character", quiet = !verbose)
  if (line01[1] != "SpectraSuite") {
    warning("Input file was not created by SpectrSuite as expected: skipping")
    return(NA)
  }
  file_header <- scan(file = file, nlines = 20,
                      skip = 0, what = "character", sep = "\n", quiet = !verbose)

  data.rows <- oofile_data_rows(file_header)

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
    line03 <- sub("Date: [[:alpha:]]{3} ", "", file_header[3])
    if (is.null(tz)) {
      tz <- sub("^(.{16})([[:upper:]]{3,4})(.{5})$", "\\2", line03)
      if (nchar(tz) == 4) {
        tz <- sub("S", "", tz)
      }
    }
    time <- lubridate::parse_date_time(line03, "mdHMSy", tz = tz, locale = "C")
    if (verbose) {
      message("File '", basename(file), "' with header time: ", time)
    }
  } else if (is.na(time)) {
    time <- as.POSIXct(NA_real_, origin = lubridate::origin)
  } else if (verbose) {
    message("File '", basename(file), "' with user time: ", time)
  }

  data.rows <- oofile_data_rows(file_header)

  old.opts <- options(readr.num_columns = ifelse(verbose, 6, 0))
  z <- readr::read_tsv(
    file = file,
    col_names = c("w.length", "counts"),
    skip =  data.rows[["skip"]],
    n_max = data.rows[["npixels"]],
    locale = locale
  )
  options(old.opts)

  old.opts <- options("photobiology.strict.range" = NA)
  z <- photobiology::as.raw_spct(z)
  options(old.opts)

  comment(z) <-
    paste(paste(file_header[1:(data.rows[["skip"]] - 1)], collapse = "\n"),
          "^^^^ end of file header ^^^^",
          paste("Ocean Optics Spectra Suite raw counts file '", file, "' imported on ",
                lubridate::now(tzone = "UTC"), " UTC", sep = ""),
          sep = "\n")

  photobiology::setWhenMeasured(z, time)
  photobiology::setWhereMeasured(z, geocode)
  photobiology::setWhatMeasured(z, label)
  z <- set_oo_ssdata_descriptor(z,
                                descriptor = descriptor,
                                action = ifelse(is.null(descriptor), "overwrite", "merge"))
  set_oo_ssdata_settings(z)
}
