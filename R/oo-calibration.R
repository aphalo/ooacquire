#' Convert an OO calibration
#'
#' Convert irradiance calibration values as supplied by Ocean Optics into
#' multipliers expressed in the units and format expected by the functions in
#' this package.
#'
#' @param x generic_spct object with variables \code{w.length} and
#'   \code{oo.cal}.
#' @param area numeric Area of the cosine diffuser (mm2).
#' @param diff.type character Value giving type of diffuser as in OO's
#'   documents, case insensitive. Ignored unless \code{area = NULL}.
#' @param verbose Logical indicating the level of warnings wanted.
#'
#' @details Ocean Optics expresses spectroradiometer calibrations in a format
#'   that is independent of the diffuser user. Such a calibration cannot be
#'   used as is, it needs to be scaled based on the \emph{effective} light
#'   collecting surface area of the diffuser used. It is crucial that the
#'   correct \code{area} or \code{diff.type} is entered when calling this
#'   function. (This is an unusual approach as there is variation among
#'   individual diffusers of a given type. Calibrations are also affected
#'   by the optical fibre and even the alignment of the fibre at the
#'   spectrometer attachment bulkhead.)
#'
#' @section Cosine diffusers:
#'   Ocean Optics spectrometers can be used with cosine diffusers supplied
#'   by Ocean Optics or by third parties. Some diffusers are recognised by
#'   name (argument passed to \code{diff.type}) while others can be described by
#'   their light collecting area expressed in \eqn{mm^2} (argument passed to
#'   \code{area}). The known cosine diffuser names are tabulated below.
#'
#'   \tabular{ll}{
#'   \strong{Name}  \tab \strong{Supplier} \cr
#'   "CC-3-DA"      \tab Ocean Optics\cr
#'   "CC-3"         \tab Ocean Optics\cr
#'   "CC-3-UV-S"    \tab Ocean Optics\cr
#'   "CC-3-UV-T"    \tab Ocean Optics\cr
#'   "UV-J1002-SMA" \tab CMS Schreder\cr
#'   "D7-SMA"       \tab Bentham Instruments\cr
#'   "D7-H-SMA"     \tab Bentham Instruments\cr
#'   }
#'
#' @return A \code{calibration_spct} object of the same number of rows as
#'   \code{x} containing wavelengths in variable \code{w.length} and the
#'   re-scaled calibration factors in variable \code{irrad.mult}. In practice
#'   the \code{calibration_spct} object contains one row for each pixel in
#'   the detector array of the spectrometer.
#'
#' @export
#'
#' @seealso \code{\link{read_oo_caldata}}
#'
oo_calib2irrad_mult <-
  function(x,
           area = NULL,
           diff.type = NULL,
           verbose = getOption("photobiology.verbose", default = FALSE)) {
    stopifnot(is.generic_spct(x))
    if (is.null(area)) {
      if (is.null(diff.type)) {
        stop("Missing argument: one of 'area' or 'diff.type' must be supplied.")
      }
      area <- # diameters in metres
        switch(toupper(diff.type),
               "CC-3-DA" = pi * (7.14e-3 / 2)^2, # OO
               "CC-3" = pi * (3.9e-3 / 2)^2, # OO
               "CC-3-UV-S" = pi * (3.9e-3 / 2)^2, # OO
               "CC-3-UV-T" = pi * (3.9e-3 / 2)^2, # OO
               "UV-J1002-SMA" = pi * (11e-3 / 2)^2, # Schreder
               "D7-SMA" = pi * (10e-3 / 2)^2, # Bentham
               "D7-H-SMA" = pi * (10e-3 / 2)^2, # Bentham
               {warning("diff.type argument '", diff.type, "' not recognized.")
               NA_real_})
    }
    stopifnot(is.numeric(area))
    wl.steps <- diff(x[["w.length"]])
    wl.steps <- c(wl.steps[1], wl.steps)
    wl.steps <- caTools::runmean(wl.steps, k = 2)
    x[["irrad.mult"]] <- x[["oo.cal"]] / (area * wl.steps) * 1e-6 # uW -> W
    if (!verbose) {
      # delete values read from file
      x[["oo.cal"]] <- NULL
    }
    # attributes retained from argument
    setCalibrationSpct(x)
  }

#' Read OO irradiance calibration.
#'
#' Reads a calibration data file as supplied by Ocean Optics. Wavelength and
#' calibration values are stored as data and the metadata parsed from the header
#' or supplied as arguments as attributes of the same object, including the time
#' and date of the calibration. The whole file header is in addition stored as a
#' comment.
#'
#' @param file character string Path or file to read.
#' @param time a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} the \code{when.measured} attribute is set to
#'   \code{NA}.
#' @param geocode A one row data frame with numeric columns \code{lon} and
#'   \code{lat}, and optionally a character column \code{address}.
#' @param label character string, but if \code{NULL} the value of \code{file} is
#'   used, and if \code{NA} the "what.measured" attribute is set to \code{NA}.
#' @param descriptor list A list describing the instrument, used to set
#'   attribute \code{instr.descriptor}.
#' @param tz character Time zone is by default read from the file.
#' @param locale	The locale controls defaults that vary from place to place. The
#'   default locale is US-centric (like R), but you can use
#'   \code{\link[readr]{locale}} to create your own locale that controls things
#'   like the default time zone, encoding, decimal mark, big mark, and day/month
#'   names.
#' @param verbose Logical indicating the level of warnings wanted.
#'
#' @details The format of the data used by Ocean Optics seems to have remained
#'   consistent in time and with spectrometer model, but not the header. In
#'   particular, the format of the date and whether time is included or not
#'   varies. The function is able to decode three different date formats that I
#'   have come across. If decoding fails with a message indicating that parsing
#'   has failed, the problem can be overcome by passing the date as an argument
#'   to parameter \code{time} when calling the function, as in this case no
#'   decoding of the date is attempted. The easiest approach is to use function
#'   \code{\link[lubridate]{dmy}} or similar from package 'lubridate'.
#'   \emph{When no time zone information is included in the file header, the
#'   local time zone is used, which in many cases will shift the date by one
#'   day. If you know where the calibration was done, the correct TZ can be
#'   passed as an argument. Anyway as calibrations are usually considered valid
#'   for one or two years, this shift is only rarely important,}
#'
#' @return A generic_spct object, with columns \code{w.length} and
#'   \code{oo.cal}.
#'
#' @seealso \code{\link{oo_calib2irrad_mult}}
#'
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

    line01 <- scan(file = file, nlines = 1, skip = 0,
                   what = "character", quiet = !verbose)
    if (line01[1] != "Date") {
      warning("Input file may not be a calibration file as expected: skipping")
      return(NA)
    }

    line02 <- scan(file = file, nlines = 1, skip = 1,
                   what = "character", quiet = !verbose)
    spectrometer.sn <- line02[2]

    what.measured <- list(file.name = basename(file),
                          sn = spectrometer.sn,
                          user.label = ifelse(is.null(label), "", label)
                          )

    data.rows <- c(9, Inf)
    names(data.rows) <- c("skip", "npixels")

    file_header <- scan(file = file, nlines = data.rows[["skip"]],
                        skip = 0, what = "character", sep = "\n",
                        quiet = !verbose)

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
      # get time from file header if no argument was passed in call
      # I have come across three different date formats in calibration files
      # requiring two different approaches to parsing
      if (grepl("[0,1][0-9][-/][0-3][0-9][-/][0-2][0-9]$", line01[2])) {
        # dates like 07-05-25 or 07/05/25 for day/month/year
        date.string <- line01[2]
        time <- lubridate::dmy(date.string, tz = tz)
      } else {
        # dates like "Fri Jun 03 13:17:08 EDT 2016"
        line01 <- sub("Date: [[:alpha:]]{3} ", "", file_header[1])
        if (is.null(tz)) {
          tz <- sub("^(.{16})([[:upper:]]{3,4})(.{5})$", "\\2", line01)
          if (nchar(tz) == 4) {
            tz <- sub("S", "", tz)
          }
        }
        time <- lubridate::parse_date_time(line01, "mdHMSy", tz = tz, locale = "C")
      }
      if (verbose) {
        message("File '", basename(file), "' with header time: ", time)
      }
    } else if (is.na(time)) {
      time <- as.POSIXct(NA_real_, origin = lubridate::origin)
    } else if (verbose) {
      message("File '", basename(file), "' with user time: ", time)
    }

    old.opts <- options(readr.num_columns = ifelse(verbose, 6, 0))
    z <- readr::read_table(
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
            paste("Ocean Optics irradiance calibration file '", file,
                  "' imported on ",
                  lubridate::now(tzone = "UTC"), " UTC", sep = ""),
            sep = "\n")

    photobiology::setWhenMeasured(z, time)
    photobiology::setWhereMeasured(z, geocode)
    photobiology::setWhatMeasured(z, what.measured)
    set_oo_ssdata_descriptor(z,
                             descriptor = descriptor,
                             action = ifelse(is.null(descriptor),
                                             "overwrite", "merge"))
  }
