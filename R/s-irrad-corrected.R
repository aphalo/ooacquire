#' Convert raw counts data into spectral irradiance
#'
#' @param x A named list of one to hree vectors of file names, with names
#'   "light", "filter", and "dark". Or a raw_mspt object, or a raw_spct object.
#' @param spct.names named character vector of length three.
#' @param correction.method A named list of constants and functions defining the
#'   method to be sued for stray light and dark signal corrections.
#' @param return.cps logical Useful when there is no need to apply a calibration,
#'   such as when computing new calibration multipliers.
#' @param descriptor A named list with a descriptor of the characteristics of
#'   the spectrometer (if serial number does not agree an error is triggered).
#' @param locale	The locale controls defaults that vary from place to place. The
#'   default locale is US-centric (like R), but you can use
#'   \code{\link[readr]{locale}} to create your own locale that controls things
#'   like the default time zone, encoding, decimal mark, big mark, and day/month
#'   names.
#' @param verbose Logical indicating the level of warnings wanted.
#' @param ... Named argument passed to \code{photobiology::cps2irrad} which is
#'   the final calculation step.
#'
#' @note Currently \code{s_irrad_corrected.list} allows processing of files
#'   written by OceanOptics' SpectraSuite software, from protocols with
#'   integration-time bracketing or not, with a dark reference measurement or
#'   not. Three measurements components are recognized: a "light" measurement, a
#'   "filter" measurement using a polycarbonate filter and a dark measurement.
#'   Only "light" is mandatory. Data should be raw counts, either corrected for
#'   detector non-linearity or not. All three spectra should be acquired using
#'   the same instrument settings to achieve good accuracy.
#'
#' @export
#'
s_irrad_corrected <- function(x, ...) UseMethod("s_irrad_corrected")

#' @describeIn s_irrad_corrected Default for generic function.
#' @export
s_irrad_corrected.default <- function(x, ...) {
  stop("'s_irrad_corrected' not implemented for class '", class(x))
}

#' @describeIn s_irrad_corrected Default for generic function.
#' @param time a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} no date variable is added
#' @export
s_irrad_corrected.list <- function(x,
                                   time = NULL,
                                   correction.method,
                                   return.cps = FALSE,
                                   descriptor,
                                   locale = NULL,
                                   verbose = getOption("photobiology.verbose", default = FALSE),
                                   ...) {
  comment.txt <- paste(names(x),
                       sapply(x, paste, collapse = ", "),
                       sep = ": ", collapse = "\n")

  raw.mspct <-
    ooacquire::read_files2mspct(x,
                                time = time,
                                locale = locale,
                                descriptor = descriptor,
                                verbose = verbose)

  corrected.spct <-
    s_irrad_corrected(x = raw.mspct,
                      spct.names = c(light = "light",
                                     filter = "filter",
                                     dark = "dark"),
                      correction.method = correction.method,
                      return.cps = return.cps,
                      verbose = verbose,
                      ...)

  setWhatMeasured(corrected.spct, comment.txt)
  comment(corrected.spct) <- paste("Processed on ", lubridate::today(),
                                   "\nwith 's_irrad_corrected()' from 'ooacquire' ver. ",
                                   utils::packageVersion("ooacquire"),
                                   "\n\nfrom files:\n", comment.txt, sep = "")
  corrected.spct

}

#' @describeIn s_irrad_corrected Default for generic function.
#' @export
s_irrad_corrected.raw_mspct <-
  function(x,
           spct.names = c(light = "light",
                          filter = "filter",
                          dark = "dark"),
           correction.method,
           return.cps = FALSE,
           verbose = getOption("photobiology.verbose", default = FALSE),
           ...) {
    if (length(x[[ spct.names["light"] ]]) == 0) {
      if (verbose) {
        warning("'raw_spct' object for 'light' scans missing")
      }
      if (return.cps) {
        return(cps_spct())
      } else {
        return(source_spct())
      }
    }

    corrected.spct <-
      ooacquire::uvb_corrections(x = x,
                                 spct.names = spct.names,
                                 stray.light.method = correction.method[["stray.light.method"]],
                                 stray.light.wl = correction.method[["stray.light.wl"]],
                                 flt.dark.wl = correction.method[["flt.dark.wl"]],
                                 flt.ref.wl = correction.method[["flt.ref.wl"]],
                                 flt.Tfr = correction.method[["flt.Tfr"]],
                                 inst.dark.pixs = correction.method[["inst.dark.pixs"]],
                                 worker.fun = correction.method[["worker.fun"]],
                                 trim = correction.method[["trim"]],
                                 verbose = verbose)

    if (return.cps) {
      corrected.spct
    } else {
      photobiology::cps2irrad(corrected.spct, ...)
    }
  }

#' @describeIn s_irrad_corrected Default for generic function.
#' @export
s_irrad_corrected.raw_spct <- function(x,
                                       correction.method,
                                       return.cps = FALSE,
                                       verbose = getOption("photobiology.verbose", default = FALSE),
                                       ...) {
  raw.mspct <- raw_mspct(list(light = x))
  s_irrad_corrected(x = raw.mspct,
                    correction.method = correction.method,
                    return.cps = return.cps,
                    verbose = verbose,
                    ...)
}

#' Select which instrument descriptor to use
#'
#' Select from a list of instrument descriptors which one to use based on
#' date of measurement.
#'
#' @param date Any object that \code{anytime::anydate()} will decode as a date
#'   or convert to a date. Used to select a descriptor containing calibration
#'   data valid for a given day.
#' @param descriptors A named list of descriptors of the characteristics of
#'   the spectrometer including calibration data.
#' @param verbose Logical indicating the level of warnings wanted.
#' @param ... Currently ignored.
#'
#' @note The default argument for \code{verbose} is for this function
#'   \code{TRUE} unless the argument to \code{date} is a date object, as
#'   conversion of other objects to a date may fail.
#'
#'   If a character string is passed as argument to \code{date}, it must be
#'   in a format suitable for \code{anytime::anydate()}. One needs to be
#'   careful with months and days of the month when supplying them as numbers,
#'   so using months names or their abbreviations may be safer.
#'
#' @export
#'
which_descriptor <- function(date = lubridate::today(),
                             descriptors = ooacquire::MAYP11278_descriptors,
                             verbose = NULL,
                             ...) {
  if (is.null(verbose)) {
    verbose <- !lubridate::is.instant(date)
  }

  if (!lubridate::is.Date(date)) {
    date <- anytime::anydate(date)
  }

  for (d in rev(names(descriptors))) {
    if (descriptors[[d]][["inst.calib"]][["start.date"]] < date &
        descriptors[[d]][["inst.calib"]][["end.date"]] > date) {
      if (verbose) {
        message("Descriptor ", d, " selected for ", date)
      }
      return(descriptors[[d]])
    }
  }

  if (verbose) {
    warning("No valid descriptor found for ", date)
  } else {
    message("No valid descriptor found for ", date)
  }
  return(list())
}