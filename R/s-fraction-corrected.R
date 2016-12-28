#' Convert raw counts data into a spectral fraction
#'
#' Convert raw counts data into spectral transmittance or spectral reflectance.
#'
#' @param x A named list of one to three vectors of file names, with names
#'   "sample", "ref", and "dark".
#' @param reference.value numeric or filter_spct or reflector_spct object, with the
#'   fractional transmittance or reflectance of the reference.
#' @param type character One of "internal" or "total".
#' @param method A named list of constants and functions defining the
#'   method to be sued for stray light and dark signal corrections.
#' @param dyn.range numeric Effective dynamic range of the spectrometer.
#' @param qty.out character, one of "Tfr", "Rfr".
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
#' @note Currently \code{s_fraction_corrected.list} allows processing of files
#'   written by OceanOptics' SpectraSuite software, from protocols with
#'   integration-time bracketing or not, with a dark reference measurement or
#'   not. Four measurements components are recognized: a "sample" measurement,
#'   a "referenece" measurement using a clear or white, a "filter" measurement
#'   with a UV-blocking filter in the light pass, and a "dark" measurement.
#'   Only "sample" and "reference" are mandatory. Data should be raw counts,
#'   either corrected for detector non-linearity or not. All three spectra
#'   should be acquired using the same instrument settings to achieve good
#'   accuracy.
#'
#' @export
#'
s_fraction_corrected <- function(x, ...) UseMethod("s_fraction_corrected")

#' @describeIn s_fraction_corrected Default for generic function.
#' @export
s_fraction_corrected.default <- function(x, ...) {
  stop("'s_fraction_corrected' not implemented for class '", class(x))
}

#' @describeIn s_fraction_corrected Default for generic function.
#' @param time a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} no date variable is added
#' @export
s_fraction_corrected.list <- function(x,
                                      reference.value = 1,
                                      type = "internal",
                                      time = NULL,
                                      method,
                                      qty.out = "Tfr",
                                      descriptor = NULL,
                                      dyn.range = NULL,
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

  fraction.spct <- s_fraction_corrected(x = raw.mspct,
                                        reference.value = reference.value,
                                        type = type,
                                        method = method,
                                        dyn.range = dyn.range,
                                        qty.out = qty.out,
                                        verbose = verbose,
                                        ...)

  setWhatMeasured(fraction.spct, comment.txt)
  comment(fraction.spct) <- paste("Processed on ", lubridate::today(),
                                   "\nwith 's_fraction_corrected()' from 'ooacquire' ver. ",
                                   utils::packageVersion("ooacquire"),
                                   "\n\nfrom files:\n", comment.txt, sep = "")
  fraction.spct

}

#' @describeIn s_fraction_corrected Default for generic function.
#'
#' @export
s_fraction_corrected.raw_mspct <- function(x,
                                           reference.value = 1,
                                           type = "internal",
                                           method,
                                           dyn.range = NULL,
                                           qty.out = "Tfr",
                                           verbose = getOption("photobiology.verbose", default = FALSE),
                                           ...) {

  if (length(x[["sample"]]) == 0 || length(x[["reference"]]) == 0) {
    if (verbose) {
      warning("Raw spectra for 'sample' and/or 'reference' missing")
    }
    if (qty.out == "Rfr") {
      z <- reflector_spct()
      return(z)
    } else {
      z <- filter_spct()
      return(z)
    }
  }

  corrected_smp.spct <-
    ooacquire::uvb_corrections(x = x[["sample"]],
                               flt = x[["filter"]],
                               dark = x[["dark"]],
                               stray.light.method = method[["stray.light.method"]],
                               stray.light.wl = method[["stray.light.wl"]],
                               flt.dark.wl = method[["flt.dark.wl"]],
                               flt.ref.wl = method[["flt.ref.wl"]],
                               worker.fun = method[["worker.fun"]],
                               trim = method[["trim"]],
                               verbose = verbose)

  corrected_ref.spct <-
    ooacquire::uvb_corrections(x = x[["reference"]],
                               flt = x[["filter"]],
                               dark = x[["dark"]],
                               stray.light.method = method[["stray.light.method"]],
                               stray.light.wl = method[["stray.light.wl"]],
                               flt.dark.wl = method[["flt.dark.wl"]],
                               flt.ref.wl = method[["flt.ref.wl"]],
                               worker.fun = method[["worker.fun"]],
                               trim = method[["trim"]],
                               verbose = verbose)

  if (qty.out == "Rfr") {
    z <- photobiology::cps2Rfr(corrected_smp.spct,
                               corrected_ref.spct,
                               dyn.range = dyn.range) / reference.value
    setRfrType(z, type)
  } else if (qty.out == "Tfr") {
    z <- photobiology::cps2Tfr(corrected_smp.spct,
                               corrected_ref.spct,
                               dyn.range = dyn.range) / reference.value
    setRfrType(z, type)
  }
}
