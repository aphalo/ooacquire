#' Make default method from descriptor
#'
#' A function that builds a default method from an instrument descriptor.
#' Useful when the spectrometers has not been characterized as needed for
#' the more sofisticated methods. Can use stray light correction
#' but not slit function correction. Stray light correction is valid
#' only if it was also used during irradiance callibration. Suitablity
#' of wavelengths and method depends on the instrument configuration so
#' they are set to \code{NA} as default..
#'
#' @param descriptor list, as returned by \code{get_oo_descriptr()}
#' @param stray.light.method character Name of method.
#' @param stray.light.wl,flt.dark.wl,flt.ref.wl numeric vector with wavelengths (nm).
#' @param flt.Tfr numeric Transmittance of filter as a fraction (0...1).
#'
#' @note Defaults for indexes are for the first channel of the first
#'   spectrometer currently connected.
#'
#' @details The currently recognized methods for stray-light correction are
#' \code{"none"}, \code{"original"}, \code{"sun"}, and \code{"simple"}. With
#' the default method \code{"none"}, the values of the remaining parameters
#' are ignored.
#'
#' @export
#' @return a list
#'
new_correction_method <- function(descriptor,
                                  stray.light.method = "none",
                                  stray.light.wl = c(NA_real_, NA_real_),
                                  flt.dark.wl =  c(NA_real_, NA_real_),
                                  flt.ref.wl =  c(NA_real_, NA_real_),
                                  flt.Tfr = NA_real_) {
  ## Configuration is not known
  # We find the dark pixels from the descriptor
  inst.dark.pixs <- 1:descriptor[["num.dark.pixs"]]

  # We need to validate the inputs (dark pixels are not usable)
  wl.range <- range(descriptor[["wavelengths"]][-inst.dark.pixs])
  if (is.null(wl.range) || anyNA(wl.range)) {
    stop("No wavelength calibration available!")
  }
  wl.method <- range(stray.light.wl, flt.dark.wl, flt.dark.wl)
  if (anyNA(c(wl.range, wl.method)) ||
      wl.range[1] > wl.method[1] ||
      wl.range[2] < wl.method[2]) {
    wl.method[1] <- ifelse(wl.range[1] > wl.method[1], wl.range[1], wl.method[1])
    wl.method[2] <- ifelse(wl.range[2] < wl.method[2], wl.range[2], wl.method[2])
    stray.light.method = "none"
    warning("Invalid wavelengths for instrument: stray-light correction disabled!")
  }

  if (!stray.light.method %in% c("original", "sun", "simple")) {
    stray.light.wl <- flt.dark.wl <- flt.ref.wl <- NA_real_
    if (stray.light.method != "none") {
      stray.light.method <- "none"
      warning("Unknown stray-light method: stray-light correction disabled!")
    }
  }

  method <- list(
    spectrometer.sn = descriptor$spectrometer.sn,
    stray.light.method = stray.light.method,
    stray.light.wl = stray.light.wl,
    flt.dark.wl = flt.dark.wl,        # used for $N$2 in Lasse's calc worksheet
    flt.ref.wl = flt.ref.wl,         # used for $N$3 in Lasse's calc worksheet
    flt.Tfr = flt.Tfr,
    inst.dark.pixs = inst.dark.pixs,
    tail.coeffs = NA_real_,
    worker.fun = NA_character_)

  method[["trim"]] <-
    switch(stray.light.method,
           original = 0,
           sun = 0.05,
           simple = 0.05,
           none = 0
    )

}

#' Check consistency of serial number
#'
#' This method checks that the sn of the instrument used to acquire the raw
#' counts matches that in the definition of a correction method.
#'
#' @param x a generic_mspct object, in normal use a raw_mspct object.
#' @param correction.method a list describing the correction method. Must
#'   incluse a member named \code{"spectrometer.sn"}.
#' @param missmatch.action a function, in practice one of \code{stop},
#'   \code{warning}, \code{message} or \code{NULL}.
#'
#' @return A logical vector of length one, with \code{FALSE} indicating a
#'   missmatch.
#'
#' @export
#'
check_sn_match <- function(x,
                           correction.method,
                           missmatch.action = stop) {
  equal.sn <- TRUE
  for (s in x) {
    if (getInstrDesc(s)[["spectrometer.sn"]] != correction.method[["spectrometer.sn"]]) {
      equal.sn <- FALSE
    }
  }
  if (!all(equal.sn)) {
    if (!is.null(missmatch.action)) {
      missmatch.action("Serial number mismatch with 'correction.method' definition: ",
                       names(x[!equal.sn]))
    }
  }

  all(equal.sn)
}

