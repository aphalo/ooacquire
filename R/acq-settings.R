#' Settings for spectral measurement
#'
#' Validate parameters for spectral measurements and return a list of values
#' usable as input for functions \code{retune_acq_settings()}, \code{acq_sptc()},
#' and \code{acq_mspct()}.
#'
#' @param oo_descriptor list as returned by function \code{get_oo_descriptor}
#' @param integ.time numeric vaue in seconds
#' @param num.scans integer
#' @param min.integ.time numeric vaue in seconds
#' @param max.integ.time numeric vaue in seconds
#' @param tot.time.range numeric vector of length two with values in seconds
#' @param HDR.mult a numeric vector with integ.time multipliers to be used for
#'   "bracketing".
#' @param pix.selector a logical or numeric vector used as subscript to select
#'   pixels
#' @param corr.elect.dark,corr.sensor.nl  integer 0L (FALSE) or 1L (TRUE)
#' @param boxcar.width integer number of pixels to average
#' @param verbose a logical to enable or disable warnings
#'
#' @note \code{pixel.selector} can be used for two different purposes: to
#'   ignore bad pixels and to restrict integration-time tuning to the response
#'   from a range of pixels. The interpretation of \code{tot.time.range} is
#'   as follows: first value is minimum time, second value is maximum time.
#'   If both values are the same, then an exact measurement time is computed.
#'
#' @details \code{acq_settings()} is used to manually save a complete set of
#'   instrument settings in a way that they can be reused as needed for repated
#'   acquisition of spectra. \code{tune_acq_settings()} and
#'   \code{tune_acq_settings()} differ only in their formal parameters and
#'   optimize settings to the current spectral irradiance.
#'
#' @note Ocean Optics spectrometers can be queried for the maximum and minimum
#'   supported integration times. This function modifies the user supplied
#'   values if outside these bounds. The defaults of -Inf and Inf force the use
#'   of the whole valid range of integration time supported by the connected
#'   intrument. \code{pixel.selector} can be used for two different purposes: to
#'   ignore bad pixels and to restrict integration-time tuning to the response
#'   from a range of pixels.
#'
#' @export
#' @return a list.
#'
acq_settings <- function(oo_descriptor,
                         integ.time, # seconds
                         num.scans = 10L,
                         min.integ.time = -Inf, # seconds
                         max.integ.time = Inf, # seconds
                         tot.time.range = c(0, Inf), # seconds
                         HDR.mult = c(short = 1, long = 10),
                         pix.selector = TRUE,
                         corr.elect.dark = 0L,
                         corr.sensor.nl = 0L,
                         boxcar.width = 0L,
                         verbose = TRUE) {
  # Check length consistency
  stopifnot(length(integ.time) == length(num.scans))
  # convert times to microseconds
  integ.time <- integ.time * 1e6
  min.integ.time <- min.integ.time * 1e6
  max.integ.time <- max.integ.time * 1e6
  tot.time.range <- tot.time.range * 1e6
  # We check bounds and set the "fixed settings" (non-tunable)
  min.integ.time <- max(min.integ.time, oo_descriptor$min.integ.time)
  max.integ.time <- min(max.integ.time, oo_descriptor$max.integ.time)
  # Check bounds for integ.time value supplied by user
  integ.time <- ifelse(integ.time > max.integ.time, max.integ.time, integ.time)
  integ.time <- ifelse(integ.time < min.integ.time, min.integ.time, integ.time)
  # Check num.scans
  num.scans <- round(num.scans)
  stopifnot(all(num.scans >= 1))
  # make sure HDR multipliers are sorted
  HDR.mult <- sort(HDR.mult)
  if (HDR.mult[1] > 1) {
    warning("Using a smallest 'HDR.mult' value that is > 1 will cause clipping")
  }
  if (is.logical(pix.selector) && length(pix.selector) != 1) {
    stopifnot(length(pix.selector) == length(oo_descriptor$wavelengths))
  }
  # return a list
  list(
    # settings
    pix.selector = pix.selector,
    HDR.mult = HDR.mult,
    integ.time = integ.time,
    max.integ.time = max.integ.time,
    min.integ.time = min.integ.time,
    num.scans = num.scans,
    tot.time.range = tot.time.range,
    corr.elect.dark = corr.elect.dark,
    corr.sensor.nl = corr.sensor.nl,
    boxcar.width = boxcar.width,
    # diagnosis
    tot.time = integ.time * num.scans,
    rel.signal = NA
  )
}

#' Tune settings for measurement
#'
#' Find optimal settings for spectral measurements under a given measurement
#' protocol.
#'
#' @rdname acq_settings
#' @return a list.
#'
#' @param acq_settings list as returned by a previous call to \code{acq_settings()},
#'   or \code{tune_acq_settings()}.
#'
#' @export
#'
tune_acq_settings <- function(oo_descriptor,
                              acq_settings,
                              verbose = TRUE) {
  x <- acq_settings
  nl.fun <- oo_descriptor$inst.calib$nl.fun
  # correction for electrical dark (in instrument using ocluded pixels in array)
  rOmniDriver::set_correct_for_electrical_dark(oo_descriptor$w, x$corr.elect.dark,
                                               oo_descriptor$sr.index,
                                               oo_descriptor$ch.index)
  # correction for sensor non-linearuty (in instrument)
  rOmniDriver::set_correct_for_detector_nonlinearity(oo_descriptor$w,
                                                     x$corr.sensor.nl,
                                                     oo_descriptor$sr.index,
                                                     oo_descriptor$ch.index)
  # moving window smoothing
  rOmniDriver::set_boxcar_width(oo_descriptor$w,
                                x$boxcar.width,
                                oo_descriptor$sr.index,
                                oo_descriptor$ch.index)
  # to speed up tunning we set number of scans to one
  rOmniDriver::set_scans_to_avg(oo_descriptor$w, 1L,
                                oo_descriptor$sr.index,
                                oo_descriptor$ch.index)
  # to more easily reach the target we linearize the counts before interpolation
  nl.fun <- oo_descriptor$inst.calib$nl.fun
  # optimize parameters
  integ.time <- x$integ.time[1]
  target.min.counts <- nl.fun(0.8 * oo_descriptor$max.counts)

  i <- 0L
  repeat {
    if (verbose) {
      message("Integration time (ms): ", format(integ.time  * 1e-3))
    }
    rOmniDriver::set_integration_time(oo_descriptor$w,
                                      integ.time,
                                      oo_descriptor$sr.index,
                                      oo_descriptor$ch.index)
    raw.counts <- rOmniDriver::get_spectrum(oo_descriptor$w,
                                            oo_descriptor$sr.index,
                                            oo_descriptor$ch.index)
    max.counts <- nl.fun(max(raw.counts[x$pix.selector]))
    while (rOmniDriver::is_saturated(oo_descriptor$w,
                                     oo_descriptor$sr.index,
                                     oo_descriptor$ch.index))
    {
      integ.time <- integ.time / 3
      if (integ.time < x$min.integ.time) {
        break()
      }
      if (verbose) message("Clipping! Trying (ms): ", format(integ.time  * 1e-3))
      rOmniDriver::set_integration_time(oo_descriptor$w,
                                        integ.time,
                                        oo_descriptor$sr.index,
                                        oo_descriptor$ch.index)
      raw.counts <- rOmniDriver::get_spectrum(oo_descriptor$w,
                                              oo_descriptor$sr.index,
                                              oo_descriptor$ch.index)
      max.counts <- nl.fun(max(raw.counts[x$pix.selector]))
    }
    if (verbose) message(paste("max.counts[", i, "]: ", format(max.counts)))
    if (max.counts < target.min.counts && integ.time < x$max.integ.time) {
      if (verbose) message("max count <", round(target.min.counts))
      if (max.counts < 0.9 * target.min.counts) {
        integ.time <- round(integ.time * target.min.counts / max.counts * 1.1, 0)
      } else {
        integ.time <- round(integ.time * 1.2, 0)
      }
    }

    if (integ.time > x$max.integ.time) {
      if (verbose) {
        warning("Light level is too low for optimal performance! Using (ms): ",
                format(integ.time * 1e-3))
      }
      break()
    }

    if (integ.time < x$min.integ.time) {
      if (verbose) {
        warning("Clipping cannot be avoided! Using (ms): ",
                format(integ.time * 1e-3))
      }
      break()
    }

    if (max.counts >= target.min.counts) {
      break()
    } else {
      i <- i + 1
    }
  }
  integ.time <- x$HDR.mult * integ.time # vectorized!
  integ.time <- ifelse(integ.time > x$max.integ.time, x$max.integ.time, integ.time)
  integ.time <- ifelse(integ.time < x$min.integ.time, x$min.integ.time, integ.time)
  num.scans <- ifelse(integ.time < x$tot.time.range[1],
                      trunc(x$tot.time.range[1] / integ.time) + 1,
                      1)
  if (x$tot.time.range[2] - x$tot.time.range[1] < x$min.integ.time) {
    integ.time <- x$tot.time.range[1] / num.scans
  } else if (integ.time[1] > x$tot.time.range[2]) {
    integ.time <- x$tot.time.range[2]
  }
  acq_settings$integ.time <- integ.time
  acq_settings$num.scans <- num.scans
  #diagnosis
  acq_settings$tot.time <- integ.time * num.scans
  acq_settings$rel.signal = max.counts / oo_descriptor$max.counts

  if (verbose) {
    message("Relative saturation: ",
            format(acq_settings$rel.signal, width = 8), " ")
    message("Integration times (ms): ",
            format(acq_settings$integ.time * 1e-3, nsmall = 0, width = 8), " ")
    message("Numbers of scans:       ",
            format(acq_settings$num.scans, width = 8), " ")
    message("Total time (s):         ",
            format(acq_settings$tot.time * 1e-6,
                   digits = 3, width = 8), " ")
  }
  acq_settings
}
