#' Settings for spectral measurement
#'
#' Validate parameters for spectral measurements and return a list of values
#' usable as input for functions \code{retune_acq_settings()}, \code{acq_sptc()},
#' and \code{acq_mspct()}.
#'
#' @param oo_descriptor list as returned by function \code{get_oo_descriptor}
#' @param integ.time numeric vaue in seconds
#' @param num.scans integer
#' @param start.integ.time numeric vaue in seconds
#' @param min.integ.time numeric vaue in seconds
#' @param max.integ.time numeric vaue in seconds
#' @param min.tot.time numeric vaue in seconds
#' @param HDR.mult a numeric vector with integ.time multipliers to be used for
#'   "bracketing".
#' @param pix.selector a logical or numeric vector used as subscript to select
#'   pixels
#' @param verbose a logical to enable or disable warnings
#'
#' @note \code{pixel.selector} can be used for two different purposes: to
#'   ignore bad pixels and to restrict integration-time tuning to the response
#'   from a range of pixels.
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
                         start.integ.time = 50e-3, # seconds
                         min.integ.time = -Inf, # seconds
                         max.integ.time = Inf, # seconds
                         min.tot.time = 2, # seconds
                         HDR.mult = c(short = 1, long = 10),
                         pix.selector = TRUE,
                         verbose = TRUE) {
  # Check length consistency
  stopifnot(length(integ.time) == length(num.scans))
  # convert times to microseconds
  integ.time <- integ.time * 1e6
  start.integ.time <- start.integ.time * 1e6
  min.integ.time   <- min.integ.time   * 1e6
  max.integ.time   <- max.integ.time   * 1e6
  min.tot.time   <- min.tot.time   * 1e6
  # We check bounds and set the "fixed settings" (non-tunable)
  min.integ.time <- max(min.integ.time, oo_descriptor$min.integ.time)
  max.integ.time <- min(max.integ.time, oo_descriptor$max.integ.time)
  # Check bounds for integ.time value supplied by user
  integ.time <- ifelse(integ.time > max.integ.time, max.integ.time, integ.time)
  integ.time <- ifelse(integ.time < min.integ.time, min.integ.time, integ.time)
  # Check num.scans
  num.scans <- round(num.scans)
  stopifnot(all(num.scans >= 1))
  # Calculate total.time
  total.time <- integ.time * num.scans
  list(
    pix.selector = pix.selector,
    HDR.mult = HDR.mult,
    integ.time = integ.time,
    max.integ.time = max.integ.time,
    min.integ.time = min.integ.time,
    num.scans = num.scans,
    #diagnosis
    total.time = total.time,
    rel.signal = NA
  )
}

#' Tune settings for measurement
#'
#' Find parameters for spectral measurements with a given measurements
#' protocol.
#'
#' @rdname acq_settings
#' @export
#' @return a list.
#'
tune_acq_settings <- function(oo_descriptor,
                              start.integ.time = 50e-3, # seconds
                              min.integ.time = -Inf, # seconds
                              max.integ.time = Inf, # seconds
                              min.tot.time = 2, # seconds
                              HDR.mult = c(short = 1, long = 10),
                              pix.selector = TRUE,
                              verbose = TRUE) {
  # convert times to microseconds
  start.integ.time <- start.integ.time * 1e6
  min.integ.time   <- min.integ.time   * 1e6
  max.integ.time   <- max.integ.time   * 1e6
  min.tot.time   <- min.tot.time   * 1e6
  # make sure HDR multipliers are sorted
  mult.reorder <- order(HDR.mult)
  HDR.mult <- HDR.mult[mult.reorder]
  NR.flag <- NR.flag[mult.reorder]
  if (HDR.mult[1] > 1) {
    warning("Using a smallest 'HDR.mult' value that is > 1 will cause clipping")
  }
  # We check bounds and set the "fixed settings" (non-tunable)
  min.integ.time <- max(min.integ.time, oo_descriptor$min.integ.time)
  max.integ.time <- min(max.integ.time, oo_descriptor$max.integ.time)
  if (is.logical(pix.selector)) {
    stopifnot(length(pix.selector) == length(oo_descriptor$wavelengths))
  }
  pix.selector <- pix.selector
  rOmniDriver::set_correct_for_electrical_dark(oo_descriptor$w, 0L,
                                               oo_descriptor$sr.index,
                                               oo_descriptor$ch.index)
  rOmniDriver::set_correct_for_detector_nonlinearity(oo_descriptor$w, 0L,
                                                     oo_descriptor$sr.index,
                                                     oo_descriptor$ch.index)
  rOmniDriver::set_boxcar_width(oo_descriptor$w, 0L,
                                oo_descriptor$sr.index,
                                oo_descriptor$ch.index)
  rOmniDriver::set_scans_to_avg(oo_descriptor$w, 1L,
                                oo_descriptor$sr.index,
                                oo_descriptor$ch.index)

  # optimize parameters
  integ.time <- start.integ.time
  target.min.counts <- 0.8 * oo_descriptor$max.counts

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
    max.counts <- max(raw.counts[pix.selector])
    while (rOmniDriver::is_saturated(oo_descriptor$w,
                                     oo_descriptor$sr.index,
                                     oo_descriptor$ch.index))
    {
      integ.time <- integ.time * 0.6666667
      if (integ.time < min.integ.time) {
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
      max.counts <- max(raw.counts[pix.selector])
    }
    if (verbose) message(paste("max.counts[", i, "]: ", format(max.counts)))
    if (max.counts < target.min.counts && integ.time < max.integ.time) {
      if (verbose) message("max count < 50000")
      if (max.counts < 0.9 * target.min.counts) {
        integ.time <- round(integ.time * target.min.counts / max.counts * 1.1, 0)
      } else {
        integ.time <- round(integ.time * 1.2, 0)
      }
    }

    if (integ.time > max.integ.time) {
      if (verbose) {
        warning("Light level is too low for optimal performance! Using (ms): ",
                format(integ.time * 1e-3))
      }
      break()
    }

    if (integ.time < min.integ.time) {
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
  integ.time <- HDR.mult * integ.time # vectorized!
  integ.time <- ifelse(integ.time > max.integ.time, max.integ.time, integ.time)
  integ.time <- ifelse(integ.time < min.integ.time, min.integ.time, integ.time)
  num.scans <- ifelse(integ.time < min.tot.time,
                      trunc(min.tot.time / integ.time) + 1,
                      1)
  total.time <- integ.time * num.scans
  z <- list(
    pix.selector = pix.selector,
    HDR.mult = HDR.mult,
    integ.time = integ.time,
    max.integ.time = max.integ.time,
    min.integ.time = min.integ.time,
    num.scans = num.scans,
    #diagnosis
    total.time = total.time,
    rel.signal = max.counts / oo_descriptor$max.counts
  )

  if (verbose) {
    message("Relative saturation: ",
            format(z$rel.signal, width = 8), " ")
    message("Integration times (ms): ",
            format(z$integ.time * 1e-3, nsmall = 0, width = 8), " ")
    message("Numbers of scans:       ",
            format(z$num.scans, width = 8), " ")
    message("Total time (s):         ",
            format(z$integ.time * z$num.scans * 1e-6,
                   digits = 3, width = 8), " ")
  }

  z
}

#' Tune settings for measurement
#'
#' Find optimal settings for spectral measurements under a given measurement
#' protocol.
#'
#' @rdname acq_settings
#' @export
#' @return a list.
#' @rdname acq_settings
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
  pix.selector <- x$pix.selector
  # convert times to microseconds
  start.integ.time <- x$start.integ.time * 1e6
  min.integ.time   <- x$min.integ.time   * 1e6
  max.integ.time   <- x$max.integ.time   * 1e6
  min.tot.time   <- x$min.tot.time   * 1e6
  # make sure HDR multipliers are sorted
  HDR.mult <- x$HDR.mult
  mult.reorder <- order(HDR.mult)
  HDR.mult <- HDR.mult[mult.reorder]
  NR.flag <- NR.flag[mult.reorder]
  if (HDR.mult[1] > 1) {
    warning("Using a smallest 'HDR.mult' value that is > 1 will cause clipping")
  }
  # We check bounds and set the "fixed settings" (non-tunable)
  min.integ.time <- max(min.integ.time, oo_descriptor$min.integ.time)
  max.integ.time <- min(max.integ.time, oo_descriptor$max.integ.time)
  if (is.logical(pix.selector)) {
    stopifnot(length(pix.selector) == length(oo_descriptor$wavelengths))
  }
  pix.selector <- pix.selector
  rOmniDriver::set_correct_for_electrical_dark(oo_descriptor$w, 0L,
                                               oo_descriptor$sr.index,
                                               oo_descriptor$ch.index)
  rOmniDriver::set_correct_for_detector_nonlinearity(oo_descriptor$w, 0L,
                                                     oo_descriptor$sr.index,
                                                     oo_descriptor$ch.index)
  rOmniDriver::set_boxcar_width(oo_descriptor$w, 0L,
                                oo_descriptor$sr.index,
                                oo_descriptor$ch.index)
  rOmniDriver::set_scans_to_avg(oo_descriptor$w, 1L,
                                oo_descriptor$sr.index,
                                oo_descriptor$ch.index)

  # optimize parameters
  integ.time <- start.integ.time
  target.min.counts <- 0.8 * oo_descriptor$max.counts

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
    max.counts <- max(raw.counts[pix.selector])
    while (rOmniDriver::is_saturated(oo_descriptor$w,
                                     oo_descriptor$sr.index,
                                     oo_descriptor$ch.index))
    {
      integ.time <- integ.time * 0.6666667
      if (integ.time < min.integ.time) {
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
      max.counts <- max(raw.counts[pix.selector])
    }
    if (verbose) message(paste("max.counts[", i, "]: ", format(max.counts)))
    if (max.counts < target.min.counts && integ.time < max.integ.time) {
      if (verbose) message("max count < 50000")
      if (max.counts < 0.9 * target.min.counts) {
        integ.time <- round(integ.time * target.min.counts / max.counts * 1.1, 0)
      } else {
        integ.time <- round(integ.time * 1.2, 0)
      }
    }

    if (integ.time > max.integ.time) {
      if (verbose) {
        warning("Light level is too low for optimal performance! Using (ms): ",
                format(integ.time * 1e-3))
      }
      break()
    }

    if (integ.time < min.integ.time) {
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
  integ.time <- HDR.mult * integ.time # vectorized!
  integ.time <- ifelse(integ.time > max.integ.time, max.integ.time, integ.time)
  integ.time <- ifelse(integ.time < min.integ.time, min.integ.time, integ.time)
  num.scans <- ifelse(integ.time < min.tot.time,
                      trunc(min.tot.time / integ.time) + 1,
                      1)
  total.time <- integ.time * num.scans
  z <- list(
    pix.selector = pix.selector,
    HDR.mult = HDR.mult,
    integ.time = integ.time,
    max.integ.time = max.integ.time,
    min.integ.time = min.integ.time,
    num.scans = num.scans,
    #diagnosis
    total.time = total.time,
    rel.signal = max.counts / oo_descriptor$max.counts
  )

  if (verbose) {
    message("Relative saturation: ",
            format(z$rel.signal, width = 8), " ")
    message("Integration times (ms): ",
            format(z$integ.time * 1e-3, nsmall = 0, width = 8), " ")
    message("Numbers of scans:       ",
            format(z$num.scans, width = 8), " ")
    message("Total time (s):         ",
            format(z$integ.time * z$num.scans * 1e-6,
                   digits = 3, width = 8), " ")
  }

  z
}
