#' Settings for spectral measurement
#'
#' Validate parameters for spectral measurements and return a list of values
#' usable as input for functions \code{retune_acq_settings()}, \code{acq_sptc()},
#' and \code{acq_mspct()}.
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor}
#' @param integ.time numeric vaue in seconds
#' @param num.scans integer
#' @param min.integ.time numeric vaue in seconds
#' @param max.integ.time numeric vaue in seconds
#' @param tot.time.range numeric vector of length two with values in seconds
#' @param HDR.mult a numeric vector with integ.time multipliers to be used for
#'   "bracketing".
#' @param target.margin numeric (0..1)
#' @param pix.selector a logical or numeric vector used as subscript to select
#'   pixels
#' @param corr.elect.dark,corr.sensor.nl  integer 0L (FALSE) or 1L (TRUE)
#' @param boxcar.width integer Number of pixels to average
#' @param force.valid logical Accept all spectra as valid, for example do not
#'   treat clipping as an error condition.
#' @param num.exposures integer Number of flashes triggered per scan.
#' @param verbose a logical to enable or disable warnings
#'
#' @note \code{pixel.selector} can be used for two different purposes: to
#'   ignore bad pixels and to restrict integration-time tuning to the response
#'   from a range of pixels. The interpretation of \code{tot.time.range} is
#'   as follows: first value is minimum time, second value is maximum time.
#'   If both values are the same, then an exact measurement time is computed.
#'
#' @details \code{acq_settings()} is used to create a complete set of
#'   instrument settings in a way that they can be reused as needed for repated
#'   acquisition of spectra. \code{acq_settings()} is an object constructor and
#'   \code{tune_acq_settings()} takes as argument a stored object containing
#'   settings and tunes them to optimize them for the measurement of the current
#'   spectral irradiance.
#'
#' @note Ocean Optics spectrometers can be queried for the maximum and minimum
#'   supported integration times. This function modifies the user supplied
#'   values if outside these bounds. The defaults of -Inf and Inf force the use
#'   of the whole valid range of integration time supported by the connected
#'   intrument. \code{pixel.selector} can be used for two different purposes: to
#'   ignore bad pixels and to restrict integration-time tuning to the response
#'   from a range of pixels.
#'
#' @family acquisition-settings related functions
#'
#' @export
#'
#' @return a list.
#'
acq_settings <- function(descriptor,
                         integ.time = 10e-3, # seconds
                         num.scans = 10L,
                         min.integ.time = -Inf, # seconds
                         max.integ.time = Inf, # seconds
                         tot.time.range = c(0, Inf), # seconds
                         HDR.mult = ifelse(any(num.exposures != -1L),
                                           rep(1, length(num.exposures)),
                                           c(short = 1, long = 10)),
                         target.margin = 0.10, # fraction
                         pix.selector = TRUE,
                         corr.elect.dark = 0L,
                         corr.sensor.nl = 0L,
                         boxcar.width = 0L,
                         force.valid = FALSE,
                         num.exposures = -1L,
                         verbose = TRUE) {
  # Check length consistency
  if (length(num.exposures) == 1 && length(HDR.mult) > 1) {
    num.exposures <- rep(num.exposures, times = length(HDR.mult))
  } else if (length(num.exposures) != length(HDR.mult)) {
    warning("Length missmatch in 'num.exposures', using only first value")
    num.exposures  <- rep(num.exposures[1], times = length(HDR.mult))
  }

  if (any(num.exposures > 0L)) {
    num.scans <- rep(1, times = length(num.exposures))
  }

  stopifnot(length(integ.time) == length(num.scans))

  # if calculations are per exposure, HDR.mult should be all 1
  stopifnot(all(num.exposures == -1L) || all(HDR.mult == 1))

  # convert times to microseconds
  integ.time <- integ.time * 1e6
  min.integ.time <- min.integ.time * 1e6
  max.integ.time <- max.integ.time * 1e6
  tot.time.range <- tot.time.range * 1e6
  # We check bounds and set the "fixed settings" (non-tunable)
  min.integ.time <- max(min.integ.time, descriptor$min.integ.time)
  max.integ.time <- min(max.integ.time, descriptor$max.integ.time)
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
    stopifnot(length(pix.selector) == length(descriptor$wavelengths))
  }
  # return a list
  list(
    # user settings
    pix.selector = pix.selector,
    # tunning settings
    HDR.mult = HDR.mult,
    target.margin = target.margin,
    max.integ.time = max.integ.time,
    min.integ.time = min.integ.time,
    tot.time.range = range(tot.time.range), # fix length != 2
    # instrument settings
    integ.time = integ.time,
    num.scans = num.scans,
    corr.elect.dark = corr.elect.dark,
    corr.sensor.nl = corr.sensor.nl,
    boxcar.width = boxcar.width,
    # flash settings
    num.exposures = num.exposures,
    # accept invalid spectra as good (we may be willing to accept clipping)
    force.valid = force.valid,
    # processing flag
    linearized = as.logical(corr.sensor.nl),
    # diagnosis
    tot.time = integ.time * num.scans,
    rel.signal = NA
  )
}

#' Set linearized attribute
#'
#' Tag the settings as linearized
#'
#' @param acq.settings list as returned by function \code{acq_settings()}
#'
#' @return a copy of the argument passed for \code{acq.settings} with the
#' linearized field of the settings data replaced by TRUE.
#'
#' @family acquisition-settings related functions
#'
#' @keywords internal
#'
set_linearized <- function(acq.settings) {
  acq.settings$linearized <- TRUE
  acq.settings
}

#' Set integration time
#'
#' Set the integration time in the instrument settings data structure.
#'
#' @param acq.settings list as returned by function \code{acq_settings()}
#'
#' @return a copy of the argument passed for \code{acq.settings} with the
#' \code{integ.time} field of the settings data replaced by \code{integ.time}.
#'
#' @family acquisition-settings related functions
#'
#' @keywords internal
#'
set_integ_time <- function(acq.settings,
                           integ.time,
                           single.scan = FALSE,
                           verbose = TRUE) {
  if (length(integ.time) == 1) {
    integ.time <- integ.time * acq.settings$HDR.mult
  } else if (length(integ.time) != length(acq.settings$HDR.mult)) {
    warning("Length missmatch in integration time, using only first value")
    integ.time  <- integ.time[1] * acq.settings$HDR.mult
  }
  integ.time <- ifelse(integ.time > acq.settings$max.integ.time, acq.settings$max.integ.time, integ.time)
  integ.time <- ifelse(integ.time < acq.settings$min.integ.time, acq.settings$min.integ.time, integ.time)
  if (single.scan) {
    num.scans <- rep(1, times = length(integ.time))
  } else {
    num.scans <- ifelse(integ.time < acq.settings$tot.time.range[1],
                        trunc(acq.settings$tot.time.range[1] / integ.time) + 1,
                        1)
    if (acq.settings$tot.time.range[2] - acq.settings$tot.time.range[1] < acq.settings$min.integ.time) {
      integ.time <- acq.settings$tot.time.range[1] / num.scans
    } else if (integ.time[1] > acq.settings$tot.time.range[2]) {
      integ.time <- acq.settings$tot.time.range[2]
    }
  }
  acq.settings$integ.time <- integ.time
  acq.settings$num.scans <- num.scans
  #diagnosis
  acq.settings$tot.time <- integ.time * num.scans
  acq.settings$rel.signal = NA_real_

  if (verbose) {
    message("Relative saturation: ",
            format(acq.settings$rel.signal, width = 10, digits = 3), " ")
    message("Integration times (ms): ",
            format(acq.settings$integ.time * 1e-3, nsmall = 0, width = 10, digits = 3), " ")
    message("Numbers of scans:       ",
            format(acq.settings$num.scans, width = 10, digits = 3), " ")
    message("Total time (s):         ",
            format(acq.settings$tot.time * 1e-6,
                   digits = 3, width = 10), " ")
  }
  acq.settings
}

#' Set number of exposures
#'
#' Set the number of exposures and HDR.multipliers in the instrument settings
#' data structure. This value is by default missing, indicating a protocol in
#' which one assumes contant illumination during an integration. If the light
#' exposure is shorter than the integration time, we need to express the results
#' per exposure, and consequently calculations done based on the number of
#' exposure events during each integration. This is the case, for example, when
#' measuring the spectral emission of a xenon flash.
#'
#' @param acq.settings list as returned by function \code{acq_settings()}
#' @param num.exposures, integer Number exposures per integration, set to
#'   \code{-1L} for continuous illumination.
#' @param HDR.mult a numeric vector with integ.time multipliers to be used for
#'   "bracketing".
#' @param single.scan, logical Will we use a single integration, or average
#'   multiple scans. By default it is \code{TRUE} when multiple exposures are
#'   used, and \code{FALSE} otherwise.
#' @param verbose a logical to enable or disable warnings
#'
#' @return a copy of the argument passed for \code{acq.settings} with the
#' \code{integ.time} field of the settings data replaced by \code{integ.time}.
#'
#' @family acquisition-settings related functions
#'
#' @keywords internal
#'
set_num_exposures <- function(acq.settings,
                              num.exposures = rep(-1L, length(HDR.mult)),
                              HDR.mult = 1,
                              single.scan = num.exposures > -1L,
                              verbose = TRUE) {
#  num.exposures <- as.integer(num.exposures)
  stopifnot(all(num.exposures == -1L) || all(HDR.mult == 1))

  if (length(num.exposures) == 1 && length(HDR.mult) > 1) {
    num.exposures <- rep(num.exposures, times = length(HDR.mult))
  } else if (length(num.exposures) != length(HDR.mult)) {
    warning("Length missmatch in 'num.exposures', using only first value")
    num.exposures  <- rep(num.exposures[1], times = length(HDR.mult))
  }

  if (single.scan || any(num.exposures > 0L)) {
    num.scans <- rep(1, times = length(num.exposures))
    acq.settings$num.scans <- num.scans
  }
  acq.settings$num.exposures <- num.exposures
  acq.settings$HDR.mult <- HDR.mult

  acq.settings
}

#' Tune settings for measurement
#'
#' Find optimal settings for spectral measurements under a given measurement
#' protocol.
#'
#' @details This function searches for the optimal integration time for a
#'   given condition by trial and error helped by interpolation and
#'   extrapolation when readings are not saturated. In the case of clipping or
#'   sensor signal saturation the integration time is decreased until clipping
#'   is avoided and then it is increased until optimal.
#'
#' @note The returned value can be used as argument to \code{acq.settings} in
#'   other functions like \code{\link{acq_raw_spct}} and
#'   \code{\link{acq_raw_mspct}}
#'
#' @rdname acq_settings
#'
#' @return a list Containing the tuned settings.
#'
#' @param acq.settings list as returned by a previous call to
#'   \code{acq_settings()}, or \code{tune_acq_settings()}.
#'
#' @export
#'
tune_acq_settings <- function(descriptor,
                              acq.settings,
                              verbose = TRUE) {
  # old objects are missing this field, so we set it to retain old behaviour
  if (!exists("num.exposures", acq.settings)) {
    acq.settings$num.exposures <- -1L
  }

  x <- acq.settings

  if (!all(x$num.exposures < 0L)) {
    warning("Multiple exposures set.\nIntegration time not adjusted!")
    return(x)
  }

  # correction for electrical dark (in instrument using ocluded pixels in array)
  rOmniDriver::set_correct_for_electrical_dark(descriptor$w, x$corr.elect.dark,
                                               descriptor$sr.index,
                                               descriptor$ch.index)
  # correction for sensor non-linearuty (in instrument)
  rOmniDriver::set_correct_for_detector_nonlinearity(descriptor$w,
                                                     x$corr.sensor.nl,
                                                     descriptor$sr.index,
                                                     descriptor$ch.index)
  # moving window smoothing
  rOmniDriver::set_boxcar_width(descriptor$w,
                                x$boxcar.width,
                                descriptor$sr.index,
                                descriptor$ch.index)
  # to speed up tunning we set number of scans to one
  rOmniDriver::set_scans_to_avg(descriptor$w,
                                1L,
                                descriptor$sr.index,
                                descriptor$ch.index)
  # to more easily reach the target we linearize the counts before interpolation
  nl.fun <- descriptor$inst.calib$nl.fun
  # optimize parameters
  integ.time <- x$integ.time[1]
  target.margin <- x$target.margin
  target.min.counts <- nl.fun((1 - target.margin) * descriptor$max.counts)
  target.max.counts <- nl.fun((1 - target.margin / 2) * descriptor$max.counts)
  target.counts <- nl.fun((1 - target.margin / 3 * 2) * descriptor$max.counts)

  i <- 0L
  repeat {
    if (verbose) {
      message("Integration time (ms): ", format(integ.time  * 1e-3))
    }
    rOmniDriver::set_integration_time(descriptor$w,
                                      integ.time,
                                      descriptor$sr.index,
                                      descriptor$ch.index)
    raw.counts <- rOmniDriver::get_spectrum(descriptor$w,
                                            descriptor$sr.index,
                                            descriptor$ch.index)
    dark.counts <- nl.fun(min(raw.counts[x$pix.selector]))
    max.counts <- nl.fun(max(raw.counts[x$pix.selector]))
    while (max.counts > target.max.counts)
    {
      # increased divisor to 5 to make the "blind" decrease in integration time faster
      integ.time <- integ.time / 5
      if (integ.time < x$min.integ.time) {
        break()
      }
      if (verbose) message("Clipping! Trying (ms): ", format(integ.time  * 1e-3))
      rOmniDriver::set_integration_time(descriptor$w,
                                        integ.time,
                                        descriptor$sr.index,
                                        descriptor$ch.index)
      raw.counts <- rOmniDriver::get_spectrum(descriptor$w,
                                              descriptor$sr.index,
                                              descriptor$ch.index)
      # subtracting the minimum counts as proxi for dark signal improves
      # the guess without much danger of overshooting
      dark.counts <- nl.fun(min(raw.counts[x$pix.selector]))
      max.counts <- nl.fun(max(raw.counts[x$pix.selector]))
    }
    if (verbose) message(paste("max.counts[", i, "]: ", format(max.counts)))
    if (max.counts < target.min.counts && integ.time < x$max.integ.time) {
      if (verbose) message("max count <", trunc(target.min.counts))
      # replaced round with trunc as the algorithm was sometimes overshooting
      integ.time <- trunc(integ.time *
                            (target.counts - dark.counts) /
                            (max.counts - dark.counts))
    }

    if (integ.time >= x$max.integ.time) {
      integ.time <- x$max.integ.time
      if (verbose) {
        warning("Light level is too low for optimal performance! Using (ms): ",
                format(integ.time * 1e-3))
      }
      break()
    }

    if (integ.time < x$min.integ.time) {
      integ.time = x$min.integ.time
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
  acq.settings$integ.time <- integ.time
  acq.settings$num.scans <- num.scans
  #diagnosis
  acq.settings$tot.time <- integ.time * num.scans
  acq.settings$rel.signal = max.counts / nl.fun(descriptor$max.counts)

  if (verbose) {
    message("Relative saturation: ",
            format(acq.settings$rel.signal, width = 10, digits = 3), " ")
    message("Integration times (ms): ",
            format(acq.settings$integ.time * 1e-3, nsmall = 0, width = 10, digits = 3), " ")
    message("Numbers of scans:       ",
            format(acq.settings$num.scans, width = 10, digits = 3), " ")
    message("Total time (s):         ",
            format(acq.settings$tot.time * 1e-6,
                   digits = 3, width = 10), " ")
  }
  acq.settings
}

