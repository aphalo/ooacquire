#' Get the instrument description and EEPROM data
#'
#' Model, configuration, serial number, and calibration data stored in the
#' EEPROM of an Ocean Optics spectrometer are retrieved and returned in a
#' list.
#'
#' @param w an open Wrapper object from Omnidriver
#' @param sr.index an index to address the spectrometer for the time being not
#'   exported
#' @param ch.index an index to address the channel in a spectrometer with more
#'   than one channel.
#'
#' @export
#' @return a list
#'
get_oo_descriptor <- function(w, sr.index = 0L, ch.index = 0L) {
  get_calib_coeffs <- function() {
    z <- list()
    calib.data <-
      rOmniDriver::get_calibration_coefficients_from_buffer(w, sr.index, ch.index)
    # linearization
    nl.poly <- calib.data$getNlCoefficients()
    z$nl.poly <- polynom::polynomial(nl.poly)
    # stray light
    z$straylight.coeff <- calib.data$getStrayLight()
    z$straylight.slope <- calib.data$getStrayLightSlope()
    # wavelength calibration
    wl.poly <- calib.data$getWlCoefficients()
    z$wl.poly <- polynom::polynomial(wl.poly)
    z
  }
  bench <- rOmniDriver::get_bench(w, sr.index, ch.index)
  list(
    time = lubridate::now(),
    w = w,
    sr.index = sr.index,
    ch.index = ch.index,
    spectrometer.name = rOmniDriver::get_name(w, sr.index),
    spectrometer.sn =  rOmniDriver::get_serial_number(w, sr.index),
    bench.grating = bench$getGrating(),
    bench.filter = bench$getFilterWavelength(),
    bench.slit = bench$getSlitSize(),
    min.integ.time = rOmniDriver::get_minimum_integration_time(w, sr.index),
    max.integ.time = rOmniDriver::get_maximum_integration_time(w, sr.index),
    max.counts = rOmniDriver::get_maximum_intensity(w, sr.index),
    wavelengths = rOmniDriver::get_wavelengths(w, sr.index, ch.index),
    inst.calib = get_calib_coeffs()
  )
}

#' Get the current values of instrument settings
#'
#' Query the spectrometer for the settings currently in use for corrections,
#' smotthing and acquisition parameters integration time and number of scans.
#'
#' @param w an open Wrapper object from Omnidriver
#' @param sr.index an index to address the spectrometer for the time being not
#'   exported
#' @param ch.index an index to address the channel in a spectrometer with more
#'   than one channel.
#'
#' @export
#' @return a list
#'
get_oo_settings <- function(w, sr.index = 0L, ch.index = 0L) {
  list(
    time = lubridate::now(),
    w = w,
    sr.index = sr.index,
    ch.index = ch.index,
    correct.elec.dark =
      rOmniDriver::get_correct_for_electrical_dark(w, sr.index, ch.index),
    correct.non.lin =
      rOmniDriver::get_correct_for_detector_nonlineary(w, sr.index, ch.index),
    correct.stray.light =
      rOmniDriver::get_correct_for_stray_light(w, sr.index, ch.index),
    boxcar.width = rOmniDriver::get_boxcar_width(w, sr.index, ch.index),
    integ.time = rOmniDriver::get_integration_time(w, sr.index, ch.index),
    num.scans = rOmniDriver::get_scans_to_avg(w, sr.index, ch.index)
  )
}


#' Tune settings for measurement
#'
#' Find parameters for spectral measurements with a given measurements
#' protocol.
#'
#' @param oo_descriptor list as returned by function \code{get_oo_descriptor}
#' @param start.integ.time numeric vaue in seconds
#' @param min.integ.time numeric vaue in seconds
#' @param max.integ.time numeric vaue in seconds
#' @param min.tot.time numeric vaue in seconds
#' @param HDR.mult a numeric vector with integ.time multipliers to be used for
#'   "bracketing".
#' @param NR.flag a logical vector indicating whether a filter reading will be
#'   used.
#' @param pix.selector a logical or numeric vector used as subscript to select
#'   pixels
#' @param verbose a logical to enable or disable warnings
#'
#' @note Ocean Optics spectrometers can be queried for the maximum and minimum
#'   supported integration times. This function modifies the user supplied
#'   values if outside these bounds. The defaults of -Inf and Inf force the use
#'   of the whole valid range of integration times for the connected intrument.
#'   \code{pixel.selector} can be used for two different purposes: to ignore
#'   bad pixels and to restrict integration-time tuning to the response from a
#'   range of pixels.
#'
#' @details \code{tune_acq_settings()} is used usually for the fist measurement
#'   in a series as it starts the tuning of the acquisition parameters from an
#'   integration time suplied by the user or an arbitrary default.
#'   \code{retune_acq_settings()} instead does a similar tunning starting from a
#'   previously adjusted set of values. This is just a convenience function to
#'   simplify data acquisition code and speed-up data acquisition by refining a
#'   previously set value.
#'
#' @keywords manip misc
#' @export
#' @return a list.
#'
tune_acq_settings <- function(oo_descriptor,
                              start.integ.time = 50e-3, # seconds
                              min.integ.time = -Inf, # seconds
                              max.integ.time = Inf, # seconds
                              min.tot.time = 2, # seconds
                              HDR.mult = c(short = 1, long = 10),
                              NR.flag = c(short = TRUE, long = TRUE),
                              pix.selector = TRUE,
                              verbose = TRUE) {
  stopifnot(length(HDR.mult) == length(NR.flag))
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
    NR.flag = NR.flag,
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

#' @rdname tune_acq_settings
#'
#' @param x list as returned by a previous call to \code{tune_acq_settings()} or
#'   \code{retune_acq_settings()}.
#'
#' @export
#'
retune_acq_settings <- function(oo_descriptor,
                                x,
                                verbose = TRUE) {
  tune_acq_settings(oo_descriptor = oo_descriptor,
                    start.integ.time = x$integ.time,
                    min.integ.time = x$min.integ.time,
                    max.integ.time = x$max.integ.time,
                    min.tot.time = x$min.tot.time,
                    HDR.mult = x$HDR.mult,
                    NR.flag = x$NR.flag,
                    pix.selector = x$pix.selector,
                    verbose = verbose)
}

#' Measure one raw spectrum
#'
#' @export
#'
acq_raw_spct <- function(oo_descriptor,
                         acq_settings,
                         what.measured = NA,
                         verbose = TRUE) {
  x <- acq_settings
  y <- oo_descriptor
  num.readings <- length(x$integ.time)
  z <- photobiology::raw_spct(w.length = y$wavelengths, counts = 0)

  for (i in 1:num.readings) {
    counts.name <- paste("counts", i, sep = "_")
    rOmniDriver::set_integration_time(y$w, x$integ.time[i], y$sr.index, y$ch.index)
    actual.integ.time <- rOmniDriver::get_integration_time(y$w, y$sr.index, y$ch.index)
    if (actual.integ.time != x$integ.time[i]) {
      warning("The spectrometer has overridden the integration time!")
      x$integ.time[i] <- actual.integ.time
    }
    rOmniDriver::set_scans_to_avg(y$w, x$num.scans[i], y$sr.index, y$ch.index)
    if (verbose) message(paste("Measurement ", i, "..."))
    spct <- rOmniDriver::get_spectrum(y$w, y$sr.index, y$ch.index)
    if (rOmniDriver::is_spectrum_valid(y$w, y$sr.index, y$ch.index))
    {
      z[[counts.name]] <- spct
    } else {
      z[[counts.name]] <- rep(NA_real_, length(z$w.length))
    }
  }
  photobiology::setInstrDesc(z, y)
  photobiology::setInstrSettings(z, x)
  photobiology::setWhenMeasured(z, lubridate::now())
  photobiology::setWhatMeasured(z, what.measured)
  z
}

#' Take one set of spectral readings
#'
#' Take readings according to parameters from a list of settings and a protocol
#' defined by a vector of names.
#'
#' @param x an initialized list of raw spectra containing parameter values
#' @param protocol a vector of character strings
#' @param add.label a character string to be concatenated to the current label
#' @param verbose a logical to enable or disable warnings
#' @keywords manip misc
#' @export
#' @return a raw_mspct object
#'
acq_raw_mspct <- function(oo_descriptor,
                          acq_settings,
                          protocol = c("measure", "filter", "dark"),
                          user.label,
                          geocode = NA,
                          verbose = TRUE) {
  previous.protocol <- "none"
  z <- list()
  for (p in protocol) {
    if (p != previous.protocol) {
      answ <- readline(paste("Ready to acquire", p,
                             "measurement ('z' = abort)"))
      if (tolower(answ[1]) == "z") {
        break()
      }
    }
    z <- c(z, acq_raw_spct(oo_descriptor = oo_descriptor,
                           acq_settings = acq_settings,
                           what.measured = list(what = p, user.label = user.label)))
  }
  photobiology::as.raw_mspct(z)
  photobiology::setWhereMeasured(z, geocode)
  z
}


