#' Measure one raw spectrum
#'
#' Take one spectral measurement which depending on the settings can consist
#' on more than one raw spectrum meant to represent a SINGLE observation after
#' conversion into calibrated data such as in the case of HDR.
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor}
#' @param acq.settings list as returned by functions \code{tune_acq_settings}
#' @param num.exposures integer Number or light pulses (flashes) per scan. Set
#'   to \code{-1L} to indicate that the light source is continuous.
#' @param f.trigger.pulses function Function to be called to trigger light
#'   pulse(s). Should accept as its only argument the number of pulses, and
#'   return \code{TRUE} on sucess and \code{FALSE} on failure.
#' @param what.measured value used to set attribute
#' @param where.measured data.frame with at least columns "lon" and "lat"
#'   compatible with value returned by \code{ggmap::geocode()}
#' @param set.all logical resend or not all instrument settings
#' @param verbose logical to enable or disable warnings
#'
#' @export
#'
#' @return a "raw_spct" object with one or more columns containing raw counts
#' and one column with wavelength data.
#'
acq_raw_spct <- function(descriptor,
                         acq.settings,
                         num.exposures = -1L,
                         f.trigger.pulses = f.trigger.message,
                         what.measured = NA,
                         where.measured = data.frame(lon = NA_real_, lat = NA_real_),
                         set.all = TRUE,
                         verbose = TRUE) {
  x <- acq.settings
  x$integ.time <- as.integer(x$integ.time) # integer microseconds

  num.readings <- length(x$integ.time)

  num.exposures <- as.integer(num.exposures)
  if (length(num.exposures) == 1L) {
    if (num.readings > 1L) {
      num.exposures <- rep(num.exposures, num.readings)
    }
  }
  stopifnot(length(num.exposures) == num.readings)
  x <- set_num_exposures(x, num.exposures)

  y <- descriptor

  z <- dplyr::data_frame(w.length = y$wavelengths)
  start.time <- lubridate::now()

  if (set.all) {
    # set according to acq.settings
    # correction for electrical dark (in instrument using ocluded pixels in array)
    rOmniDriver::set_correct_for_electrical_dark(descriptor$w,
                                                 x$corr.elect.dark,
                                                 descriptor$sr.index,
                                                 descriptor$ch.index)

    # correction for sensor non-linearuty (in instrument)
    rOmniDriver::set_correct_for_detector_nonlinearity(descriptor$w,
                                                       x$corr.sensor.nl,
                                                       descriptor$sr.index,
                                                       descriptor$ch.index)
    actual.corr.sensor.nl <-
      rOmniDriver::get_correct_for_detector_nonlineary(descriptor$w,
                                                     descriptor$sr.index,
                                                     descriptor$ch.index)
    # We need to
    if (x$corr.sensor.nl != actual.corr.sensor.nl) {
      # We guard against failure to set requested setting
      # It should never happen
      warning("The spectrometer has overridden linearity correction setting!")
      x$corr.sensor.nl <- actual.corr.sensor.nl
    }

    # moving window smoothing
    rOmniDriver::set_boxcar_width(descriptor$w,
                                  x$boxcar.width,
                                  descriptor$sr.index,
                                  descriptor$ch.index)
   }

  for (i in 1:num.readings) {
    if (num.readings > 1) {
      counts.name <- paste("counts", i, sep = "_")
    } else {
      counts.name <- "counts"
    }

    rOmniDriver::set_integration_time(y$w, x$integ.time[i], y$sr.index, y$ch.index)
    actual.integ.time <- rOmniDriver::get_integration_time(y$w, y$sr.index, y$ch.index)
    # We need to
    if (as.integer(x$integ.time[i]) - actual.integ.time > x$integ.time[i] * 1e-5) {
      # We guard against failure to set integration time
      # It should never happen as we check validity value requested
      warning("The spectrometer has overridden the integration time!")
    }
    # could improve precision in case of rounding errors
    x$integ.time[i] <- actual.integ.time

    rOmniDriver::set_scans_to_avg(y$w, x$num.scans[i], y$sr.index, y$ch.index)
    actual.num.scans <- rOmniDriver::get_scans_to_avg(y$w, y$sr.index, y$ch.index)
    # We need to
    if (x$num.scans[i] != actual.num.scans) {
      # We guard against failure to set integration time
      # It should never happen as we check validity value requested
      warning("The spectrometer has overridden the number of scans!")
      x$num.scans[i] <- actual.num.scans
    }

    if (verbose) message(paste("Measurement ", i, "..."))
    if (num.exposures[i] > 0L  && !is.null(f.trigger.pulses)) {
      f.trigger.pulses(num.exposures[i])
    }

    counts <- rOmniDriver::get_spectrum(y$w, y$sr.index, y$ch.index)
    if (rOmniDriver::is_spectrum_valid(y$w, y$sr.index, y$ch.index) || x$force.valid)
    {
      z[[counts.name]] <- counts
    } else {
      z[[counts.name]] <- rep(NA_real_, length(z$w.length))
    }

  }

  z <- as.raw_spct(z)

  attr(z, "linearized") <- x$corr.sensor.nl

  photobiology::setInstrDesc(z, y)
  # we remove the Java wrapper so that RJava is not required to read the data
  photobiology::trimInstrDesc(z, c("-", "w"))
  photobiology::setInstrSettings(z, x)
  photobiology::setWhenMeasured(z, start.time)
  photobiology::setWhereMeasured(z, where.measured)
  photobiology::setWhatMeasured(z, what.measured)
  stopifnot(is.raw_spct(z))
  z
}

#' Take one set of spectral readings
#'
#' Take readings according to parameters from a list of settings and a protocol
#' defined by a vector of names.
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor()}
#' @param acq.settings list as returned by functions \code{tune_acq_settings()}
#'   or \code{retune_acq_settings()} or \code{acq_settings()}
#' @param num.exposures integer Number or light pulses (flashes) per scan. Set
#'   to \code{-1L} to indicate that the light source is continuous.
#' @param f.trigger.pulses function Function to be called to trigger light
#'   pulse(s). Should accept as its only argument the number of pulses, and
#'   return \code{TRUE} on sucess and \code{FALSE} on failure.
#' @param seq.settings list with members "step" numeric value in seconds,
#'   "num.steps" integer.
#' @param protocol vector of character strings
#' @param user.label character string to set as label
#' @param where.measured data.frame with at least columns "lon" and "lat"
#'   compatible with value returned by \code{ggmap::geocode()}
#' @param pause.fun function used for handling protocol transitions#'
#' @param verbose ogical to enable or disable warnings
#' @param ... passed to \code{pause.fun} (ignored by the default function)
#'
#' @export
#' @return a raw_mspct object
#'
#' @note The default \code{pause.fun} prompts the user at each change of value
#'   in \code{protocol}. The user can write other functions, for example for a
#'   time delay.
#'
acq_raw_mspct <- function(descriptor,
                          acq.settings,
                          num.exposures = -1L,
                          f.trigger.pulses = f.trigger.message,
                          seq.settings = list(step = 0, num.steps = 1L),
                          protocol = c("light", "filter", "dark"),
                          user.label = "",
                          where.measured = data.frame(lon = NA_real_, lat = NA_real_),
                          pause.fun = NULL,
                          verbose = TRUE,
                          ...) {

  default_pause_fun <- function(acq.what, ...) {
    answ <- readline(paste("Ready to acquire", acq.what,
                           "measurement ('z' = abort)"))
    tolower(answ[1]) != "z"
  }

  if (is.null(pause.fun)) {
    pause.fun <- default_pause_fun
  }

  previous.protocol <- "none"
  z <- list()
  idx <- 0
  start.time <- lubridate::now()
  for (p in protocol) {
    if (p != previous.protocol) {
      previous.protocol <- p
      if (!pause.fun(p, ...)) {
        return(raw_mspct())
      }
    }
    if (p != "dark" && !is.null(f.trigger.pulses)) {
      f.current <- f.trigger.pulses
    } else {
      f.current <- NULL
    }
    idx <- idx + 1
    z[[idx]] <- acq_raw_spct(descriptor = descriptor,
                             acq.settings = acq.settings,
                             num.exposures = num.exposures,
                             f.trigger.pulses = f.current,
                             what.measured = paste(p, ": ", user.label, sep = ""),
                             where.measured = where.measured)
    # next 3 statements shouldn't be needed. CHECK!
    photobiology::setWhenMeasured(z[[idx]], start.time)
    photobiology::setWhereMeasured(z[[idx]], where.measured)
    photobiology::setWhatMeasured(z[[idx]], paste(p, ":", user.label))
    # remove dependency of object on rJava
    trimInstrDesc(z[[idx]], c("-", "w"))
  }
  z <- photobiology::as.raw_mspct(z)

  # assertions to catch errors early on
  stopifnot(is.raw_mspct(z))
  stopifnot(length(z) == length(protocol))

  names(z) <- protocol
  z
}

