#' Measure one raw spectrum
#'
#' Take one spectral measurement which depending on the settings can consist
#' in multiple raw spectra meant to represent a SINGLE observation after
#' conversion into calibrated data, such as in the case of bracketing of
#' integration time for HDR.
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor}.
#' @param acq.settings list as returned by functions \code{tune_acq_settings}.
#' @param f.trigger.pulses function Function to be called to trigger light
#'   pulse(s). Should accept as its only argument the number of pulses, and
#'   return \code{TRUE} on sucess and \code{FALSE} on failure.
#' @param what.measured value used to set attribute.
#' @param where.measured data.frame with at least columns "lon" and "lat"
#'   compatible with value returned by \code{ggmap::geocode()}.
#' @param set.all logical resend or not all instrument settings.
#' @param verbose logical to enable or disable warnings.
#'
#' @family raw-counts-spectra acquisition functions
#'
#' @export
#'
#' @return A \code{raw_spct} object with one column \code{w.length} and one
#'   column \code{counts}, or two or more columns \code{counts1, counts2, ...}
#'   containing raw counts data. The number of columns with raw counts is
#'   determined by \code{acq.settings}, with multiple columns in the case of
#'   integration time bracketing or HDR.
#'
#' @seealso \code{\link{acq_raw_mspct}} which can be used to acquire multiple
#' spectra according to a user defined protocol.
#'
acq_raw_spct <- function(descriptor,
                         acq.settings,
                         f.trigger.pulses = f.trigger.message,
                         what.measured = NA,
                         where.measured = data.frame(lon = NA_real_, lat = NA_real_),
                         set.all = TRUE,
                         verbose = TRUE) {
  if (getOption("ooacquire.offline", TRUE)) {
    warning("Package 'rOmniDriver' required to access spectrometer. Data acquisition skipped.")
    return(raw_spct())
  }
  x <- acq.settings
  x$integ.time <- as.integer(x$integ.time) # integer microseconds

  num.readings <- length(x$integ.time)

  num.exposures <- x$num.exposures
  if (length(num.exposures) != num.readings) {
    num.exposures <- rep_len(num.exposures, num.readings)
    x <- set_num_exposures(x, num.exposures)
  }

  stopifnot(length(num.exposures) >= num.readings)
  stopifnot(length(x$HDR.mult) == num.readings)

  y <- descriptor

  z <- tibble::tibble(w.length = y$wavelengths)
  start.time <- lubridate::now(tzone = "UTC")

  if (set.all) {
    # set according to acq.settings
    # correction for electrical dark (in instrument using occluded pixels in array)
    rOmniDriver::set_correct_for_electrical_dark(descriptor$w,
                                                 x$corr.elect.dark,
                                                 descriptor$sr.index,
                                                 descriptor$ch.index)

    # correction for sensor non-linearity (in instrument)
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

    if (verbose) message("Measurement x", acq.settings$HDR.mult[i], " ... ", appendLF = FALSE)

    # light source, e,g,, flash trigger
    if (num.exposures[i] > 0L  && !is.null(f.trigger.pulses)) {
      f.trigger.pulses(num.exposures[i])
    }

    # the USB2000 occasionally returns zero counts (timing problem?)
    # or data could be corrupted, in which case we retry
    j <- 0L
    max.attempts <- 3L
    repeat {
      j <- j + 1L
      counts <- rOmniDriver::get_spectrum(y$w, y$sr.index, y$ch.index)
      if (!all(counts == 0) && rOmniDriver::is_spectrum_valid(y$w, y$sr.index, y$ch.index)) {
        if (verbose) message("ready.")
        break()
      } else if (j > max.attempts) {
        counts <- rep_len(NA_real_, length(counts))
        if (verbose) message("failed in ", j, " attempts.")
        break()
      }
    }

    if (!rOmniDriver::is_spectrum_valid(y$w, y$sr.index, y$ch.index) && !x$force.valid)
    {
      counts <- rep_len(NA_real_, length(counts))
    }

    z[[counts.name]] <- counts

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
#' @details Function \code{acq_raw_mspct} acquires directly from a spectrometer
#'   a collection of spectra. The settings used for the acquisition of each
#'   member spectrum are the same, and are given by the argument passed to
#'   \code{acq.settings}. The number of numbers and their names are given by the
#'   argument passed to \code{protocol}.
#'
#'   Two types of light sources can be measured, for continuous-emission light
#'   sources, the integration time can at later steps used to compute irradiance.
#'   In the case of flashes, the duration of the exposure is unknown and
#'   irradiance cannot be computed, while spectral energy per flash can be
#'   computed if the number of flashes is known. The argument to
#'   \code{num.exposures} must be set to the number of flashes.
#'
#'   Two parameters accept functions as arguments, and default to functions that
#'   request the operator to trigger the flash or change the light conditions
#'   according to the names of the steps in the argument to \code{protocol}.
#'
#'   For sequences of light measurements using single "dark" and "filter"
#'   measurements the interpretation of "step.delay" can vary. If shorter than
#'   the number of steps, the values are interpreted as the time increment in
#'   seconds between the start of successive measurements. If the length is
#'   the same as "num.steps", and the values are monotonically increasing,
#'   they are interpreted as time offsets from the start of the sequence.
#'
#' @note Obviously the duration of the time steps must be longer than the time
#'   that a measurment takes. This time can be significantly more than the sum
#'   of integration times, as there is considerable overhead in both the
#'   OmniDriver Java code, in USB communication, in the spectrometer
#'   itself and in R.
#'
#'   No multitasking is used or supported, so R waits for the spectrometer to
#'   answer. The operating system and other programs are not blocked, but the
#'   current R instance is.
#'
#' @family raw-counts-spectra acquisition functions
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor()}.
#' @param acq.settings list as returned by functions \code{tune_acq_settings()}
#'   or \code{retune_acq_settings()} or \code{acq_settings()}.
#' @param f.trigger.pulses function Function to be called to trigger light
#'   pulse(s). Should accept as its only argument the number of pulses, and
#'   return \code{TRUE} on sucess and \code{FALSE} on failure.
#' @param seq.settings list with members "initial.delay", "step,delay" numeric
#'   values in seconds, "num.steps" integer.
#' @param protocol vector of character strings.
#' @param user.label character string to set as label.
#' @param where.measured data.frame with at least columns "lon" and "lat"
#'   compatible with value returned by \code{ggmap::geocode()}.
#' @param pause.fun function used for handling protocol transitions.
#' @param verbose ogical to enable or disable warnings.
#' @param ... passed to \code{pause.fun} (ignored by the default function).
#'
#' @export
#'
#' @return A raw_mspct object. The names and number of member spectra are
#' determined by \code{protocol}, and the number of columns in each member
#' spectrum is determined by \code{acq.settings}.
#'
#' @seealso \code{\link{acq_raw_spct}} which is used to acquire each member
#' spectrum.
#'
acq_raw_mspct <- function(descriptor,
                          acq.settings,
                          f.trigger.pulses = f.trigger.message,
                          seq.settings = list(initial.delay = 0,
                                              step.delay = 0,
                                              num.steps = 1L),
                          protocol = c("light", "filter", "dark"),
                          user.label = "",
                          where.measured = data.frame(lon = NA_real_, lat = NA_real_),
                          pause.fun = NULL,
                          verbose = TRUE,
                          ...) {
  if (getOption("ooacquire.offline", TRUE)) {
    warning("Package 'rOmniDriver' required to access spectrometer. Data acquisition skipped.")
    return(raw_mspct())
  }
  default_pause_fun <- function(acq.what, ...) {
    utils::flush.console()
    answ <- readline(paste("Acquire", toupper(acq.what),
                           "scans, z = abort (m-/z):"))[1]
    tolower(answ) != "z"
  }

  if (is.null(pause.fun)) {
    pause.fun <- default_pause_fun
  }

  if (seq.settings[["num.steps"]] == 1L) {
    steps <- 0 # (seconds) or no delay
  } else if (length(seq.settings[["step.delay"]]) == seq.settings[["num.steps"]] &&
             !is.unsorted(seq.settings[["step.delay"]], strictly = TRUE)) {
    # use vector of time offsets as is
    steps <- seq.settings[["step.delay"]]
  } else {
    # build vector of offsets with regular pattern of steps
    steps <- c(0,
               cumsum(rep_len(seq.settings[["step.delay"]],
                              length.out = seq.settings[["num.steps"]] - 1L)))
  }
  # add initial delay
  steps <- steps + seq.settings[["initial.delay"]]

  if (verbose && length(steps) > 1L) {
    message("'steps' = ", paste(steps, collapse = ", "), " (seconds)")
  }

  previous.protocol <- "none"
  z <- z.names <- list()
  idx <- 0
  start.time <- lubridate::now(tzone = "UTC")

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
    if (p == "light") {
      times <-
        lubridate::ceiling_date(lubridate::now(tzone = "UTC"),
                                unit = seq.settings[["start.boundary"]]) +
        seconds(steps)
      z.names <- paste(p,
                       format_idx(seq_along(times)),
                       sep = ".")
    } else {
      times <- lubridate::now(tzone = "UTC")
      z.names <- p
    }

    if (verbose && length(times) > 1L) {
      message("Series from ", times[1], " to ", times[length(times)],
              " taking ", length(times), " measurements")
    }

    delays <- numeric(length(times)) # delays compared to scheduled time
    for (i in seq_along(times)) {
      if (verbose && length(times) > 1L) {
        message("Time step ", i)
      }
      repeat {
        # we could subtract a lag correction dependent on host and spectrometer
        seconds.to.wait <- lubridate::seconds(times[[i]] - lubridate::now(tzone = "UTC"))
        if (seconds.to.wait <= 0.001) {
          delays[i] <- round(seconds.to.wait * -1e3, 0)
          break()
        }
        Sys.sleep(seconds.to.wait)
      }
      idx <- idx + 1
      acq.time <- lubridate::now(tzone = "UTC")
      z[[idx]] <- acq_raw_spct(descriptor = descriptor,
                               acq.settings = acq.settings,
                               f.trigger.pulses = f.current,
                               what.measured = paste(p, ": ", user.label, sep = ""),
                               where.measured = where.measured)
      # next 3 statements shouldn't be needed. CHECK!
      photobiology::setWhenMeasured(z[[idx]], acq.time)
      photobiology::setWhereMeasured(z[[idx]], where.measured)
      photobiology::setWhatMeasured(z[[idx]], paste(p, ":", user.label))
      # remove dependency of object on rJava
      trimInstrDesc(z[[idx]], c("-", "w"))
    }
  }
  end.time <- lubridate::now(tzone = "UTC")
  if (length(times) > 1L && verbose) {
    message("Series: delays (min, median, max): ",
            paste(c(min(delays), stats::median(delays), max(delays)),
                  collapse = ", "),
            " (ms)")
    message("Start: ", start.time,
            ", end: ", end.time, ", ellapsed: ", format(end.time - start.time, digits = 4),
            "\nfirst step: ", when_measured(z[[1]]),
            ", last step: ", when_measured(z[[length(z)]]))
  }
  z <- photobiology::as.raw_mspct(z)

  if (length(z) == length(protocol)) {
    names(z) <- protocol
  } else {
    stopifnot(length(z) == length(protocol) + length(steps) - 1)
    names(z) <- unlist(z.names)
  }
  z
}

