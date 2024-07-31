#' Measure one raw spectrum
#'
#' Take one spectral measurement which depending on the settings can consist
#' in multiple raw spectra meant to represent a SINGLE observation after
#' conversion into calibrated data, such as in the case of bracketing of
#' integration time for HDR.
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor}.
#' @param acq.settings list as returned by functions \code{tune_acq_settings}.
#' @param f.trigger.on,f.trigger.off function Functions to be called
#'   immediately before and immediately after a measurement.
#' @param what.measured value used to set attribute.
#' @param where.measured data.frame with at least columns "lon" and "lat"
#'   compatible with value returned by \code{ggmap::geocode()}.
#' @param set.all logical resend or not all instrument settings.
#' @param verbose logical to enable or disable warnings.
#'
#' @details
#' This function acquires one raw-detector-counts spectrum from a spectrometer,
#' using the \code{descriptor} to connect to the spectrum and retrieve the
#' valid range for settings. The settings in \code{acq.settings} are first
#' sent to the spectrometer and the values retrieved in case the spectrometer
#' has overridden the requested settings. Subsequently a spectrum, possibly
#' obtained by averaging multiple spectra in the spectrometer is acquired, and
#' the spectrometer queried on whether data are valid or not.
#'
#' Function \code{acq_raw_spct()} can optionally call two functions, one at the
#' start of the measurement and another one after it ends. \code{f.trigger.on()}
#' can be used for example when measuring the output from a xenon flash lamp, to
#' trigger a given number of light flashes. In other cases \code{f.trigger.on()}
#' and \code{f.trigger.off()} can be used together to start and end a concurrent
#' measurement or any other action using a relay controlled by code in a
#' function defined by the user.
#'
#' The functions passed as arguments to \code{f.trigger.on()} and
#' \code{f.trigger.off()} should return very quickly when called, so as not to
#' disturb the timing of the measurements of spectra as these start only after
#' \code{f.trigger.on()} returns to the caller.
#'
#' The first formal parameter of \code{f.trigger.on()} should handle as input
#' an integer value indicating the number of events to trigger and a second
#' argument giving the delay in seconds between pulses. Of course the arguments
#' can be ignored if not needed, but should accepted. \code{f.trigger.off()}
#' currently expects no arguments. Example scripts are provided for YoctoPuce
#' USB modules.
#'
#' The default function, displays a message asking the user to manually trigger
#' the flash a number of times that depends on the settings in
#' \code{acq.settings}.
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
                         f.trigger.on = f.trigger.message,
                         f.trigger.off = NULL,
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

  if (anyNA(num.exposures)) {
    stop("'num.exposures' contains NAs")
  }
  stopifnot(length(num.exposures) >= num.readings)
  stopifnot(length(x$HDR.mult) == num.readings)

  y <- descriptor

  z <- tibble::tibble(w.length = y$wavelengths)

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

  start.time <- lubridate::now(tzone = "UTC")

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
    # ensure validity of date in case of rounding errors or other mismatches
    #   compared to intended settings
    x$integ.time[i] <- actual.integ.time

    rOmniDriver::set_scans_to_avg(y$w, x$num.scans[i], y$sr.index, y$ch.index)
    actual.num.scans <- rOmniDriver::get_scans_to_avg(y$w, y$sr.index, y$ch.index)
    # We need to
    if (x$num.scans[i] != actual.num.scans) {
      # We guard against failure to number of scans
      warning("The spectrometer has overridden the number of scans!")
      # ensure validity of data in case of mismatch between actual setting
      #   and intended setting
      x$num.scans[i] <- actual.num.scans
    }

    # light source, e,g,, flash trigger
    # concurrent measurements, e.g., trigger camera once
    # concurrent measurements, e.g., enable sensor or camera
    if (num.exposures[i] != 0L  && !is.null(f.trigger.on)) {
      target.delay <- max(0, (x$num.scans[i] * x$integ.time[i] - 0.05) / 2) # max 1/20 s shutter speed
      f.trigger.on(n = num.exposures[i], delay = target.delay)
    }

    if (verbose) cat("Measurement x", acq.settings$HDR.mult[i], " ... ", sep = "")

    # the USB2000 occasionally returns zero counts (timing problem?)
    # or data could be corrupted, in which case we retry
    j <- 0L
    max.attempts <- 3L
    repeat {
      j <- j + 1L
      counts <- rOmniDriver::get_spectrum(y$w, y$sr.index, y$ch.index)
      if (!all(counts == 0) && rOmniDriver::is_spectrum_valid(y$w, y$sr.index, y$ch.index)) {
        if (verbose) cat("ready.\n")
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

    # concurrent measurements, e.g., disable sensor or camera
    if (!is.null(f.trigger.off)) {
      f.trigger.off()
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
#'   Sequences of light measurements using single "dark" and "filter"
#'   measurements are scheduled by setting the four members of the named list
#'   passed as argument to \code{seq.settings}. The member \code{initial.delay}
#'   is numeric and gives a minimum delay in seconds before the start of
#'   measurements with a default of 0s. Member \code{step.delay} is numeric and
#'   gives the delay in seconds between successive "light" measurements. In
#'   most cases a vector of length one is used as time delta in seconds. Any
#'   vector shorter than the number of steps will be extended with
#'   \code{rep_len()}, and the values interpreted as the time increment
#'   in seconds between the start of successive measurements. If the length is
#'   the same as "num.steps", and the values are monotonically increasing, they
#'   are interpreted as time offsets from the start of the sequence. Member
#'   \code{start.boundary} can take one of "none", "second", "minute" or "hour"
#'   indicating the unit to which the start of the series should be scheduled,
#'   e.g. the next minute and 0s, for "minute". Member \code{num.steps} must
#'   be an integer between 1 and small thousands indicating the number of time
#'   steps in the series.
#'
#' @note Obviously the duration of the time steps must be longer than the time
#'   that a measurement takes. This time can be significantly more than the sum
#'   of integration times, as there is considerable overhead in both the
#'   OmniDriver Java code, in USB communication, in the spectrometer
#'   itself and in R. The overhead depends strongly on the model of
#'   spectrometer.
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
#' @param f.trigger.on,f.trigger.off function Functions to be called
#'   immediately before and immediately after a measurement. See
#'   \code{\link{acq_raw_spct}} for details.
#' @param triggers.enabled character vector Names of protocol steps during which
#'   trigger functions should be called.
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
#' spectrum. Computations on date times are done with
#' \code{\link[lubridate]{lubridate}}.
#'
acq_raw_mspct <- function(descriptor,
                          acq.settings,
                          f.trigger.on = f.trigger.message,
                          f.trigger.off = NULL,
                          triggers.enabled = character(),
                          seq.settings = list(initial.delay = 0,
                                              start.boundary = "none",
                                              step.delay = 0,
                                              num.steps = 1L),
                          protocol = c("light", "filter", "dark"),
                          user.label = "",
                          where.measured = data.frame(lon = NA_real_, lat = NA_real_),
                          pause.fun = NULL,
                          verbose = TRUE,
                          ...) {

  if (getOption("ooacquire.offline", TRUE)) {
    warning("Package 'rOmniDriver' required to access spectrometer.",
            " Data acquisition skipped.",
            call. = FALSE)
    return(raw_mspct())
  }

  default_pause_fun <- function(acq.what, ...) {
    utils::flush.console()
    answ <- readline(paste("Acquire", toupper(acq.what),
                           "readings: g = GO, z = abort (g-/z):"))[1]
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
  steps <- steps + seq.settings[["initial.delay"]][1]

  previous.protocol <- "none"

  # set flag if buffered acquisition of time series is possible
  high.speed <-
    all(seq.settings[["step.delay"]] == 0) &&
    seq.settings[["num.steps"]] > 1L &&
    length(acq.settings[["HDR.mult"]]) == 1L &&
    length(acq.settings[["integ.time"]]) == 1L

  z <- list()
  z.names <- character()
  idx <- 0 # spectrum index into collection (across protocol steps)

  start.time <- lubridate::now(tzone = "UTC")
  for (p in protocol) {
    if (p != previous.protocol) {
      previous.protocol <- p
      if (!pause.fun(p, ...)) {
        return(raw_mspct())
      }
    }
    if (p %in% triggers.enabled) {
      if (is.function(f.trigger.on)) {
        f.on.current <- f.trigger.on
      } else {
        f.on.current <- NULL
      }
      if (is.function(f.trigger.off)) {
        f.off.current <- f.trigger.off
      } else {
        f.off.current <- NULL
      }
    } else {
      f.on.current <- NULL
      f.off.current <- NULL
    }
    if (p == "light") {
      if (is.character(seq.settings[["start.boundary"]]) &&
          seq.settings[["start.boundary"]] == "none") {
        times <- lubridate::now(tzone = "UTC") +
          lubridate::milliseconds(10) +
          lubridate::seconds(steps)
      } else {
        times <-
          lubridate::ceiling_date(lubridate::now(tzone = "UTC") +
                                    lubridate::milliseconds(10),
                                  unit = seq.settings[["start.boundary"]]) +
          lubridate::seconds(steps)
      }
      z.names <- c(z.names,
                   paste(p,
                         format_idx(seq_along(times)),
                         sep = "."))
    } else {
      times <- lubridate::now(tzone = "UTC")
      z.names <- c(z.names, p)
    }

    if (verbose && p == "light") {
      if (high.speed) {
        cat("Buffered series of ", seq.settings[["num.steps"]], " starting at ",
            strftime(times[1], format = "%H:%M:%OS", usetz = TRUE),
            " (\"quiet\")\n", sep = "")
      } else if (all(seq.settings[["step.delay"]] == 0) && length(times) > 1L) {
        cat("Fast series  of ", seq.settings[["num.steps"]], " starting at ",
            strftime(times[1], format = "%H:%M:%OS", usetz = TRUE),
            " (\"quiet\")\n", sep = "")
      } else if (length(times) > 1L) {
        cat("Timed series of ", length(times), " from ",
            strftime(times[1], format = "%H:%M:%OS", usetz = TRUE), " to ",
            strftime(times[length(times)], format = "%H:%M:%OS", usetz = TRUE),
            "\n", sep = "")
      } else if (steps[1] > 1) {
        cat("Timed acquisition at ", strftime(times[1], format = "%H:%M:%OS", usetz = TRUE),
            "\n", sep = "")
      }
    }

    if (high.speed && p == "light") {
      # acquire multiple spectra as fast as possible
      zz <- hs_acq_raw_mspct(descriptor = descriptor,
                             acq.settings = acq.settings,
                             num.spectra = length(times),
                             f.trigger.on = f.on.current,
                             f.trigger.off = f.off.current,
                             what.measured = paste(p, " HS: ", user.label, sep = ""),
                             where.measured = where.measured,
                             verbose = TRUE,
                             return.list = TRUE)
      z <- c(z, zz)
      idx <- idx + length(zz)
    } else {
      messages.enabled <-
        verbose && (p %in% c("dark", "filter") ||
                      length(times) == 1L ||
                      length(times) > 1L && all(seq.settings[["step.delay"]] > 0.5)
                    )

      # acquire multiple spectra one by one at target times
      if (verbose && !messages.enabled) message("Acquiring spectra ... ", appendLF = FALSE)
      for (i in seq_along(times)) {
        if (messages.enabled && length(times) > 1L) {
          message("Time point ", i, " of ", length(times))
        }
        repeat {
          seconds.to.wait <- lubridate::seconds(times[[i]] - lubridate::now(tzone = "UTC"))
          if (seconds.to.wait <= 0.0005) {
            break()
          }
          Sys.sleep(seconds.to.wait)
        }
        idx <- idx + 1

        z[[idx]] <-
          acq_raw_spct(descriptor = descriptor,
                       acq.settings = acq.settings,
                       f.trigger.on = f.on.current,
                       f.trigger.off = f.off.current,
                       what.measured = paste(z.names[[i]], ": ", user.label, sep = ""),
                       where.measured = where.measured,
                       verbose = messages.enabled)
      }
      if (verbose && !messages.enabled) message("ready.")
    }
  }
  end.time <- lubridate::now(tzone = "UTC")

  stopifnot(length(z) == length(z.names), !anyNA(z.names))
  names(z) <- z.names

  z <- photobiology::as.raw_mspct(z)

  if (verbose && (seq.settings[["num.steps"]] > 1L || high.speed)) {
    light.spectra.idx <- grepl("^light", names(z))
    actual.times <-
      unlist(photobiology::when_measured(z[light.spectra.idx])[["when.measured"]],
             use.names = FALSE)
    actual.steps <- round(diff(actual.times), 3) # rounded to millisecond
    cat("Realized series of ",
        length(actual.times),
        " from ",
        strftime(actual.times[1], format = "%H:%M:%OS", usetz = TRUE),
        " to ",
        strftime(actual.times[length(actual.times)],
                 format = "%H:%M:%OS", usetz = TRUE), "\n", sep = "")
    cat(length(actual.steps), " time intervals (min, median, max): ",
        paste(format(c(min(actual.steps),
                       stats::median(actual.steps),
                       max(actual.steps))),
              collapse = ", "), "\n", sep = "")
    cat("Elapsed time ", format(end.time - start.time, digits = 4),
        "; from ", strftime(start.time, format = "%H:%M:%OS", usetz = TRUE),
        " to ",  strftime(end.time, format = "%H:%M:%OS", usetz = TRUE),
        "\n", sep = "")
  }
  z
}

