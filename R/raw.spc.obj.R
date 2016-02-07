get_oo_descriptor <- function(w, sr.index = 0L) {
  list(
    w = w,
    sr = sr.index,
    spectrometer.name = rOmniDriver::get_name(w, sr.index),
    spectrometer.sn =  rOmniDriver::get_serial_number(w, sr.index),
    bench.grating = bench$getGrating(),
    bench.filter = bench$getFilterWavelength(),
    bench.slit = bench$getSlitSize(),
    bench.detector = rOmniDriver::get_detector(w, sr.index),
    min.integ.time = rOmniDriver::get_minimum_integration_time(w, sr.index),
    max.integ.time = rOmniDriver::get_maximum_integration_time(w, sr.index),
    wavelengths = rOmniDriver::get_wavelengths(w, sr.index)
  )
}

get_oo_settings <- function(w, sr.index = 0L) {
  bench <- rOmniDriver::get_bench(w, sr.index)
  list(
    w = w,
    sr = sr.index,
    correct.elec.dark = rOmniDriver::get_correct_for_electrical_dark(w, sr.index),
    correct.non.lin = rOmniDriver::get_correct_for_detector_nonlineary(w, sr.index),
    correct.stray.light = rOmniDriver::get_correct_for_stray_light(w, sr.index),
    boxcar.width = rOmniDriver::get_boxcar_width(w, sr.index),
    integ.time = rOmniDriver::get_integration_time(w, sr.index),
    num.scans = rOmniDriver::get_scans_to_avg(w, sr.index),
  )
}


#' Create a new list of raw spectra
#'
#' Function to create a list object to be used to store
#' one set of raw spectra.
#'
#' @keywords manip misc
#' @export
#' @return a list
#'
new_raw_spc <- function() {
  x <- list(
    init = FALSE,
    params.set = FALSE,
    data.valid = FALSE,
    user.label = "",
    settings = list(
      w = NA,
      sr = NA,
      spectrometer.sn = "",
      spectrometer.name = "",
      bench = NA,
      correct.elec.dark = NA,
      correct.non.lin = NA,
      boxcar.width = NA,
      pix.selector = TRUE,
      min.integ.time = NA,
      max.integ.time = NA,
      integ.time = NA,
      num.scans = NA,
      mode = NA
    ),
    data = list(),
    version = "1"
  )
  return(x)
}

#' Initialize a list of raw spectra
#'
#' Function to initialize a list object to be used to store
#' one set of raw spectra.
#'
#' @param x an empty list of raw spectra
#' @param w an open Wrapper object from Omnidriver
#' @param sr.index an index to address the spectrometer
#' @keywords manip misc
#' @export
#' @return a list
#'
init_raw_spc <- function(x = new_raw_spc(), w, sr.index = 0) {
  x$settings$w  <- w
  x$settings$sr <- sr.index
  x$settings$spectrometer.name <- rOmniDriver::get_name(w, sr.index)
  x$settings$spectrometer.sn <- rOmniDriver::get_serial_number(w, sr.index)
  bench <- rOmniDriver::get_bench(w, sr.index)
  x$settings$bench <- list(grating = bench$getGrating(),
                  filter = bench$getFilterWavelength(),
                  slit = bench$getSlitSize()
#                  detector = bench$get_detector(w, sr.index)
  )
  x$init <- TRUE
  return(x)
}

#' Set the measurement parameters of a list of raw spectra
#'
#' Function to set parameters of a list object to be used to store
#' one set of raw spectra.
#'
#' @param x an initialized list of raw spectra
#' @param start.int.time numeric vaue in seconds
#' @param min.int.time numeric vaue in seconds
#' @param max.int.time numeric vaue in seconds
#' @param min.tot.time numeric vaue in seconds
#' @param HDR.mult a numeric vector with int.time multipliers
#' @param NR.flag a logical vector indicating whether filter is used
#' @param user.label a character string to be stored
#' @param at.geocode a numeric vector of length two with
#' @param mode a character string (only "raw" is implemented)
#' @param verbose a logical to enable or disable warnings
#' @param pix.selector a logical or numeric vector used as subscript to select
#'   pixels
#'
#' @note The Maya2000Pro specifications give min.int.time at 7.2 ms and
#'   max.int.time at 5 s. It seems to be possible to use longer max.int.time,
#'   but when very long times are used the driver may override the setting to an
#'   unknown value, causing gross errors when the calibration is applied as the
#'   integration time stored in the returned object may differ from the one
#'   really used by the instrument.
#'
#' @keywords manip misc
#' @export
#' @return a list
#'
set_params_raw_spc <- function(x,
                               start.int.time = 50e-3, # seconds
                               min.int.time = 7.2e-3, # seconds
                               max.int.time = 65, # seconds
                               min.tot.time = 2, # seconds
                               HDR.mult = c(short = 1, long = 10),
                               NR.flag = c(short = TRUE, long = TRUE),
                               user.label = "",
                               at.geocode = c(lat=NA, lon=NA),
                               mode = "raw",
                               verbose = TRUE,
                               pix.selector = TRUE) {
  if (!x$init) {
    warning("Object not initilized. Parameters not set.")
    return(x)
  }
  if (mode != "raw") {
    warning("Mode: ", mode, " not supported.")
    return(x)
  }
  # convert times to microseconds
  start.int.time <- start.int.time * 1e6
  min.int.time   <- min.int.time   * 1e6
  max.int.time   <- max.int.time   * 1e6
  min.tot.time   <- min.tot.time   * 1e6
  # make sure HDR multipliers are sorted
  HDR.mult <- sort(HDR.mult)
  if (HDR.mult[1] > 1) {
    warning("Using a smallest 'HDR.mult' value that is > 1 will cause clipping")
  }
  # mode is "raw"
  x$user.label <- user.label
  x$settings$mode <- mode
  x$settings$pix.selector <- pix.selector
  rOmniDriver::set_correct_for_electrical_dark(x$settings$w, 0L, x$settings$sr)
  rOmniDriver::set_correct_for_detector_nonlinearity(x$settings$w, 0L, x$settings$sr)
  rOmniDriver::set_boxcar_width(x$settings$w, 0L, x$settings$sr)
  rOmniDriver::set_scans_to_avg(x$settings$w, 1L, x$settings$sr)
  x$settings$correct.elec.dark <- FALSE
  x$settings$correct.non.lin <- FALSE
  x$settings$boxcar.width <- 0L
  x$settings$min.integ.time <- min.int.time
  x$settings$max.integ.time <- max.int.time

  # optimize parameters
  int.time <- start.int.time

  i <- 0L
  repeat {
    if (verbose) {
      message("Integration time (ms): ", format(int.time  * 1e-3))
    }
    rOmniDriver::set_integration_time(x$settings$w, int.time, x$settings$sr)
    spc <- rOmniDriver::get_spectrum(x$settings$w, x$settings$sr)
    max.counts <- max(spc[pix.selector])
    while (rOmniDriver::is_saturated(x$settings$w, x$settings$sr))
    {
      int.time <- int.time * 0.6666667
      if (int.time < min.int.time) {
        warning("Clipping cannot be avoided! Using (ms): ", int.time)
        break()
      }
      if (verbose) message("Clipping! Trying (ms): ", format(int.time  * 1e-3))
      rOmniDriver::set_integration_time(x$settings$w, int.time, x$settings$sr)
      spc <- rOmniDriver::get_spectrum(x$settings$w, x$settings$sr)
      max.counts <- max(spc[pix.selector])
    }
    if (verbose) message(paste("max.counts[", i, "]: ", format(max.counts)))
    if (max.counts < 50000 && int.time < max.int.time) {
      if (verbose) message("max count < 50000")
      if (max.counts < 45000) {
        int.time <- round(int.time * 55000 / max.counts, 0)
      } else {
        int.time <- round(int.time * 1.2, 0)
      }
    }

    if (int.time > max.int.time) {
      int.time <- max.int.time

      if (verbose) {
        warning("Light level is too low for optimal performance.")
      }
    }
    if (max.counts >= 50000 || int.time >= max.int.time){
      break()
    } else {
      i <- i + 1
    }
  }
  int.time <- HDR.mult * int.time # vectorized!
  int.time <- ifelse(int.time > max.int.time, max.int.time, int.time)
  int.time <- ifelse(int.time < min.int.time, min.int.time, int.time)
  x$settings$integ.time <- int.time

  x$settings$num.scans <- ifelse(x$settings$integ.time < min.tot.time,
                                 trunc(min.tot.time / x$settings$integ.time) + 1,
                                 1)
  if (verbose) {
    message("Maximum counts:         ",
            format(max.counts, width = 8), " ")
    message("Integration times (ms): ",
            format(x$settings$integ.time * 1e-3, nsmall = 0, width = 8), " ")
    message("Numbers of scans:       ",
            format(x$settings$num.scans, width = 8), " ")
    message("Total time (s):         ",
            format(x$settings$integ.time * x$settings$num.scans * 1e-6,
                   digits = 3, width = 8), " ")
  }
  x$params.set <- TRUE
  return(x)
}

#' Copy the initialization data and measurement parameters from one list of raw spectra to another
#'
#' Function for copying the initialization data and measurement parameters
#' from one list of raw spectra to another.
#'
#' @param x.to an empty list of raw spectra
#' @param x.from an initialized and set up list of raw spectra
#' @param user.label a character string to be stored
#'
#' @note x.from may contain data or not. If present data is not copied.
#'
#' @keywords manip misc
#' @export
#' @return a list
#'
copy_settings <- function(x.from, x.to=new_raw_spc(), user.label=NULL) {
  x.to[["settings"]] <- x.from[["settings"]]
  x.to$init <- x.from$init
  x.to$params.set <- x.from$params.set
  x.to$data.valid <- FALSE
  if (is.null(user.label)) {
    x.to$user.label <- x.from$user.label
  } else {
    x.to$user.label <- user.label
  }
  return(x.to)
}

#' Take one spectral reading with parameters from a list of raw spectra
#'
#' Function to take one set of possibly bracketed spectra using parameters stored in
#' a list object used to store one set of raw spectra. The spectra are returned
#' as a list of numeric vectors.
#'
#' @param x an initialized list of raw spectra containing parameter values
#' @param verbose a logical to enable or disable warnings
#' @keywords manip misc
#' @export
#' @return a list of numeric vectors
#'
take_one_reading <- function(x, verbose = TRUE) {
  num.HDR.meas <- length(x$settings$integ.time)
  raw.scans <- list(start.time=Sys.time())
  for (i in 1:num.HDR.meas) {
    scan.name <- paste("scan", formatC(i, width=3, flag="0"), sep="")
    rOmniDriver::set_integration_time(x$settings$w, x$settings$integ.time[i], x$settings$sr)
    actual.integ.time <- rOmniDriver::get_integration_time(x$settings$w, x$settings$sr)
    if (actual.integ.time != x$settings$integ.time[i]) {
      warning("The spectrometer has overridden the requested integration time!")
      message("Requested integ.time: ", signif(x$settings$integ.time[i] * 1e6, 3),
              ", actual integ.time: ", signif(actual.integ.time  * 1e6, 3), " seconds")
      # This is needed to maintain acquired data validity!
      x$settings$integ.time[i] <- actual.integ.time
    }
    rOmniDriver::set_scans_to_avg(x$settings$w, x$settings$num.scans[i], x$settings$sr)
    if (verbose) message(paste("Measurement ", i, "..."))
    spc <- rOmniDriver::get_spectrum(x$settings$w, x$settings$sr)
    if (rOmniDriver::is_spectrum_valid(x$settings$w, x$settings$sr))
    {
      raw.scans[[scan.name]] <- spc
    }
    else
    {
      raw.scans[[scan.name]] <- NA
    }
  }
  return(raw.scans)
}

#' Take one set of spectral readings with parameters from a list of raw spectra
#'
#' Function to take one set of spectral readings as needed for noise reduction (NR)
#' based on substraction of stray light estimated from readings with a long pass filter,
#' and dark readings. Each of these, possibly bracketed as a series of spectra using
#' different values for integration time and number of scans to average. All this information
#' must be already stored in argument x.
#' A copy of x is returned with the results of the measurements added.
#'
#' @note The parameter readings can be used to set the order of the different types of
#' readings, and which readings will be done or skipped. In the current implementation
#' it is possible to include "meas" and "filter" only once, but in contrast, it is possible
#' to take two dark measurements if needed.
#'
#' @param x an initialized list of raw spectra containing parameter values
#' @param protocol a vector of character strings
#' @param add.label a character string to be concatenated to the current label
#' @param verbose a logical to enable or disable warnings
#' @keywords manip misc
#' @export
#' @return a list
#'

take_readings <- function(x, protocol = c("meas", "filter", "dark1"), add.label=NULL, verbose=TRUE) {
  if (!x$init || !x$params.set) {
    warning("Object not in state valid for taking measurements.")
    return(x)
  }
  if (!is.null(add.label)) {
    x$user.label <- paste(x$user.label, add.label)
  }
  x$data$at.time <- Sys.time()
  for (reading in protocol) {
      readline(paste("SR ready to take", reading,  "measurement:"))
      x$data$raw.scans[[reading]] <- take_one_reading(x, verbose=verbose)
  }
  x$data.valid <- TRUE # replace with test
  return(x)
}


