#' Acquire spectra at high speed
#'
#' Take one set of spectra at high speed using unchanged instrument settings
#' special OmniDriver API functions for buffered acquisition. No HDR bracketing
#' is possible, and synchronization with a pulsed light source is not supported.
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor}.
#' @param acq.settings list as returned by functions \code{tune_acq_settings}.
#' @param num.spectra integer Number of individual spectra to acquire.
#' @param base.name character The name given to individual spectra is formed
#'   by this string followed by a sequential numeric index.
#' @param f.trigger.on,f.trigger.off function Functions to be called
#'   immediately before and immediately after a measurement. See
#'   \code{\link{acq_raw_spct}} for details.
#' @param what.measured value used to set attribute.
#' @param where.measured data.frame with at least columns "lon" and "lat"
#'   compatible with value returned by \code{ggmap::geocode()}.
#' @param set.all logical resend or not all instrument settings.
#' @param verbose logical to enable or disable warnings.
#' @param return.list logical Return a \code{list} instead of a \code{raw_mspct}
#'   object.
#'
#' @family raw-counts-spectra acquisition functions
#'
#' @export
#'
#' @return A \code{raw_mspct} containing one \code{raw_spct} object with one
#'   column \code{w.length} and one column \code{counts} for each spectrum. The
#'   number of columns with raw counts is always one and integration time
#'   bracketing or HDR values in \code{acq.settings} are ignored except for the
#'   smallest value.
#'
#' @seealso For normal speed acquisition of a single spectrum and multiple
#'   spectra according to a user defined protocol see \code{\link{acq_raw_spct}}
#'   and \code{\link{acq_raw_mspct}}.
#'
hs_acq_raw_mspct <- function(descriptor,
                             acq.settings,
                             num.spectra = 100L,
                             base.name = NULL,
                             f.trigger.on = f.trigger.message,
                             f.trigger.off = NA,
                             what.measured = NA,
                             where.measured = data.frame(lon = NA_real_, lat = NA_real_),
                             set.all = TRUE,
                             verbose = TRUE,
                             return.list = FALSE) {
  if (getOption("ooacquire.offline", TRUE)) {
    warning("Package 'rOmniDriver' required to access spectrometer. Data acquisition skipped.")
    return(raw_spct())
  }
  x <- acq.settings
  if (length(x$integ.time) > 1L ||
      length(x$HDR.mult) > 1L ||
      length(x$tot.time) > 1L ||
      length(x$rel.signal) > 1L) {
    stop("No HDR possible at high speed.")
  }

  num.exposures <- x$num.exposures
  if (length(num.exposures) > 1) {
    x <- set_num_exposures(x, num.exposures[1L])
  }

  num.exposures <- x$num.exposures
  if (length(num.exposures) > 1) {
    num.exposures <- num.exposures[1]

  }

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

  # acquire a batch of spectra

  stopifnot(length(x$integ.time) == 1) # HDR bracketing is not possible

  rOmniDriver::set_integration_time(y$w, x$integ.time, y$sr.index, y$ch.index)
  actual.integ.time <- rOmniDriver::get_integration_time(y$w, y$sr.index, y$ch.index)
  # We need to
  if (as.integer(x$integ.time) - actual.integ.time > x$integ.time * 1e-5) {
    # We guard against failure to set integration time
    # It should never happen as we check validity of value requested
    warning("The spectrometer has overridden the integration time!")
  }
  # could improve precision in case of rounding errors
  x$integ.time <- actual.integ.time

  rOmniDriver::set_scans_to_avg(y$w, x$num.scans, y$sr.index, y$ch.index)
  actual.num.scans <- rOmniDriver::get_scans_to_avg(y$w, y$sr.index, y$ch.index)
  if (as.integer(x$num.scans) - actual.num.scans) {
    # We guard against failure to set number of scans
    # It should never happen as we check validity of value requested
    warning("The spectrometer has overridden the number of scans to average!")
  }

  # setup memory buffer
  rOmniDriver::highSpdAcq_allocate_buffer(y$w, y$sr.index, num.spectra)

  # concurrent measurements, e.g., trigger camera once
  # concurrent measurements, e.g., enable sensor or camera
  if (is.function(f.trigger.on)) {
    f.trigger.on(n = abs(num.exposures[i]))
  }

  if (verbose) message("Acquiring ", num.spectra, " spectra ... ", appendLF = FALSE)

  # start acquisition
  start.time <- lubridate::now(tzone = "UTC")
  rOmniDriver::highSpdAcq_start_acquisition(y$w, y$sr.index)

  if (verbose) message("ready.")

  # concurrent measurements, e.g., disable sensor or camera
  if (is.function(f.trigger.off)) {
    f.trigger.off()
  }

  # retrieve actual number of spectra acquired
  actual.num.spectra <-
    rOmniDriver::highSpdAcq_get_number_of_spectra_acquired(y$w)

  if (actual.num.spectra != num.spectra) {
    warning("Requested ", num.spectra, "; acquired ", actual.num.spectra, " spectra")
    num.spectra <- actual.num.spectra
  }

  # retrieve the spectra and time stamps one by one
  zz <- list()
  # OmniDriver uses indexes starting at zero
  first.time.stamp <- rOmniDriver::highSpdAcq_get_time_stamp(y$w, 0L)
  for (i in 1:num.spectra) {
    z[["counts"]] <- rOmniDriver::highSpdAcq_get_spectrum(y$w, i - 1L)
    this.timestamp <- rOmniDriver::highSpdAcq_get_time_stamp(y$w, i - 1L)
    # we need to compute the difference in seconds or milliseconds using API functions
    delta.time <- rOmniDriver::get_seconds_time_delta(first.time.stamp, this.timestamp)
    this.acq.time <- start.time + lubridate::seconds(delta.time)
    z <- as.raw_spct(z)
    attr(z, "linearized") <- x$corr.sensor.nl
    photobiology::setInstrDesc(z, y)
    # we remove the Java wrapper so that RJava is not required when reading the data
    photobiology::trimInstrDesc(z, c("-", "w"))
    photobiology::setInstrSettings(z, x)
    photobiology::setWhenMeasured(z, this.acq.time)
    photobiology::setWhereMeasured(z, where.measured)
    photobiology::setWhatMeasured(z, what.measured)
    if (!is.raw_spct(z)) {
      warning("Acquired spectrum '", i, "' is invalid.")
      zz[[i]] <- raw_spct() # empty object
    } else {
      zz[[i]] <- z
    }
  }

  if (!is.null(base.name)) {
    # vectorized assembly of names for individual spectra
    names(zz) <- paste(base.name, format_idx(1:num.spectra), sep = ".")
  }

  if (return.list) {
    zz
  } else {
    photobiology::as.raw_mspct(zz)
  }
}
