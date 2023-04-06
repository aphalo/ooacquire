#' Measure spectra at high speed
#'
#' Take one set of spectra using the same instruments settings at high speed
#' using the special OmniDriver API functions. No HDR bracketing is possible,
#' and synchorization with a pulsed light source is not supported.
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor}.
#' @param acq.settings list as returned by functions \code{tune_acq_settings}.
#' @param num.spectra integer Number of individual spectra to acquire.
#' @param base.name character The name given to individual spectra is formed
#'   by this string followed by a sequential numeric index.
#' @param f.trigger.pulses function Function to be called to trigger an
#'   action. Should accept as its only argument the number of pulses, and
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
#' @return A \code{raw_mspct} containing one \code{raw_spct} object with one
#'   column \code{w.length} and one column \code{counts} for each spectrum. The
#'   number of columns with raw counts is is always one and integration time
#'   bracketing or HDR values in \code{acq.settings} are ignored except for the
#'   smallest value.
#'
#' @seealso For normal speed acquisition of a single spectrum and multiple
#'   spectra according to a user defined protocol see \code{\link{acq_raw_spct}}
#'   and \code{\link{acq_raw_mspct}}.
#'
hs_acq_raw_spct <- function(descriptor,
                            acq.settings,
                            num.spectra = 100L,
                            base.name = "light",
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
  if (length(x$integ.time) > 1L) {
    warning("No bracketing or HDR supported at high speed: using smallest value.")
    x$integ.time <- as.integer(min(x$integ.time)) # integer microseconds
  }

  num.readings <- 1L #

  num.exposures <- -1L # unsupported

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

  # acquire a batch of spectra

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

  rOmniDriver::set_scans_to_avg(y$w, 1L, y$sr.index, y$ch.index)
  actual.num.scans <- rOmniDriver::get_scans_to_avg(y$w, y$sr.index, y$ch.index)
  # We need to
  if (x$num.scans[i] != actual.num.scans) {
    # We guard against failure to set integration time
    # It should never happen as we check validity value requested
    warning("The spectrometer has overridden the number of scans!")
    x$num.scans[i] <- actual.num.scans
  }

  # setup memoery buffer
  rOmniDriver::highSpdAcq_allocate_buffer(y$w, y$sr.index, num.spectra)

  if (verbose) message("Scans 1 to ", num.spectra, " ... ", appendLF = FALSE)

  # start acquisition
  rOmniDriver::highSpdAcq_start_acquisition(y$w, y$sr.index)

  # retrieve actual number of spectra acquired
  actual.num.spectra <-
    rOmniDriver::highSpdAcq_get_number_of_spectra_acquired(y$w)

  if (actual.num.spectra != num.spectra) {
    warning("Requested ", num.spectra, "; acquired ", actual.num.spectra, " spectra")
    num.spectra <- actual.num.spectra
  }

  # vectorized assembly of names for individual spectra
  obj.names <- paste(base.name, format_idx(1:num.spectra), sep = ".")
  # retrieve the spectra and time stamps one by one
  zz <- list()
  for (i in 1:num.spectra) {
    counts <- rOmniDriver::highSpdAcq_get_spectrum(i)
    acq.time <- rOmniDriver::highSpdAcq_get_time_stamp(i)
    z <- tibble::tibble(w.length = y$wavelengths,
                        counts = counts)
    z <- as.raw_spct(z)
    attr(z, "linearized") <- x$corr.sensor.nl
    photobiology::setInstrDesc(z, y)
    # we remove the Java wrapper so that RJava is not required to read the data
    photobiology::trimInstrDesc(z, c("-", "w"))
    photobiology::setInstrSettings(z, x)
    photobiology::setWhenMeasured(z, acq.time)
    photobiology::setWhereMeasured(z, where.measured)
    photobiology::setWhatMeasured(z, what.measured)
    if (!is.raw_spct(z)) {
      warning("Acquired spectrum '", obj.names[i], "' is invalid, discarding it.")
      next()
    } else {
      zz[[obj.names[i]]] <- z
    }
  }
  # convert list into raw_mspct object
  as.raw_mspct(zz)
}
