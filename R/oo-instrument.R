#' Get the instrument description and EEPROM data
#'
#' Model, configuration, serial number, and calibration data stored in the
#' EEPROM of an Ocean Optics spectrometer are retrieved and returned in a
#' list.
#'
#' @param w an open Wrapper object from OmniDriver.
#' @param sr.index an index to address the spectrometer for the time being not
#'   exported.
#' @param ch.index an index to address the channel in a spectrometer with more
#'   than one channel.
#' @param area numeric Passed to \code{o_calib2irrad_mult()}.
#' @param diff.type character Passed to \code{o_calib2irrad_mult()}.
#'
#' @export
#' @return a list
#'
#' @note One and only one of \code{area} or \code{diff.type} is needed if an
#'   irradiance calibration stored in the EEPROM is to be retrieved. If both
#'   are null, as by default, the irradiance calibration factors will not be
#'   retireved even if present in the EEPROM.
#'
get_oo_descriptor <- function(w,
                              sr.index = 0L,
                              ch.index = 0L,
                              area = NULL,
                              diff.type = NULL) {

  get_calib <- function(w, sr.index, ch.index, w.lengths) {
    z <- list()
    inst.calib <-
      rOmniDriver::get_calibration_coefficients_from_buffer(jwrapper = w,
                                                            sr.index = sr.index,
                                                            ch.index = ch.index)
    # linearization
    oo.nl.coeff <- inst.calib$getNlCoefficients()
    z$nl.coeff <- oo.nl.coeff
    oo.nl.poly <- polynom::polynomial(oo.nl.coeff)
    oo.nl.fun <- as.function(oo.nl.poly)
    z$nl.fun <- compiler::cmpfun(function(x) {x / oo.nl.fun(x)})
    # stray light
    z$straylight.coeff <- inst.calib$getStrayLight()
    z$straylight.slope <- inst.calib$getStrayLightSlope()
    # wavelength calibration
    wl.coeff <- inst.calib$getWlCoefficients()
    z$wl.coeff
    wl.poly <- polynom::polynomial(wl.coeff)
    z$wl.fun <- as.function(wl.poly)
    # slit function
    z$slit.fun <- NA
    # irradiance calibration factors
    if (xor(is.null(area), is.null(diff.type)) &&
      rOmniDriver::is_feature_supported_irradiance_calibration_factor(jwrapper = w,
                                                                        sr.index = sr.index)) {
      irrad.calib.factor <-
        rOmniDriver::get_feature_irradiance_calibration_factor(jwrapper = w,
                                                               sr.index = sr.index)
      # Even if feature is supported data may be unavailable, in which case
      # the call may not return unless a timeout is set in the spectrometer.
      # The timeout value seems to be irrelevant, so we set it to 0.5 s.
      rOmniDriver::set_USB_timeout(jwrapper = w,
                                   time.millisec = 500L,
                                   sr.index = 0L)
      oo.calib <- try(irrad.calib.factor$getIrradianceCalibrationFactors(),
                      silent = TRUE)
      if (inherits(oo.calib, "try-error")) {
        warning("Unable to read irradiance calibration from spectrometer",
                call. = FALSE)
        z$irrad.mult <- rep(NA_real_, length(w.lengths))
        z$start.date <- NA_real_
        z$end.date <- NA_real_
      } else {
        z$irrad.mult <- oo_calib2irrad_mult(oo.calib,
                                            area = area,
                                            diff.type = diff.type)
        z$start.date <- lubridate::today() - lubridate::days(1)
        z$end.date <- lubridate::today() + lubridate::days(30)
      }
    } else {
      z$irrad.mult <- rep(NA_real_, length(w.lengths))
      z$start.date <- NA_real_
      z$end.date <- NA_real_
    }
    z
  }

  bench <- rOmniDriver::get_bench(jwrapper = w,
                                  sr.index = sr.index,
                                  ch.index = ch.index)
  w.lengths <- rOmniDriver::get_wavelengths(jwrapper = w,
                                            sr.index = sr.index,
                                            ch.index = ch.index)
  list(
    time = lubridate::now(),
    w = w,
    sr.index = sr.index,
    ch.index = ch.index,
    spectrometer.name = rOmniDriver::get_name(jwrapper = w,
                                              sr.index = sr.index),
    spectrometer.sn =  rOmniDriver::get_serial_number(jwrapper = w,
                                                      sr.index = sr.index),
    bench.grating = ifelse(!methods::is(bench, "jobjRef"),
                           NA_character_, bench$getGrating()),
    bench.filter = ifelse(!!methods::is(bench, "jobjRef"),
                          NA_character_, bench$getFilterWavelength()),
    bench.slit = ifelse(!!methods::is(bench, "jobjRef"),
                        NA_character_, bench$getSlitSize()),
    num.pixs = rOmniDriver::get_number_of_pixels(jwrapper = w,
                                                 sr.index = sr.index,
                                                 ch.index = ch.index),
    num.dark.pixs = rOmniDriver::get_number_of_dark_pixels(jwrapper = w,
                                                           sr.index = sr.index,
                                                           ch.index = ch.index),
    min.integ.time = rOmniDriver::get_minimum_integration_time(jwrapper = w,
                                                               sr.index = sr.index),
    max.integ.time = rOmniDriver::get_maximum_integration_time(jwrapper = w,
                                                               sr.index = sr.index),
    max.counts = rOmniDriver::get_maximum_intensity(jwrapper = w,
                                                    sr.index = sr.index),
    wavelengths = w.lengths,
    wl.range = range(w.lengths),
    bad.pixs = numeric(),
    inst.calib = get_calib(w = w,
                           sr.index = sr.index,
                           ch.index = ch.index,
                           w.lengths = w.lengths)
  )
}

#' Add bad pixel information to an instrument description
#'
#' A integer vector of indexes to bad pixels in the instrument array. Data from
#' these array pixels will be discarded.
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor}
#' @param spct an object of class \code{generic_spct} or derived.
#' @param mspct an object of class \code{generic_mspct} or derived.
#' @param bad.pixs numeric vector of positional indexes corresponding to
#'   individual pixels in the instrument array, or an object of class
#'   \code{"instr_desc"} from where to copy the \code{bad.pixs} member.
#'
#' @note  Following R's syntax the first pixel in the detector array has index
#'  1.
#'
#' @return A copy of the argument passed to \code{descriptor}, \code{spct} or
#'   \code{mspct} with the descriptor with indexes to bad pixels set to the
#'   new values.
#'
#' @export
#'
set_descriptor_bad_pixs <- function(descriptor,
                                    bad.pixs) {
  # support copy from descriptor to descriptor
  if (inherits(bad.pixs, "instr_desc")) {
    bad.pixs <- bad.pixs[["bad.pixs"]]
  }
  # validate user input
  bad.pixs <- as.integer(bad.pixs)
  bad.pixs <- unique(sort(bad.pixs))
  stopifnot(min(bad.pixs) >= 1 &&
              max(bad.pixs) <= length(descriptor$wavelengths))
  descriptor$bad.pixs <- bad.pixs
  descriptor
}

#' @rdname set_descriptor_bad_pixs
#'
#' @export
#'
update_spct_bad_pixs <- function(spct,
                                 bad.pixs) {
  descriptor <- photobiology::getInstrDesc(x = spct)
  if (is.na(descriptor[["spectrometer.name"]]) ||
      is.na(descriptor[["spectrometer.sn"]])) {
    warning("Skipping... the spectrum has an invalid or missing descriptor.")
    return(spct)
  }
  descriptor <- set_descriptor_bad_pixs(descriptor = descriptor,
                                        bad.pixs = bad.pixs)
  photobiology::setInstrDesc(x = spct, instr.desc = descriptor)
}

#' @rdname set_descriptor_bad_pixs
#'
#' @export
#'
update_mspct_bad_pixs <- function(mspct,
                                  bad.pixs) {
  photobiology::msmsply(mspct = mspct,
                        .fun = update_spct_bad_pixs,
                        bad.pixs = bad.pixs)
}

#' Replace integration time limits in instrument descriptor
#'
#' This function can be needed in exceptional cases such as when the limits
#' stored in the intrument's persistent memory are wrong. In other cases in can
#' be used to restrict the range of values allowed to be set to a smaller range
#' than natively supported by the spectrometer.
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor}
#' @param min.integ.time,max.integ.time numeric values in seconds in seconds.
#' @param force.change If FALSE values outside the range returned by a query
#' to the instrument trigger an error. If TRUE this test is overriden.
#'
#' @return a copy of the argument passed for \code{oo_descriptor} with the
#' integration time fields of the descriptor modified.
#'
#' @note This function should not be needed, but for some instruments the query
#' may fail or return the wrong value. Values should be within the range
#' in the instrument's specifications. Setting wrong values can result in
#' invalid data without an error being triggered.
#'
#' @export
#'
set_descriptor_integ_time <- function(descriptor,
                                      min.integ.time = NA_integer_,
                                      max.integ.time = NA_integer_,
                                      force.change = FALSE) {
  # validate user input
  min.integ.time <- as.integer(min.integ.time * 1e6)
  max.integ.time <- as.integer(max.integ.time * 1e6)
  if (!is.na(min.integ.time)) {
    if (!force.change) {
      stopifnot(min.integ.time >=
        rOmniDriver::get_minimum_integration_time(
          descriptor$w, descriptor$sr.index))
    }
    descriptor$min.integ.time <- min.integ.time
  }
  if (!is.na(max.integ.time)) {
    if (!force.change) {
      if (getOption("ooacquire.offline", TRUE)) {
        warning("Off-line: maximum integration time validation skipped.")
        return(raw_spct())
      } else {
        stopifnot(max.integ.time <=
                    rOmniDriver::get_maximum_integration_time(
                      descriptor$w, descriptor$sr.index))
      }
    }
    descriptor$max.integ.time <- max.integ.time
  }
  descriptor
}

#' Replace wavelength values in an instrument description
#'
#' Replace wavelength values in an instrument descriptor for an Ocean Optics
#' spectrometer with new values. (e.g. when wavelngth calibration is not stored
#' in firmware).
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor}
#' @param wl numeric vector of sorted wavelengths values corresponding to each
#' pixel in the instrument array.
#'
#' @return a copy of the argument passed for \code{oo_descriptor} with the
#' wavelengths field of the calibration data replaced by the new values.
#'
#' @export
#'
set_descriptor_wl <- function(descriptor,
                              wl) {
  if (exists("wavelengths", descriptor)) {
      old.wl <- descriptor[["wavelengths"]]
      stopifnot((is.null(old.wl) || length(old.wl) == length(wl)))
  }
  stopifnot(!is.unsorted(wl, strictly = TRUE))
  descriptor[["wavelengths"]] <- wl
  descriptor
}

#' Replace linearization function in instrument description.
#'
#' Uses a user supplied function, possibly that supplied by a manufacturer
#' like Ocean Optics stored in firmware or in any other form.
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor}
#' @param nl.coeff numeric vector, if nl.fun is missing, assumed to be a
#'   polynomial.
#' @param nl.fun A function or a polynom::polynomial object containing
#'   the linearization to be applied.
#'
#' @return A copy of the argument passed for \code{oo_descriptor} with the
#' wavelengths field of the calibration data replaced by the new values.
#'
#' @export
#'
set_descriptor_nl <- function(descriptor,
                              nl.coeff = NA_real_,
                              nl.fun = NULL)
{
  stopifnot(!is.null(nl.coeff) || !is.null(nl.fun))

  # if a vector of polynomial coefficients is supplied, but no function we
  # build a polynomial from it
  if (is.null(nl.fun)) {
    nl.fun <- polynom::polynomial(nl.coeff)
  }

  # if polynomial supplied instead of function we convert it
  if (polynom::is.polynomial(nl.fun)) {
    nl.fun <- as.function(nl.fun)
  }
  stopifnot(is.function(nl.fun))
  descriptor[["inst.calib"]][["nl.coeff"]] <- nl.coeff
  descriptor[["inst.calib"]][["nl.fun"]] <- function(x) {x / nl.fun(x)}
  descriptor
}

#' Add spectral irradiance calibration
#'
#' Adds calibration data expressed as multipliers for each pixel stores in a
#' numeric vector.
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor}
#' @param irrad.mult numeric vector of the same length as the number of pixels
#'   or of length one.
#' @param wl.range numeric Range of wavelengths for which the calibration is
#'   valid.
#' @param start.date,end.date range of dates when calibration is valid.
#'
#' @return A copy of the argument passed for \code{oo_descriptor} with the
#'   irrad.mult field of the calibration data replaced by the new values.
#'
#' @export
#'
set_descriptor_irrad_mult <- function(descriptor,
                                      irrad.mult,
                                      wl.range = NULL,
                                      start.date = lubridate::today() - lubridate::days(1),
                                      end.date = lubridate::today() + lubridate::days(1))
{
  stopifnot(is.numeric(irrad.mult) && length(irrad.mult) == 1 ||
              length(irrad.mult) == length(descriptor$wavelengths))
  if (length(irrad.mult) == 1) {
    warning("'irrad.mult' of length one will be recycled.")
  }
  descriptor[["inst.calib"]][["irrad.mult"]] <- irrad.mult
  if (!is.null(range)) {
    descriptor[["inst.calib"]][["wl.range"]] <- range(wl.range)
  }
  descriptor[["inst.calib"]][["start.date"]] <- start.date
  descriptor[["inst.calib"]][["end.date"]] <- end.date
  descriptor
}

#' Get the current values of instrument settings
#'
#' Query the spectrometer for the settings currently in use for corrections,
#' smotthing and acquisition parameters integration time and number of scans.
#'
#' @param descriptor list as returned by function \code{get_oo_descriptor}
#'
#' @export
#' @return a list
#'
get_oo_settings <- function(descriptor) {
  if (getOption("ooacquire.offline", TRUE)) {
    stop("Off-line: spectrometer settings unavailable.")
  }
  w <- descriptor$w
  sr.index <- descriptor$sr.index
  ch.index <- descriptor$sr.index
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

