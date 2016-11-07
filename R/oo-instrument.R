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
  get_calib <- function() {
    z <- list()
    inst.calib <-
      rOmniDriver::get_calibration_coefficients_from_buffer(w, sr.index, ch.index)
    # linearization
    oo.nl.poly <- inst.calib$getNlCoefficients()
    oo.nl.poly <- polynom::polynomial(oo.nl.poly)
    oo.nl.fun <- as.function(oo.nl.poly)
    nl.fun <- function(x) {x / oo.nl.fun(x)}
    z$nl.fun <- nl.fun
    # stray light
    z$straylight.coeff <- inst.calib$getStrayLight()
    z$straylight.slope <- inst.calib$getStrayLightSlope()
    # wavelength calibration
    wl.poly <- inst.calib$getWlCoefficients()
    wl.poly <- polynom::polynomial(wl.poly)
    z$wl.fun <- as.function(wl.poly)
    # slit function
    z$slit.fun <- NA
    # irradiance calibration factors
    if (rOmniDriver::is_feature_supported_irradiance_calibration_factor(w, sr.index)) {
      z$irrad.mult <-
        rOmniDriver::get_feature_irradiance_calibration_factor(w, sr.index)$getIrradianceCalibrationFactors()
      z$start.date <- lubridate::today() - lubridate::days(1)
      z$end.date <- lubridate::today() + lubridate::days(1)
    } else {
      z$irrad.mult <- NA_real_
      z$start.date <- NA_real_
      z$end.date <- NA_real_
    }
  }

  bench <- rOmniDriver::get_bench(w, sr.index, ch.index)
  w.lengths <- rOmniDriver::get_wavelengths(w, sr.index, ch.index)
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
    wavelengths = w.lengths,
    wl.range = range(w.lengths),
    bad.pixs = numeric(),
    inst.calib = get_calib()
  )
}

#' Add bad pixel informatoin to an instrument description
#'
#' A integer vector of indexes to bad pixels in the instrument array. Data from
#' these array pixels will be discarded.
#'
#' @param oo_descriptor list as returned by function \code{get_oo_descriptor}
#' @param bad.pixs numeric vector of sorted wavelengths values corresponding to each
#' pixel in the instrument array.
#'
#' @return a copy of the argument passed for \code{oo_descriptor} with the
#' wavelengths field of the calibration data replaced by the new values.
#'
#' @export
#'
set_descriptor_bad_pixs <- function(oo_descriptor,
                                    bad.pixs) {
  # validate user input
  bad.pixs <- as.integer(bad.pixs)
  bad.pixs <- unique(sort(bad.pixs))
  stopifnot(min(bad.pixs) >= 1 &&
              max(bad.pixs) <= length(oo_descriptor$wavelengths))
  oo_descriptor$bad.pixs <- bad.pixs
  oo_descriptor
}

#' Replace integration time limits in instrument descriptor
#'
#' This function can be needed in exceptional cases such as when the instrument
#' the limits stored in the intrument firmware are wrong. In other cases in can
#' be used to limit the range of values allowed to be set.
#'
#' @param oo_descriptor list as returned by function \code{get_oo_descriptor}
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
set_descriptor_integ_time <- function(oo_descriptor,
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
          oo_descriptor$w, oo_descriptor$sr.index))
    }
    oo_descriptor$min.integ.time <- min.integ.time
  }
  if (!is.na(max.integ.time)) {
    if (!force.change) {
      stopifnot(max.integ.time <=
                  rOmniDriver::get_maximum_integration_time(
                    oo_descriptor$w, oo_descriptor$sr.index))
    }
    oo_descriptor$max.integ.time <- max.integ.time
  }
  oo_descriptor
}

#' Replace wavelength values in an instrument description
#'
#' Replace wavelength values in an instrument descriptor for an Ocean Optics
#' spectrometer with new values if valid. (e.g. when wavelngth calibration
#' is not stored in firmware).
#'
#' @param oo_descriptor list as returned by function \code{get_oo_descriptor}
#' @param wl numeric vector of sorted wavelengths values corresponding to each
#' pixel in the instrument array.
#'
#' @return a copy of the argument passed for \code{oo_descriptor} with the
#' wavelengths field of the calibration data replaced by the new values.
#'
#' @export
#'
set_descriptor_wl <- function(oo_descriptor,
                              wl) {
  old.wl <- oo_descriptor[["inst.calib"]][["wavelengths"]]
  stopifnot((is.null(old.wl) || length(old.wl) == length(wl)) &&
              !is.unsorted(wl, strictly = TRUE))
  oo_descriptor[["wavelengths"]] <- wl
  oo_descriptor
}

#' Replace linearization function in instrument description.
#'
#' Uses a user supplied function, possibly that supplied by a manufacturer
#' like Ocean Optics stored in firmware or in any other form.
#'
#' @param oo_descriptor list as returned by function \code{get_oo_descriptor}
#' @param nl.fun A function or a polynom::polynomial object containing
#'   the linearization to be applied.
#'
#' @return A copy of the argument passed for \code{oo_descriptor} with the
#' wavelengths field of the calibration data replaced by the new values.
#'
#' @export
#'
set_descriptor_nl <- function(oo_descriptor,
                              nl.fun)
{
  # if polynomial supplied instead of function we convert it
  if (polynom::is.polynomial(nl.fun)) {
    nl.fun <- as.function(nl.fun)
  }
  stopifnot(is.function(nl.fun))
  oo_descriptor[["inst.calib"]][["nl.fun"]] <- nl.fun
  oo_descriptor
}

#' Add spectral irradiance calibration
#'
#' Adds calibration data for each pixel as a numeric vector
#'
#' @param oo_descriptor list as returned by function \code{get_oo_descriptor}
#' @param irrad.mult numeric vector of the same length as the number of pixels
#'   or of length one.
#' @param wl.range numeric Range of wavelengths for which the calibration is
#'   valid.
#' @param start.date,end.date range of dates when calibration is valid.
#'
#' @return A copy of the argument passed for \code{oo_descriptor} with the
#' irrad.mult field of the calibration data replaced by the new values.
#'
#' @export
#'
set_descriptor_irrad_mult <- function(oo_descriptor,
                                      irrad.mult,
                                      wl.range = NULL,
                                      start.date = lubridate::today() - lubridate::days(1),
                                      end.date = lubridate::today() + lubridate::days(1))
{
  stopifnot(length(irrad.mult) == 1 ||
              length(irrad.mult) == length(oo_descriptor$wavelengths))
  if (length(irrad.mult) == 1) {
    warning("'irrad.mult' of length one will be recycled.")
  }
  oo_descriptor[["inst.calib"]][["irrad.mult"]] <- irrad.mult
  if (!is.null(range)) {
    oo_descriptor[["inst.calib"]][["wl.range"]] <- range(wl.range)
  }
  oo_descriptor[["inst.calib"]][["start.date"]] <- start.date
  oo_descriptor[["inst.calib"]][["end.date"]] <- end.date
  oo_descriptor
}

#' Add function for slit correction
#'
#' Adds a function to the instrument descriptor that can be used to
#' correct counts per second data to remove the effect of the tails of
#' the slit function of the instrument.
#'
#' @param oo_descriptor list as returned by function \code{get_oo_descriptor}
#' @param inv.slit.fun function with two first formal parameters taking numeric
#'   vectors of wavelengths and counts that aoolies a correction suitable
#'   for the instrument.
#'
#' @return A copy of the argument passed for \code{oo_descriptor} with the
#' irrad.mult field of the calibration data replaced by the new values.
#'
#' @export
#'
set_descriptor_slit_fun <- function(oo_descriptor,
                                    inv.slit.fun)
{
  stopifnot(is.function(inv.slit.fun))
  oo_descriptor[["inst.calib"]][["$slit.fun"]] <- inv.slit.fun
  oo_descriptor
}

#' Get the current values of instrument settings
#'
#' Query the spectrometer for the settings currently in use for corrections,
#' smotthing and acquisition parameters integration time and number of scans.
#'
#' @param oo_descriptor list as returned by function \code{get_oo_descriptor}
#'
#' @export
#' @return a list
#'
get_oo_settings <- function(oo_descriptor) {
  w <- oo_descriptor$w
  sr.index <- oo_descriptor$sr.index
  ch.index <- oo_descriptor$sr.index
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

