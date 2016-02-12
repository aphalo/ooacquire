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
    bad.pixs = numeric(),
    inst.calib = get_calib_coeffs()
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
  old.wl <- oo_descriptor$inst.calib$wavelengths
  stopifnot(length(old.wl) == length(wl) &&
              !is.unsorted(wl, strictly = TRUE))
  oo_descriptor$wavelengths <- wl
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
  oo_descriptor$inst.calib$nl.fun <- nl.fun
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
