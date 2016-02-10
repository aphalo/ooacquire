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
    nl.poly <- polynom::polynomial(nl.poly)
    z$nl.fun <- polynom::as.function(nl.poly)
    # stray light
    z$straylight.coeff <- calib.data$getStrayLight()
    z$straylight.slope <- calib.data$getStrayLightSlope()
    # wavelength calibration
    wl.poly <- calib.data$getWlCoefficients()
    wl.poly <- polynom::polynomial(wl.poly)
    z$wl.fun <- polynom::as.function(wl.poly)
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
  oo_descriptor$inst.calib$wavelengths <- wl
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
