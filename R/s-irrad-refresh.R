#' Recompute spectral irradiance
#'
#' Recompute spectral irradiance using the instrument descriptor
#' for the same spectrometer and date of measurement using the
#' calibration data in the current version of 'ooacquire' rather
#' than that available at the time tha raw data were acquired.
#'
#' @inheritParams s_irrad_corrected x
#' @param old.spct source_spct Object from which to copy metadata.
#'
#' @export
#'
#' @examples
#'
#' # original metadata only
#' s_irrad_refresh(white_LED.raw_mspct)
#'
#' # added or edited metadata present in white_led.source_spct restored
#' s_irrad_refresh(white_LED.raw_mspct, white_led.source_spct)
#'
s_irrad_refresh <- function(x,
                            old.spct = NULL) {
  old.descriptor <- photobiology::instr_descriptor(x[[1]])
  entrance.optics <- "cosine" # !!!
  current.defaults <-
    spectrometer_defaults(serial_no = old.descriptor$spectrometer.sn,
                          entrance.optics = entrance.optics,
                          data.acq.date = photobiology::when_measured(x[[1]]))

  photobiology::instr_descriptor(x) <- current.defaults$descriptor
  z <-
    s_irrad_corrected(x = x,
                      correction.method =
                        current.defaults$correction.method,
                      return.cps =
                        !is.null(old.spct) &&
                        photobiology::is.cps_spct(old.spct))
  if (!is.null(old.spct)) {
    z <- photobiology::copy_attributes(old.spct, z,
                                       which.not = c("instr.desc",
                                                     "instr.settings",
                                                     "spct.version"))
  } else {
    warning("No metadata copied as no argument was passed to 'old.spct'")
  }
  z
}
