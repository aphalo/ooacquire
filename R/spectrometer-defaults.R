#' Fetch spectrometer defaults
#'
#' Find default instrument descriptor, correction method, and default
#' measuring protocol based on spectrometer serial number and entrance
#' optics.
#'
#' @details When any of \code{protocols}, \code{descriptor} or
#' \code{correction.method} is not available (\code{NA}), the
#' missing value is replaced by a default based on the arguments
#' passed to \code{serial_no}, \code{entrance.optics}, and
#' \code{protocols}.
#'
#' @param serial_no character The serial number of an Ocean Optics spectrometer
#'   for which data is available in 'ooacquire'.
#' @param data.acq.date Any object that anytime::anydate() will decode as a date
#'   or convert to a date. Used to select a descriptor containing calibration
#'   data valid for a given day.
#' @inheritParams acq_irrad_interactive
#'
#' @examples
#'
#' str(spectrometer_defaults(serial_no = "MAYP114590"))
#'
#' @export
#'
spectrometer_defaults <-
  function(serial_no,
           entrance.optics = "cosine",
           data.acq.date = lubridate::now(tzone = "UTC"),
           descriptors = NA,
           correction.method = NA) {

    # spectrometer-specific correction method parameters
    if (anyNA(c(descriptors[[1]]))) {
      descriptor <-
        switch(serial_no,
               MAYP11278 =
                 which_descriptor(date = data.acq.date,
                                  descriptors = ooacquire::MAYP11278_descriptors,
                                  entrance.optics = entrance.optics),
               MAYP112785 =
                 which_descriptor(date = data.acq.date,
                                  descriptors = ooacquire::MAYP112785_descriptors),
               MAYP114590 =
                 which_descriptor(date = data.acq.date,
                                  descriptors = ooacquire::MAYP114590_descriptors),
               FLMS04133 =
                 which_descriptor(date = data.acq.date,
                                  descriptors = ooacquire::FLMS04133_descriptors),
               FLMS00673 =
                 which_descriptor(date = data.acq.date,
                                  descriptors = ooacquire::FLMS00673_descriptors),
               FLMS00440 =
                 which_descriptor(date = data.acq.date,
                                  descriptors = ooacquire::FLMS00440_descriptors),
               FLMS00416 =
                 which_descriptor(date = data.acq.date,
                                  descriptors = ooacquire::FLMS00416_descriptors),
               warning("No instrument descriptor found in 'ooacquire' for ",
                       "spectrometer with s/n ='", serial_no, "'!")
        )
    }
    if (anyNA(correction.method[[1]])) {

      correction.method <-
        switch(serial_no,
               MAYP11278 = ooacquire::MAYP11278_ylianttila.mthd,
               MAYP112785 = ooacquire::MAYP112785_ylianttila.mthd,
               MAYP114590 = ooacquire::MAYP114590_simple.mthd,
               FLMS04133 = ooacquire::FLMS04133_none.mthd,
               FLMS00673 = ooacquire::FLMS00673_none.mthd,
               FLMS00440 = ooacquire::FLMS00440_none.mthd,
               FLMS00416 = ooacquire::FLMS00416_none.mthd,
               {
                 warning(
                   "No spectrometer-specific method found, using a generic one",
                   call. = FALSE)
                 new_correction_method(descriptor,
                                       stray.light.method = "none")
               }
        )
    }
    list(descriptor = descriptor,
         correction.method = correction.method)
  }

