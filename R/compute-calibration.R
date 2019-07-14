#' Compute calibration multipliers.
#'
#' Function to calculate from a set of three (or optionally five or two) output
#' files from the spectrometer, as produced by SpectraSuite, for each of two
#' calibration lamps (D2 and FEL), and calibration data for the lamps, a set of
#' calibration multipliers for the instrument. This function applies a
#' non-linearity correction, an slit-function correction, and stray-light
#' correction, and optionally smoothing by means of process_maya_arrays().
#' Optionaly a HDR ("high dynamic range") merging of spectra (based on
#' integration time bracketing) using two different integration times is done.
#'
#' @param FEL.raw.counts source_mscpt The raw counts and integration time data
#'   for the FEL light source.
#' @param D2.raw.counts source_mscpt The raw counts and integration time data
#'   for the D2 light source.
#' @param pix.wavelengths numeric vector The "true" wavelengths in nanometres at
#'   each pixel in the detector array.
#' @param wl.range numeric vector of length two Range of wavelengths for which
#'   to compute the calibration.
#' @param FEL.k numeric vector a numeric vector with n constants for the
#'   polynomial for the FEL lamp.
#' @param D2.k numeric vector a numeric vector with n constants for the
#'   polynomial for the D2 lamp.
#' @param method list Method variant to be used.
#' @param verbose Logical indicating the level of warnings wanted. Defaults to
#'   \code{FALSE}.
#'
#' @return An instrument descriptor as a list containing the updated wavelengths
#'   and multipliers from the calibration.
#'
#' @note To calculate new multipliers we set the calibration multipliers equal
#'   to 1. Do the calculations as usual and calculate new multipliers based on
#'   the known spectral irradiance for the calibration lamps.
#' @export
#'
compute_irrad_calibration <-
  function(FEL.raw.counts,
           D2.raw.counts,
           pix.wavelengths = NULL,
           wl.range = c(250, 900),
           FEL.k,
           D2.k,
           method,
           verbose = getOption("photobiology.verbose", default = FALSE))
{
    # we use as dark reference only pixels that are physically blocked from
    # receiving radiation. For the Maya2000 Pro, the first 4 pixels in the
    # array detector.
    method[["flt.ref.wl"]] <- range(method[["wavelengths"]][1:4])

    FEL_spct <- s_irrad_corrected(FEL.raw.counts,
                                  method = method,
                                  return.cps = TRUE,
                                  verbose = verbose)

    D2_spct <- s_irrad_corrected(D2.raw.counts,
                                 method = method,
                                 return.cps = TRUE,
                                 verbose = verbose)

    when.measured <- getWhenMeasured(FEL_spct)

    FEL_descriptor <- getInstrDesc(FEL_spct)
    D2_descriptor <- getInstrDesc(D2_spct)
    stopifnot(FEL_descriptor[["spectrometer.sn"]] == D2_descriptor[["spectrometer.sn"]])

    # we use wavelengths from data unless new ones are supplied
    if (!is.null(pix.wavelengths)) {
      FEL_spct[["w.length"]] <- pix.wavelengths
      D2_spct[["w.length"]] <- pix.wavelengths
    } else {
      warning("Using 'w.length' from measured spectrum for calibration.")
      pix.wavelengths <- FEL_spct[["w.length"]]
    }

    within_range <- pix.wavelengths >= wl.range[1] &
                       pix.wavelengths <= wl.range[2]

    FEL_calib_spct <- photobiology::FEL_spectrum(pix.wavelengths, FEL.k, fill = NA)
    FEL_mult <- ifelse(within_range,
                       FEL_calib_spct[["s.e.irrad"]][within_range] /
                         FEL_spct[["cps"]][within_range], NA)

    D2_calib_spct <- photobiology::D2_spectrum(pix.wavelengths, D2.k, fill = NA)
    D2_mult <- ifelse(within_range,
                      D2_calib_spct[["s.e.irrad"]][within_range] /
                        D2_spct[["cps"]][within_range], NA)

    ovelaping <- pix.wavelengths >= 330 & pix.wavelengths <= 360

    D2_corr_factor <- mean(FEL_mult[ovelaping] / D2_mult[ovelaping], na.rm=TRUE)

    D2_mult <- D2_mult * D2_corr_factor

    irrad_mult <- rep(0, length.out = length(pix.wavelengths))
    irrad_mult[within_range] <-
      ifelse(pix.wavelengths[within_range] <= 340,
             D2_mult[within_range],
             FEL_mult[within_range])

    # We update the instrument descriptor with the new calibration
    updated_descriptor <- FEL_descriptor
    updated_descriptor <-
      set_descriptor_wl(descriptor = updated_descriptor,
                        wl = pix.wavelengths)
    updated_descriptor <-
      set_descriptor_irrad_mult(descriptor = updated_descriptor,
                                irrad.mult = irrad_mult,
                                wl.range = wl.range,
                                start.date = when.measured,
                                end.date = when.measured + lubridate::years(2))
    comment(updated_descriptor) <-
      paste("'ooacquire' version ", utils::packageVersion("ooacquire"),
            " on ", lubridate::now(), "\n\nFEL spectrum:\n", comment(FEL_spct),
            "\n\nD2 spectrum:\n", comment(D2_spct),
            sep="")

    return(updated_descriptor)
}


