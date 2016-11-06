#' Compute calibration multipliers.
#'
#' Function to calculate from a set of three (or optionally five or two) output
#' files from the spectrometer, as produce by SpectraSuite, for each of two
#' calibration lamps (D2 and FEL), and calibration data for the lamps, a set of
#' calibration multipliers for the instrument. This function applies a
#' non-linearity correction, an slit-function correction, and stray-light
#' correction, and optionally smoothing by means of process_maya_arrays().
#' Optionaly a HDR ("high dynamic range") merging of spectra (based on
#' integration time bracketing) using two different integration times is done.
#'
#' @param FEL_file_meas  The path to the text file containing the raw counts and
#'   integration time data for the measurement of a light source.
#' @param FEL_file_dark The path to the text file containing the raw counts and
#'   integration time data for a dark reading.
#' @param FEL_file_PC Optionally ther path to the text file containing the raw
#'   counts and integration time for a reading of the light source with a
#'   polycarbonate filter. Improves the stray light correction when the light
#'   source has a wide spectrum (specially if IR radiation is present). If not
#'   suplied, this correction is skipped.
#' @param FEL_file_long_meas The path to the text file containing the raw counts
#'   and integration time data for the measurement of a light source using a
#'   longer integration time (usualy ten time the length of the integration time
#'   used for the main spectrum). This spectrum if present is merged to achieve
#'   "bracketing", similar to HDR photography. This improves the dynamic range.
#' @param FEL_file_long_dark The path to the text file containing the raw counts
#'   and integration time data for a dark reading using the longer integration
#'   time.
#' @param D2_file_meas  The path to the text file containing the raw counts and
#'   integration time data for the measurement of a light source.
#' @param D2_file_dark The path to the text file containing the raw counts and
#'   integration time data for a dark reading.
#' @param D2_file_PC Optionally ther path to the text file containing the raw
#'   counts and integration time for a reading of the light source with a
#'   polycarbonate filter. Improves the stray light correction when the light
#'   source has a wide spectrum (specially if IR radiation is present). If not
#'   suplied, this correction is skipped.
#' @param D2_file_long_meas The path to the text file containing the raw counts
#'   and integration time data for the measurement of a light source using a
#'   longer integration time (usualy ten time the length of the integration time
#'   used for the main spectrum). This spectrum if present is merged to achieve
#'   "bracketing", similar to HDR photography. This improves the dynamic range.
#' @param D2_file_long_dark The path to the text file containing the raw counts
#'   and integration time data for a dark reading using the longer integration
#'   time.
#' @param FEL_calib_file The path to the text file containing the calibration
#'   data for the FEL lamp.
#' @param D2_calib_file The path to the text file containing the calibration
#'   data for the deuterium lamp.
#' @param bad_pixels Numeric vector of indexes to pixels that should be ignored
#' @param method Method variant used "raw" or "raw_sm" at the moment, with "raw"
#'   as default.
#' @param smoothing_hi_lim a numerical value giving the high limit wavelength in
#'   nm for smoothing (defaults to 0.0, giving no smoothing)
#' @param smoothing_coef a numeric value for adjusting the amount of smmothing
#' @param verbose Logical indicating the level of warnings wanted. Defaults to
#'   \code{FALSE}.
#'
#' @return A dataframe is returned with columns "w.length", and
#'   "cal.multipliers". containing the wavelengths from the calibration file and
#'   the calibration multipliers needed to convert corrected counts into
#'   spectral (energy) irradiance values in W m-2 nm-1 if lamp calibration data
#'   is in these units. Additional variables are included in the dataframe only
#'   if \code{verbose=TRUE}.
#'   \describe{
#'   \item{w.length}{numeric array of
#'   wavelengths in nm}
#'   \item{cal.multipliers}{numeric array giving the spectral
#'   calibration}}
#'
#' @note Method "raw" is used which is a reimplementation of the calculations
#'   developed by Lasse Ylianttila (STUK, Finland), originally in Excel. Option
#'   of using adaptive smoothing was added by Pedro J. Aphalo, as well as the
#'   modified method "full", that uses a different algorithm for the
#'   filter-based stray light correction, and adaptive smoothing by default.
#'   Method "sun" is the same as "full" but trims the spectrum by setting all
#'   values outside the range 290-900 nm to zero before smoothing.
#' @author Pedro J. Aphalo
#' @export
#' @seealso See Also as \code{\link{process_maya_arrays}}.
#' @references \url{http://www.r4photobiology.info}
#' @keywords misc
#'
compute_calibration <-
  function(FEL_file_meas,
           FEL_file_dark,
           FEL_file_PC=NULL,
           FEL_file_long_meas=NULL,
           FEL_file_long_dark=NULL,
           D2_file_meas,
           D2_file_dark,
           D2_file_PC=NULL,
           D2_file_long_meas=NULL,
           D2_file_long_dark=NULL,
           FEL_calib_file,
           D2_calib_file,
           bad_pixels=NULL,
           method="original",
           smoothing_hi_lim=0.0,
           smoothing_coef=1.0,
           verbose=FALSE)
{

    FEL.data <- process_maya_files(FEL_file_meas,
                                   FEL_file_dark,
                                   FEL_file_PC,
                                   FEL_file_long_meas,
                                   FEL_file_long_dark,
                                   method=method,
                                   smoothing_hi_lim=smoothing_hi_lim,
                                   smoothing_coef=smoothing_coef,
                                   bad_pixels=bad_pixels,
                                   verbose=verbose)


    D2.data <- process_maya_files(D2_file_meas,
                                   D2_file_dark,
                                   D2_file_PC,
                                   D2_file_long_meas,
                                   D2_file_long_dark,
                                   method=method,
                                   smoothing_hi_lim=smoothing_hi_lim,
                                   smoothing_coef=smoothing_coef,
                                   bad_pixels=bad_pixels,
                                   verbose=verbose)

    FEL_calib.data <- read.table(file=FEL_calib_file, header=TRUE, colClasses="numeric")
    D2_calib.data <- read.table(file=D2_calib_file, header=TRUE, colClasses="numeric")

    calib.df <- dplyr::data_frame(pixel_idx = 1L:2068L,
                           w.length = FEL_calib.data[ , "w.length"],
                           FEL_cts = FEL.data[ , "s.e.irrad"],
                           D2_cts = D2.data[ , "s.e.irrad"],
                           FEL_s.irrad = FEL_calib.data[ , "s.e.irrad"],
                           D2_s.irrad = D2_calib.data[ , "s.e.irrad"])
    within_range <- with(calib.df, w.length >= 250 & w.length <= 900)
    #    setkey(calib.df, within_range)
    calib.df <- dplyr::mutate(calib.df,
                               FEL_mult = ifelse(within_range, FEL_s.irrad / FEL_cts, NA),
                               D2_mult_raw = ifelse(withinrange, D2_s.irrad / D2_cts, NA))
    D2_corr_factor <- with(calib.df[calib.df[["w.length"]] >= 330 &
                                      calib.df[["w.length"]] <= 360, ],
                           mean(FEL_mult / D2_mult_raw, na.rm=TRUE) )
    calib.df[["D2_corr_factor"]] <- NA
    calib.df[within_range==TRUE, "D2_corr_factor"] <- calib.df[[D2_]]
    with(calib.df[within_range==TRUE], D2_mult := D2_mult_raw * D2_corr_factor]
    calib.df[within_range==TRUE, cal.multiplier := ifelse(w.length <= 340, D2_mult, FEL_mult)]
    calib.df[within_range==FALSE, cal.multiplier := NA]
#    setkey(calib.df)
    if (!is.null(bad_pixels)) {
      calib.df[bad_pixels, cal.multiplier := NA]
    }

    if (!verbose) {
      calib.df <- calib.df[ , list(w.length, cal.multiplier)]
    }
#    setkey(calib.df, w.length)

    setattr(calib.df, "comment", paste("FEL:\n", comment(FEL.data), "\n\nDeuterium:\n", comment(D2.data), sep="\n"))

    return(calib.df)
}

#' Function to calculate calibration multipliers.
#'
#' Function to calculate from a set of three (or optionally five or two) output
#' files from the spectrometer, as produce by SpectraSuite, for each of two
#' calibration lamps (D2 and FEL), and calibration data for the lamps, a set of
#' calibration multipliers for the instrument. This function applies a
#' non-linearity correction, an slit-function correction, and stray-light
#' correction, and optionally smoothing by means of process_maya_arrays().
#' Optionaly a HDR ("high dynamic range") merging of spectra (based on
#' integration time bracketing) using two different integration times is done.
#'
#' @param cts.dt A data frame or a data table object with all spectrometer
#'   counts data and calibrated wavelengths
#' @param FEL_short_integration_time numeric (units?)
#' @param D2_short_integration_time numeric (units?)
#' @param FEL_long_integration_time numeric (units?)
#' @param FEL_constants Numeric array with the constants needed to calculate the
#'   FEL lamp spectrum.
#' @param D2_constants Numeric array with the constants needed to calculate the
#'   D2 lamp spectrum.
#' @param bad_pixels Numeric vector of indexes to pixels that should be ignored
#' @param method Method variant used "raw" or "raw_sm" at the moment, with "raw"
#'   as default.
#' @param smoothing_hi_lim a numerical value giving the high limit wavelength in
#'   nm for smoothing (defaults to 0.0, giving no smoothing)
#' @param smoothing_coef a numeric value for adjusting the amount of smmothing
#' @param serial_number a character string containing the serial number of the
#'   spectrometer
#' @param verbose Logical indicating the level of warnings wanted. Defaults to
#'   \code{FALSE}.
#'
#' @return A dataframe is returned with columns "w.length", and
#'   "cal.multipliers". containing the wavelengths from the calibration file and
#'   the calibration multipliers needed to convert corrected counts into
#'   spectral (energy) irradiance values in W m-2 nm-1 if lamp calibration data
#'   is in these units. Additional variables are included in the dataframe only
#'   if \code{verbose=TRUE}.
#'   \describe{
#'   \item{w.length}{numeric array of wavelengths in nm}
#'   \item{cal.multipliers}{numeric array giving the spectral calibration}}
#'
#' @note Method "raw" is used which is a reimplementation of the calculations
#'   developed by Lasse Ylianttila (STUK, Finland), originally in Excel. Option
#'   of using adaptive smoothing was added by Pedro J. Aphalo, as well as the
#'   modified method "full", that uses a different algorithm for the
#'   filter-based stray light correction, and adaptive smoothing by default.
#'   Method "sun" is the same as "full" but trims the spectrum by setting all
#'   values outside the range 290-900 nm to zero before smoothing.
#' @author Pedro J. Aphalo
#' @export
#' @seealso See Also as \code{\link{process_maya_arrays}}.
#' @references \url{http://www.r4photobiology.info}
#' @keywords misc
#'
calc_maya_calibration2 <- function(cts.dt,
                                   FEL_short_integration_time,
                                   D2_short_integration_time,
                                   FEL_long_integration_time,
                                   FEL_constants,
                                   D2_constants,
                                   bad_pixels=NULL,
                                   method="raw",
                                   smoothing_hi_lim=0.0,
                                   smoothing_coef=1.0,
                                   serial_number="MAYP11278",
                                   verbose=FALSE)
{
    FEL_cts.dt <- cts.dt[ , list(w.length       = w.length,
                                 short_meas_cts = FELshort,
                                 short_dark_cts = FELshort_dark,
                                 long_meas_cts  = FELlong,
                                 long_dark_cts  = FELlong_dark,
                                 filter_cts     = FELlong_PC)]

    FEL.data <- process_maya_arrays(cts = FEL_cts.dt,
                                    short_integration_time = FEL_short_integration_time,
                                    long_integration_time =  FEL_long_integration_time,
                                   serial_number = serial_number,
                                   method=method,
                                   smoothing_hi_lim=smoothing_hi_lim,
                                   smoothing_coef=smoothing_coef,
                                   bad_pixels=bad_pixels,
                                   verbose=verbose)

    D2_cts.dt <- cts.dt[ , list(w.length       = w.length,
                                short_meas_cts = D2,
                                short_dark_cts = D2_dark,
                                filter_cts     = D2_PC)]

    D2.data <- process_maya_arrays(cts = D2_cts.dt,
                                   short_integration_time = D2_short_integration_time,
                                   long_integration_time = NULL,
                                   serial_number = serial_number,
                                  method=method,
                                  smoothing_hi_lim=smoothing_hi_lim,
                                  smoothing_coef=smoothing_coef,
                                  bad_pixels=bad_pixels,
                                  verbose=verbose)

    FEL_calib.data <- FEL_spectrum(cts.dt[ , "w.length"], FEL_constants, fill=NA)
    D2_calib.data <- D2_spectrum(cts.dt[ , "w.length"], D2_constants, fill=NA)

    calib.df <- dplyr::data_frame(pixel_idx = 1L:2068L,
                           w.length = cts.dt[ , "w.length"],
                           FEL_cts = FEL.data[ , "s.e.irrad"],
                           D2_cts = D2.data[ , "s.e.irrad"],
                           FEL_s.irrad = FEL_calib.data[ , 2],
                           D2_s.irrad = D2_calib.data[ , 2])
    within_range <- with(calib.df, w.length >= 250 & w.length <= 900)
    calib.df <-
      dplyr::mutate(calib.df,
                    FEL_mult = ifelse(within_range, FEL_s.irrad / FEL_cts, NA),
                    D2_mult_raw = ifelse(within_range, D2_s.irrad / D2_cts, NA))
    D2_corr_factor <-
      with(calib.df[calib.df[["w.length"]] >= 330 & calib.df[["w.length"]] <= 360, ],
           mean(FEL_mult / D2_mult_raw, na.rm=TRUE))
    calib.df <-
      dplyr::mutate(calib.df,
                    D2_mult = ifelse(within_range, D2_mult_raw * D2_corr_factor, NA),
                    calc_multiplier = ifelse(w.length <= 340, D2_mult, FEL_mult) )
    #    setkey(calib.df)
    if (!is.null(bad_pixels) && length(bad_pixels) > 0) {
      calib.df[bad_pixels, "cal.multiplier"] <- NA
    }

    if (!verbose) {
      calib.df <- calib.df[ , c("w.length", "cal.multiplier")]
    }
#    setkey(calib.df, w.length)

    setattr(calib.df, "comment", paste("FEL:\n", comment(FEL.data), "\n\nDeuterium:\n", comment(D2.data), sep="\n"))

    return(calib.df)
  }
