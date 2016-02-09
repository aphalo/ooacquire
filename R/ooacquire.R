#' Processing of data from Maya2000Pro
#'
#' A set of functions and data for processing the raw data from the Maya2000 Pro
#' spectrometer from Ocean Optics to obtain spectral (energy) irradiance. One
#' needs to save raw count data from SpectraSuite as all calculations are done
#' by this package, applying: 1) linearization, 2) slit function correction, 3)
#' stray light correction, 4) integration time bracketing (optional), 5)
#' calibration, and 6) adaptive smoothing.
#'
#' @details
#'
#' @note This package is provided \strong{without nay warranty of suitability
#'   for any specific purpose or instrument} as it has been tested with only
#'   three different models of Ocean Optics spectrometers, each one with one of
#'   many possible configurations.
#'
#'
#' @import photobiology
#'
"_PACKAGE"



