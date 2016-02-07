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
#' \tabular{ll}{
#' Package: \tab MayaCalc\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1.0\cr
#' Date: \tab 2015-10-07\cr
#' License: \tab GPL (>= 3.0)\cr
#' URL: \tab \url{http://www.r4photobiology.info},\cr
#' \tab \url{https://bitbucket.org/aphalo/mayacalc}\cr
#' BugReports: \tab \url{https://bitbucket.org/aphalo/mayacalc}\cr
#' }
#'
#' @note This package is provided as an \strong{example} as it is specific
#' to our own spectrometer unit. As a minimum the calibration data and
#' the constants related to the characterization of the slit function
#' and stray light properties must be obtained for the specific
#' unit of spectrometer used. For different configurations and models,
#' and measurement types the smoothing and noise removal algorithms may
#' also need re-tunning.
#'
#' @name MayaCalc-package
#' @aliases MayaCalc-package MayaCalc
#' @docType package
#' @author Pedro J. Aphalo \email{pedro.aphalo@@helsinki.fi}
#'
#' @keywords package
#' @importFrom Rcpp evalCpp
#' @useDynLib 00Acquire
#' @import photobiology
#'
NULL



