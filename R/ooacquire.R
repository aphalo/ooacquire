#' @details
#' Processing of raw-counts data acquired following different protocols and
#' corrections including integration-time bracketing and merging, slit function
#' corrections, subtraction of stray light and adaptive smoothing corrections.
#'
#' Acquisition of raw-counts spectra directly from Ocean Optics' spectrometers.
#' A high level interface to the OmniDriver library for spectrometer
#' control and data acquisition provided by Ocean Optics for their
#' instruments. Built on top of the 'rOmniDriver' package which
#' provides a very thin wrapper on the Java functions giving access to
#' the proprietary driver.
#'
#' Reading of raw-counts spectra from files saved by Ocean Optics' software:
#' OceanView, SpectraSuite, Raspberry Pi SDK, and Jaz firmware.
#'
#' @note This package is provided \strong{without any warranty of suitability
#'   for any specific purpose or instrument} as it has been tested with only
#'   three different models of Ocean Optics spectrometers, each with one or two
#'   of many available hardware configurations.
#'
#' @import photobiology
#' @import ggspectra
#' @import lubridate
#' @importFrom Rcpp evalCpp
#' @import magrittr
#' @useDynLib ooacquire
#'
"_PACKAGE"
