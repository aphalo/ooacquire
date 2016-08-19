#' @details
#' A higher level interface to the OmniDriver library for spectrometer
#' control and data acquisition provided by Ocean Optics for their
#' instruments. It is built on top of the 'rOmniDriver' package which
#' provides a very thin wrapper on the Java functions giving access to
#' the proprietary driver.
#'
#' @note This package is provided \strong{without nay warranty of suitability
#'   for any specific purpose or instrument} as it has been tested with only
#'   three different models of Ocean Optics spectrometers, each one with one of
#'   many possible configurations.
#'
#' @import photobiology
#' @importFrom Rcpp evalCpp
#' @useDynLib ooacquire
#'
"_PACKAGE"
