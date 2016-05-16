#' Convert raw detector counts into counts per second (cps).
#'
#' These functions simply divide "raw counts" by the integration time used
#' for acquisition.
#'
#' @param x an R object
#' @param ... not used in current version
#'
#' @export raw2cps
#'
#' @note In the case of objects of class "raw_spct" the columns with names
#' starting with "counts" are processed and renamed to columns with names
#' starting with "cps". All other columns are left unchanded.
#'
raw2cps <- function(x, ...) UseMethod("raw2cps")

#' @describeIn raw2cps Default method
#'
#' @export
#'
raw2cps.default <- function(x, ...) {
  return(NA)
}

#' @describeIn raw2cps Numeric method
#'
#' @param integ.time numeric, the integration time used expressed in seconds.
#'
#' @export
#'
#' @return a numeric vector of the same length as x
#'
raw2cps.numeric <- function(x, integ.time, ...) {
  return(NA)
}

#' @describeIn raw2cps Method for spectral data expressed as raw instrument counts.
#'
#' @export
#'
#' @return an object of class "cps_spct"
#'
raw2cps.raw_spct <- function(x,
                             ...) {
  # guard against attempts to reapply linearization
  if (!getInstrSettings(x)[["linearized"]]) {
    x <- linearize_counts(x)
  }
  acq_settings <- getInstrSettings(x)
  integ.time <- acq_settings$integ.time
  counts.cols <- names(x)[grep("^counts", names(x))]
  cps.cols <- gsub("^counts", "cps", counts.cols)
  stopifnot(length(counts.cols) == length(integ.time))
  other.cols <- setdiff(names(x), counts.cols)
  z <- as.generic_spct(x)[other.cols]
  for (i in seq_along(counts.cols)) {
    z[[cps.cols[i]]] <- x[[counts.cols[i]]] / integ.time[i] * 1e6
  }
  setCpsSpct(z)
  setInstrDesc(z, getInstrDesc(x))
  setInstrSettings(z, getInstrSettings(x))
  setWhenMeasured(z, getWhenMeasured(x))
  setWhereMeasured(z, getWhereMeasured(x))
  setWhatMeasured(z, getWhatMeasured(z))
  z
}

#' @describeIn raw2cps Method for collections of raw-counts spectra
#'
#' @export
#'
#' @return an object of class "cps_mspct"
#'
raw2cps.raw_mspct <- function(x, ...) {
  msmsply(x, raw2cps, ...)
}






