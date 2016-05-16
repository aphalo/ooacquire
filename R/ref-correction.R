#' Convert raw detector counts into counts per second (cps).
#'
#' These functions simply divide "raw counts" by the integration time used
#' for acquisition.
#'
#' @param x,y R objects
#' @param .oper a function with its first two formal parameters accepting
#'   numerical vectors of equal length (e.g. a binary numerical operator).
#' @param ... not used in current version
#'
#' @export ref_correction
#'
#' @note In the case of objects of class "raw_spct" the columns with names
#' starting with "counts" are processed. All other columns are left unchanded.
#'
ref_correction <- function(x, y, .oper, ...) UseMethod("ref_correction")

#' @describeIn ref_correction Default method
#'
#' @export
#'
ref_correction.default <- function(x, y, .oper, ...) {
  NA
}

#' @describeIn ref_correction Numeric method
#'
#' @export
#'
#' @return a numeric vector of the same length as x
#'
ref_correction.numeric <- function(x, y, .oper = `-`, ...) {
  .oper(x, y, ...)
}

#' @describeIn ref_correction Method for spectral data expressed as raw
#'   instrument counts.
#'
#' @export
#'
#' @return an object of class "cps_spct"
#'
ref_correction.raw_spct <- function(x,
                                    y,
                                    .oper = `-`,
                                    ...) {
  # check that data are from same instrument
  stopifnot(getInstrDesc(x)[["spectrometer.sn"]] ==
              getInstrDesc(y)[["spectrometer.sn"]])
  # guard against differences in linearization
  stopifnot(getInstrSettings(x)[["linearized"]] ==
              getInstrSettings(y)[["linearized"]])
  # check that measurement conditions were the same
  stopifnot(all(getInstrSettings(x)[["integ.time"]] ==
                  getInstrSettings(y)[["integ.time"]]))
  # check that the two spectra have equivalent spectral data
  counts.cols_x <- names(x)[grep("^counts", names(x))]
  counts.cols_y <- names(x)[grep("^counts", names(y))]
  stopifnot(all(counts.cols_x == counts.cols_y))
  counts.cols <- counts.cols_x

  # other columns to keep unchanged
  other.cols <- setdiff(names(x), counts.cols)
  z <- as.generic_spct(x)[other.cols]

  # subtract column by column
  for (i in seq_along(counts.cols)) {
    z[[counts.cols[i]]] <- .oper(x[[counts.cols[i]]], y[[counts.cols[i]]], ...)
  }
  setRawSpct(z)
  # add metadata to result
  setInstrDesc(z, getInstrDesc(x))
  setInstrSettings(z, getInstrSettings(x))
  setWhenMeasured(z, getWhenMeasured(x))
  setWhereMeasured(z, getWhereMeasured(x))
  setWhatMeasured(z, getWhatMeasured(z))
  z
}

#' @describeIn ref_correction Method for spectral data expressed as
#'   counts per second.
#'
#' @export
#'
#' @return an object of class "cps_spct"
#'
ref_correction.cps_spct <- function(x,
                                    y,
                                    .oper = `-`,
                                    ...) {
  # check that data are from same instrument
  stopifnot(getInstrDesc(x)[["spectrometer.sn"]] ==
              getInstrDesc(y)[["spectrometer.sn"]])
  # guard against differences in linearization
  stopifnot(attr(x, "linearized") == attr(y, "linearized"))
  # check that measurement conditions were the same
  stopifnot(all(getInstrSettings(x)[["integ.time"]] ==
                  getInstrSettings(y)[["integ.time"]]))
  # check that the two spectra have equivalent spectral data
  cps.cols_x <- names(x)[grep("^cps", names(x))]
  cps.cols_y <- names(x)[grep("^cps", names(y))]
  stopifnot(all(cps.cols_x == cps.cols_y))
  cps.cols <- cps.cols_x

  # other columns to keep unchanged
  other.cols <- setdiff(names(x), cps.cols)
  z <- as.generic_spct(x)[other.cols]

  # subtract column by column
  for (i in seq_along(cps.cols)) {
    z[[cps.cols[i]]] <- .oper(x[[cps.cols[i]]],  y[[cps.cols[i]]], ...)
  }
  setCpsSpct(z)
  # add metadata to result
  setInstrDesc(z, getInstrDesc(x))
  setInstrSettings(z, getInstrSettings(x))
  setWhenMeasured(z, getWhenMeasured(x))
  setWhereMeasured(z, getWhereMeasured(x))
  setWhatMeasured(z, getWhatMeasured(z))
  z
}





