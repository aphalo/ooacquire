#' Convert raw detector counts into counts-per-second
#'
#' Replace data from bad pixels with interpolated values, replace data from
#' saturated and nearby pixels withs NAs, apply linearization function if data
#' is not already linearized, optionally use a range of pixels as dark
#' reference, convert the raw counts for each integration time used into
#' counts-per-second, if data from bracketed intergartion times is available,
#' splice the different spectra.
#'
#' @param x raw_spct object.
#' @param ref.pixs.range integer vector of length 2.
#' @param ... currently ignored.
#'
#' @return a cps_spct object with one spectrum preserving the metadata present in
#'   \code{x}.
#'
#' @family functions for conversion of raw-counts data
#'
#' @export raw2corr_cps
#'
raw2corr_cps  <- function(x,
                          ref.pixs.range,
                          ...) UseMethod("raw2corr_cps")

#' @describeIn raw2corr_cps Default method
#'
#' @export
#'
raw2corr_cps.default <- function(x,
                                 ref.pixs.range = NULL,
                                 ...) {
  warning("Class ", class(x), " not supported")
  cps_spct()
}

#' @describeIn raw2corr_cps raw_spct method
#'
#' @export
#'
raw2corr_cps.raw_spct <- function(x,
                                  ref.pixs.range = c(1,100),
                                  ...) {
  # replace bad data with NAs
  x <- trim_counts(x)
  x <- bleed_nas(x)
  x <- skip_bad_pixs(x)
  # linearize detector counts
  x <- linearize_counts(x)
  # remove dark signal
  # using selected pixels of the detector array
  if (length(ref.pixs.range) >= 2) {
    ref.wls.range <- unlist(x[range(ref.pixs.range), "w.length"])
    x <- fshift(x, range = ref.wls.range)
  }
  # convert raw counts to counts per second
  x <- raw2cps(x)
  # if bracketing was used, splice the spectra
  x <- merge_cps(x)
  # apply slit function correction
  x <- slit_function_correction(x)
  x
}

#' @describeIn raw2corr_cps raw_spct method
#'
#' @export
#'
raw2corr_cps.raw_mspct <- function(x,
                                   ref.pixs.range = c(1,100),
                                   ...) {
  msmsply(x, .fun = raw2corr_cps.raw_spct, ref.pixs.range = ref.pixs.range, ...)
}


