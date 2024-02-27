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
#' @param despike logical flag, if TRUE despiking will be attempted.
#' @param hdr.tolerance numeric Passed as tolerance argument to merge_cps().
#' @param ... passed to \code{photobiology::despike}.
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
raw2corr_cps.raw_spct <-
  function(x,
           ref.pixs.range = c(1, 100),
           despike = FALSE,
           hdr.tolerance = getOption("ooacquire.hdr.tolerance", default = 0.05),
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
    x <- merge_cps(x, tolerance = hdr.tolerance)
    # apply slit function correction
    x <- slit_function_correction(x)
    # check for spikes and remove them
    if (despike) {
      spike.wls <- photobiology::spikes(x, ...)[["w.length"]]
      if (length(spike.wls) > 0) {
        warning("Despiking as spikes were detected at: ",
                paste(round(spike.wls, digits = 0), collapse = ", "), " nm.")
        x <- photobiology::despike(x, ...) # may need to adjust arguments
      }
    }
    x
  }

#' @describeIn raw2corr_cps raw_spct method
#'
#' @export
#'
raw2corr_cps.raw_mspct <- function(x,
                                   ref.pixs.range = c(1, 100),
                                   despike = FALSE,
                                   ...) {
  msmsply(x,
          .fun = raw2corr_cps.raw_spct,
          ref.pixs.range = ref.pixs.range,
          despike = despike,
          ...)
}


