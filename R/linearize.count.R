#' Function to apply linearization correction to raw counts data.
#'
#' Uses linearization parameters and function supplied by Ocean Optics, the
#' manufacturer of the spectrometer.
#'
#' @param raw_counts A numeric vector of raw detector counts. Should include
#'   data for the whole detector array.
#' @param linearize.fun A function or a polynom::polynomial object containing
#'   the linearization to be applied.
#' @param force_zero A logical indicating whether to change negative count
#'   values to zero.
#' @param verbose Logical indicating the level of warnings wanted. Defaults to
#'   \code{FALSE}.
#'
#' @return A numeric array containing the adjusted values, still as uncalibrated
#'   counts.
#'
#' @author Pedro J. Aphalo, based on Excel code by Lasse Ylianttila.
#' @export
#' @references \url{http://www.r4photobiology.info/}
#' @keywords misc
#'
linearize_count <-
function(raw_counts, linearize.fun,
         force_zero = TRUE, verbose = FALSE)
{
  if (any(raw_counts < 0.0)) {
    if (force_zero) {
      raw_counts <- ifelse(raw_counts >= 0.0, raw_counts, 0.0)
      if (verbose) warning("negative counts in raw_counts converted to zeros")
    } else if (verbose) {
      warning("retaining raw_counts < 0.0 as input to linearize count")
    }
  }
  # linearization function as supplied by Ocean Optics
  if (polynom::is.polynomial(linearize.fun)) {
    lin.calib.fun <- as.function(linearize.fun)
  }
  linearized <- raw_counts / linearze.fun(raw_counts)
  if (any(is.na(linearized))) {
    stop("NAs in linearized raw_counts")
  }
  if (verbose && any(linearized < 0.0)) {
    warning("negative counts in linearized raw_counts")
  }
  return(linearized)
}

#' Function to apply linearization correction to raw counts data.
#'
#' Uses linearization parameters and function supplied by Ocean Optics, the
#' manufacturer of the spectrometer.
#'
#' @param x An oo_spct object
#' @param cal_idx A numeric index used to access calibration data
#' @param force_zero A logical indicating whether to change negative count
#'   values to zero.
#' @param verbose Logical indicating the level of warnings wanted. Defaults to
#'   \code{FALSE}.
#'
#' @return A numeric array containing the adjusted values, still as uncalibrated
#'   counts.
#'
#' @author Pedro J. Aphalo, based on Excel code by Lasse Ylianttila.
#' @export
#' @references \url{http://www.r4photobiology.info/}
#' @keywords misc
#'
linearize.oo_spct <- function(x, cal_idx, force.zero = TRUE, verbose = TRUE) {
  linearize.fun <- polynom::polynomial()
  stopifnot(is.oo_spct(spct))
  x[["linear.counts"]] <- linearize_count(cal.idx, x[["counts"]],
                                          force.zero = force.zero, verbose = verbose)
  x
}

