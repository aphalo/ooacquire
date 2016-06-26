#' Function to apply linearization correction to raw counts data.
#'
#' Uses a function stored as an attribute of \code{x}, by default retrieved
#' from the instrument's firmware at the time of data acquisition or possibly
#' a replacement set by the user.
#'
#' @param x raw_spct object.
#' @param force_zero A logical indicating whether to change negative count
#'   values to zero.
#' @param verbose Logical Currently ignired.
#'
#' @return A raw_spct object containing the adjusted values, still as uncalibrated
#'   counts. The object is tagged with the with attribute "linearized" set to
#'   the function used for linearization.
#'
#' @note In contrast to other classes defined in package 'photobiology', class
#'   "raw_spct" can have more than one column of raw counts in cases where the
#'   intention is to merge these values as part of the processing at the time
#'   the calibration is applied. In other words these columns are not separate
#'   observations but instrumental replicates or bracketing readings part of the
#'   same "logical" or "effective" observation. The contents of any column whose
#'   name starts with "counts" will have the intrument response linearization
#'   function applied.
#'
#' @export
#'
linearize_counts <- function(x,
                             force_zero = TRUE, verbose = FALSE) {
  # guard against attempts to reapply linearization
  settings <- getInstrSettings(x)
  stopifnot(!settings[["linearized"]])
  oo_descriptor <- getInstrDesc(x)
  if (is.null(oo_descriptor$inst.calib) ||
      is.na(oo_descriptor$inst.calib) ||
      !is.function(oo_descriptor$inst.calib$nl.fun)) {
    stop("Non-linearity correction function is not available")
  }
  nl.fun <- oo_descriptor$inst.calib$nl.fun
  counts.cols <- names(x)[grep("^counts", names(x))]
  for (col in counts.cols) {
    if (force_zero) {
      x[[col]] <- ifelse(x[[col]] >= 0.0, x[[col]], 0.0)
      x[[col]] <- nl.fun(x[[col]])
    }
  }
  setInstrSettings(x, set_linearized(settings))
  x
}


