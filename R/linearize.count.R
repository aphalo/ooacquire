#' Function to apply linearization correction to raw counts data.
#'
#' Uses a user supplied function, possibly that supplied by a manufacturer
#' like Ocean Optics stored in firmware or in any other form.
#'
#' @param x raw_spct object.
#' @param linearize.fun A function or a polynom::polynomial object containing
#'   the linearization to be applied.
#' @param force_zero A logical indicating whether to change negative count
#'   values to zero.
#' @param verbose Logical Currently ignired.
#'
#' @return A raw_spct object containing the adjusted values, still as uncalibrated
#'   counts. The object is tagged with the with attribute "linearized" set to
#'   the function used for linearization.
#'
#' @export
#'
linearize_count <- function(x, linearize.fun = NULL,
                            force_zero = TRUE, verbose = FALSE)
{
  # guard against attempts to reapply linearization
  stopifnot(!attr(x, "linearized"))
  # if linearization function not supplied use the polynomial earlier
  # retrieved from instrument firmware
  if (is.null(linearize.fun)) {
    calib.data <- getInstrDesc(x)[["calib.data"]]
    stopifnot(!is.null(calib.data))
    linearize.fun <- calib.data[["nl.poly"]]
  }
  # if polynomial supplied instead of function we convert it
  if (polynom::is.polynomial(linearize.fun)) {
    linearize.fun <- as.function(linearize.fun)
  }
  # any variable whose name starts with "counts" will be linearized
  counts.cols <- names(x)[grep("^counts", names(x))]
  for (col in counts.cols) {
    if (force_zero) {
      x[[col]] <- ifelse(x[[col]] >= 0.0, x[[col]], 0.0)
      x[[col]] <- x[[col]] / linearize.fun(x[[col]])
    }
  }
  attr(x, "linearized") <- TRUE
  attr(x, "linearize.fun")  <- linearize.fun
  x
}
