# functions to open and close a wrapper to the driver

#' Connect to Maya spectrometer
#'
#' Open a connection to the first spectrometer found
#' and initialize an object with a reference to the
#' Java object returned by Omni Driver.
#'
#' @export
#'
start_session <- function() {
  srs <- rOmniDriver::init_srs()
  num.srs <- rOmniDriver::number_srs(srs)

  # num.srs <- 1
  if (num.srs < 0) {
    stop("Error get number of SRs")
  } else if (num.srs == 0) {
    stop("No SR found")
  } else {
    sr.idxs <- 1:num.srs - 1
  }
  return(init_raw_spc(w = srs, sr.index = sr.idxs[1]))
}

#' Disconnect from spectrometer
#'
#' Close the connection to the spectrometer pointed
#' at by the Java object referenced in \code{x}.
#'
#' @param x a list as returned by \code{start_session()} or
#'
#' @export
#'
end_session <- function(x) {
  rOmniDriver::srs_close(x$settings$w)
}
