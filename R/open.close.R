# functions to open and close a wrapper to the driver

#' Connect to Maya spectrometer
#'
#' Open a connection to the first spectrometer found
#' and initialize an object with a reference to the
#' Java object returned by Omni Driver.
#'
#' @return a java wrapper which allows access to the driver with an open
#' connection to the instrument.
#'
#' @export
#'
start_session <- function() {
  w <- rOmniDriver::init_srs()
  num.srs <- rOmniDriver::number_srs(w)

  # num.srs <- 1
  if (num.srs < 0) {
    stop("Error in get number of SRs")
  } else if (num.srs == 0) {
    stop("No SR found")
  }
  w
}

#' Disconnect from spectrometer
#'
#' Close the connection to the spectrometer pointed
#' at by the Java object referenced in \code{x}.
#'
#' @param w a java wrapper as returned by \code{start_session()}
#'
#' @export
#'
end_session <- function(w) {
  rOmniDriver::srs_close(w)
}
