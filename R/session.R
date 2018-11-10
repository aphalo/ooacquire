# functions to open and close a wrapper to the driver

#' Connect to Maya spectrometer
#'
#' Open a connection to the first spectrometer found
#' and initialize an object with a reference to the
#' Java object returned by Omni Driver.
#'
#' @param error.action function, usually one of \code{stop()}, \code{warning()}
#'   or \code{message()}.
#'
#' @return On success a java wrapper which allows access to the driver with an open
#' connection to the instrument.
#'
#' @export
#'
start_session <- function(error.action = stop) {
  w <- rOmniDriver::init_srs()
  message("Omni Driver version: ", rOmniDriver::get_api_version(w))
  message("rOmniDriver package version: ", utils::packageVersion("rOmniDriver"))
  num.srs <- rOmniDriver::number_srs(w)

  # num.srs <- 1
  if (num.srs < 0) {
    message_text <-
    error.action("Error starting session: ", message_text)
  } else if (num.srs == 0) {
    warning("No SR found")
  }
  w
}

#' List connected spectrometers
#'
#' List all connected spectrometers by model name and serial number, plus
#' the number of channels in each of them.
#'
#' @param w an open Wrapper object from Omnidriver
#'
#' @return a list
#'
#' @export
list_instruments <- function(w) {
  number.srs <- rOmniDriver::number_srs(w)
  srs.idxs <- seq(from = 0, length.out = number.srs)
  z <- data.frame(idx = numeric(),
                  model = character(),
                  serial.no = character(),
                  num.channels = integer())
  for (i in srs.idxs) {
    tmp <- data.frame(idx = i,
                      model = rOmniDriver::get_name(w, sr.index = i),
                      serial.no = rOmniDriver::get_serial_number(w, sr.index = i),
                      num.channels = rOmniDriver::get_number_of_channels(w, sr.index = i))
    z <- rbind(z, tmp)
  }
  z
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
  message("Clossing connection to ", rOmniDriver::number_srs(w), " spectrometer(s)")
  rOmniDriver::srs_close(w)
}
