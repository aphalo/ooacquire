#' Find valid range of integration times
#'
#' Find by trial and error the accepted range of integration times for a
#' spectrometer.
#'
#' @param x an initialized list of raw spectra
#' @param interval numeric Vector giving the interval of integration time values
#'   in seconds within which to search.
#' @param start.multiplier numeric
#' @param start.step numeric
#'
#' @return A numeric vector of length two, with minimum and maximum accepted
#'   integration time values in seconds.
#'
#' @export
#'
integ_time_range <- function(x, interval = c(1 * 1e-3, 240), start.multiplier = 2, start.step = 0.05) {

  search_boundary <- function(guessed.time, multiplier, refine.step) {
    target.int.time <- guessed.time
    repeat{
      rOmniDriver::set_integration_time(x$settings$w, target.int.time, x$settings$sr)
      actual.int.time <- rOmniDriver::get_integration_time(x$settings$w, x$settings$sr)
      if (actual.int.time == target.int.time) {
        target.int.time <-  target.int.time / multiplier
        multiplier <- multiplier * (1 + refine.step)
        if (multiplier < 1.05) {
          return(actual_int_time)
        }
      }
      target.int.time <- target.int.time * multiplier
    }
  }

  # Set a valid integration time to start with
  rOmniDriver::set_integration_time(x$settings$w, 1e5, x$settings$sr)

  # The spectrometer driver expects times in micro seconds
  interval <- interval * 1e6 # s -> us

  min.integ.time <- search_boundary(guessed.time = min(interval),
                                    multiplier = start.multiplier,
                                    refine.step = -start.step)

  max.integ.time <- search_boundary(guessed.time = max(interval),
                                    multiplier = 1 / start.multiplier,
                                    refine.step = +start.step)
  # We return times in seconds
  return(c(min.integ.time * 1e-6, max.integ.time * 1e-6))
}


#' Find valid range of integration times
#'
#' Find by trial and error the accepted range of integration times for a
#' spectrometer. This is a wrapper on \code{integ_time_range()} that opens
#' and closes the "connection" to the spectrometer.
#'
#' @return A numeric vector of length two, with minimum and maximum accepted
#'   integration time values in seconds.
#'
#' @export
#'
report_int_time_range <- function() {
  spct.init.ls <- start_session()
  range <- integ_time_range(spct.init.ls)
  end_session(spct.init.ls)
  return(range)
}
