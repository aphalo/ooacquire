#' Measure one raw spectrum
#'
#' Take one spectral measurement which depending on the settings can consist
#' on more than one raw spectrum meant to represent a SINGLE observation after
#' conversion into calibrated data such as in the case of HDR.
#'
#' @param oo_descriptor list as returned by function \code{get_oo_descriptor}
#' @param acq_settings list as returned by functions \code{tune_acq_settings}
#' @param what.measured value used to set attribute
#' @param verbose ogical to enable or disable warnings
#'
#' @export
#'
#' @return a "raw_spct" object with one or more columns containing raw counts
#' and one column with wavelength data.
#'
acq_raw_spct <- function(oo_descriptor,
                         acq_settings,
                         what.measured = NA,
                         verbose = TRUE) {
  x <- acq_settings
  y <- oo_descriptor
  num.readings <- length(x$integ.time)
  z <- photobiology::raw_spct(w.length = y$wavelengths, counts = 0)

  for (i in 1:num.readings) {
    counts.name <- paste("counts", i, sep = "_")
    rOmniDriver::set_integration_time(y$w, x$integ.time[i], y$sr.index, y$ch.index)
    actual.integ.time <- rOmniDriver::get_integration_time(y$w, y$sr.index, y$ch.index)
    if (actual.integ.time != x$integ.time[i]) {
      warning("The spectrometer has overridden the integration time!")
      x$integ.time[i] <- actual.integ.time
    }
    rOmniDriver::set_scans_to_avg(y$w, x$num.scans[i], y$sr.index, y$ch.index)
    if (verbose) message(paste("Measurement ", i, "..."))
    spct <- rOmniDriver::get_spectrum(y$w, y$sr.index, y$ch.index)
    if (rOmniDriver::is_spectrum_valid(y$w, y$sr.index, y$ch.index) || x$force.valid)
    {
      z[[counts.name]] <- spct
    } else {
      z[[counts.name]] <- rep(NA_real_, length(z$w.length))
    }
  }
  photobiology::setInstrDesc(z, y)
  photobiology::setInstrSettings(z, x)
  photobiology::setWhenMeasured(z, lubridate::now())
  photobiology::setWhatMeasured(z, what.measured)
  z
}

#' Take one set of spectral readings
#'
#' Take readings according to parameters from a list of settings and a protocol
#' defined by a vector of names.
#'
#' @param oo_descriptor list as returned by function \code{get_oo_descriptor()}
#' @param acq_settings list as returned by functions \code{tune_acq_settings()}
#' or \code{retune_acq_settings()} or \code{acq_settings()}
#' @param protocol vector of character strings
#' @param user.label character string to set as label
#' @param geocode data.frame with at least columns "lon" and "lat" compatible
#' with value returned by \code{ggmap::geocode()}
#' @param verbose ogical to enable or disable warnings
#'
#' @export
#' @return a raw_mspct object
#'
acq_raw_mspct <- function(oo_descriptor,
                          acq_settings,
                          protocol = c("measure", "filter", "dark"),
                          user.label = "",
                          geocode = data.frame(lon = NA_real_, lat = NA_real_),
                          verbose = TRUE) {
  previous.protocol <- "none"
  z <- list()
  idx <- 0
  for (p in protocol) {
    if (p != previous.protocol) {
      previous.protocol <- p
      answ <- readline(paste("Ready to acquire", p,
                             "measurement ('z' = abort)"))
      if (tolower(answ[1]) == "z") {
        break()
      }
    }
    idx <- idx + 1
    z[[idx]] <- acq_raw_spct(oo_descriptor = oo_descriptor,
                             acq_settings = acq_settings,
                             what.measured = list(what = p, user.label = user.label))
  }
  z <- photobiology::as.raw_mspct(z)
  photobiology::setWhereMeasured(z, geocode)
  z
}


