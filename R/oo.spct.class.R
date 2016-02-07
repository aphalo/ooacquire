#' Check validity of sensor counts objects
#'
#' Check that an R object contains the expected data members.
#'
#' @param x An R object
#' @param byref logical indicating if new object will be created by reference or
#'   by copy of \code{x}
#' @param strict.range logical indicating whether off-range values result in an
#'   error instead of a warning, \code{NA} disables the test
#' @param valid.counts.range integer vector of length two giving the exected
#'   maximum range of the instrument sensor
#' @param serial.number character Expected value for serial number
#' @param ... additional param possible derived methods
#' @export
#'
#' @family data validity check functions
#'
check.oo_spct <- function(x,
                          byref = TRUE,
                          strict.range = getOption("photobiology.strict.range", default = FALSE),
                          valid.counts.range = c(0, 64000),
                          ...) {

  range_check <- function(x) {
    counts.range <- range(x[["counts"]], na.rm = TRUE)
    if (counts.range[1] < valid.counts.range[1] || counts.range[2] > valid.counts.range[2]) {
      message.text <- paste0("Off-range sensor counts:", counts.range,
                             ", valid:", valid.counts.range)
      if (strict.range) {
        stop(message.text)
      } else {
        warning(message.text)
      }
    }
  }

  x <- check.generic_spct(x, multiple.wl = 1)

  if (exists("counts", x, mode = "numeric", inherits=FALSE)) {
    range_check(x)
    return(x)
  } else if (exists("sensor.counts", x, mode = "numeric", inherits=FALSE)) {
    x <- dplyr::rename(x, counts = sensor.counts)
    warning("Found variable 'sensor.counts', renamed it to 'counts'")
    range_check(x)
    return(x)
  } else {
    warning("No sensor-counts per data found in 'x'")
    x[["counts"]] = NA_real_
    return(x)
  }
}


#' Convert an R object into a sensor counts spectrum object.
#'
#' Sets the class attibute of a data.frame or an object of a derived
#' class to "oo_spct".
#'
#' @param x data.frame, list or generic_spct and derived classes
#' @param integration.time numeric Integration time (seconds)
#' @param instrument.name character
#' @paran serial.number character
#' @param time.stamp POSIXct
#' @param geolocation
#' @param meas.type character
#' @param clip.fill numeric Value used as replacement of clipped data
#'
#' @export
#' @exportClass oo_spct
#'
#' @family set and unset spectral class functions
#'
setOoSpct <- function(x,
                      integration.time = NA,
                      scans = NA,
                      instrument.name = "unknown",
                      serial.number = "missing",
                      time.stamp = lubridate::now(),
                      geolocation = c(0, 0),
                      measure.type = "scan",
                      strict.range = FALSE,
                      valid.counts.range = c(0, 64000),
                      clip.fill = NA_real_) {
  name <- substitute(x)
  photobiology::rmDerivedSpct(x)
  if (!is.data.frame(x) || inherits(x, "data.table")) {
    x <- dplyr::as_data_frame(x)
  }
  if (!photobiology::is.generic_spct(x)) {
    photobiology::setGenericSpct(x, multiple.wl = 1)
  }
  if (!is.oo_spct(x)) {
    class(x) <- c("oo_spct", class(x))
  }
  setIntegrationTime(x, integration.time)
  setNumScans(x, scans)
  setInstrumentName(x, instrument.name)
  setSerialNumber(x, serial.number)
  setTimeStamp(x, time.stamp)
  setGeolocation(x, geolocation)
  setMeasureType(x, measure.type)
  x <- photobiolgy::check(x,
             byref = TRUE,
             strict.range = strict.range,
             valid.counts.range = valid.counts.range)
  x <- dplyr::mutate(x,
                     counts = ifelse(counts == valid.counts.range[2] &
                                       !is.null(clip.fill), clip.fill, counts))
  if (is.name(name)) {
    name <- as.character(name)
    assign(name, x, parent.frame(), inherits = TRUE)
  }
  invisible(x)
}

# is functions for oo_spct class --------------------------------------------

#' Query class of spectrum objects
#'
#' Functions to check if an object is of a given type of spectrum, or coerce it if
#' possible.
#'
#' @param x an R object.
#'
#' @return These functions return \code{TRUE} if its argument is a of the queried type
#'   of spectrum and \code{FALSE} otherwise.
#'
#' @export is.generic_spct
#'
is.oo_spct <- function(x) inherits(x, "oo_spct")

# Set and get integration.time attribute ------------------------------------------------------

#' Set the "integration.time" attribute
#'
#' Function to set by reference the "integration.time" attribute of an existing
#' oo_spct
#'
#' @param x an oo_spct
#' @param integration.time numeric, time in seconds
#'
#' @return x
#'
#' @note if x is not an oo_spct an error is triggered..
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
setIntegrationTime <- function(x, integration.time = NULL) {
  stopifnot(is.oo_spct(x))
  stopifnot(is.numeric(integration.time) && integration.time > 0.0)
  name <- substitute(x)
  attr(x, "integration.time") <- integration.time
  if (is.name(name)) {
    name <- as.character(name)
    assign(name, x, parent.frame(), inherits = TRUE)
  }
  invisible(x)
}

#' Get the "integration.time" attribute
#'
#' Function to read the "integration.time" attribute of an existing oo_spct
#' object.
#'
#' @param x an oo_spct object
#'
#' @return numeric value, integration time in seconds
#'
#' @note if x is not a \code{oo_spct} object, or the attribute is missing
#'   \code{NA} is returned.
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'

getIntegrationTime <- function(x) {
  if (is.oo_spct(x)) {
    integration.time <- attr(x, "integration.time", exact = TRUE)
    stopifnot(!is.null(integration.time))
  } else {
    integration.time <- NA_real_
  }
  integration.time
}

# Set and get num.scans attribute ------------------------------------------------------

#' Set the "num.scans" attribute
#'
#' Function to set by reference the "num.scans" attribute of an existing
#' oo_spct
#'
#' @param x an oo_spct
#' @param num.scans numeric, number of averaged scans
#'
#' @return x
#'
#' @note If x is not a oo_spct and error is triggered
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
setNumScans <- function(x, num.scans = NULL) {
  stopifnot(is.oo_spct(x))
  stopifnot(is.numeric(num.scans) && num.scans > 0.0)
  name <- substitute(x)
  attr(x, "num.scans") <- num.scans
  if (is.name(name)) {
    name <- as.character(name)
    assign(name, x, parent.frame(), inherits = TRUE)
  }
  invisible(x)
}

#' Get the "num.scans" attribute
#'
#' Function to read the "num.scans" attribute of an existing oo_spct
#' object.
#'
#' @param x an oo_spct object
#'
#' @return numeric value
#'
#' @note if x is not a \code{oo_spct} object, or the attribute is missing
#'   \code{NA} is returned.
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
getNumScans <- function(x) {
  if (is.oo_spct(x)) {
    num.scans <- attr(x, "num.scans", exact = TRUE)
    stopifnot(!is.null(num.scans))
  } else {
    num.scans <- NA_real_
  }
  num.scans
}

# Set and get instrument.name attribute ------------------------------------------------------

#' Set the "instrument.name" attribute
#'
#' Function to set by reference the "instrument.name" attribute of an existing
#' oo_spct
#'
#' @param x an oo_spct
#' @param instrument.name character string with the name of the instrument
#'
#' @return x
#'
#' @note if x is not a oo_spct and error is triggered
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
setInstrumentName <- function(x, instrument.name = NULL) {
  stopifnot(is.oo_spct(x))
  stopifnot(is.character(instrument.name))
  name <- substitute(x)
  attr(x, "instrument.name") <- instrument.name
  if (is.name(name)) {
    name <- as.character(name)
    assign(name, x, parent.frame(), inherits = TRUE)
  }
  invisible(x)
}

#' Get the "instrument.name" attribute
#'
#' Function to read the "instrument.name" attribute of an existing oo_spct
#' object.
#'
#' @param x an oo_spct object
#'
#' @return character value
#'
#' @note if x is not a \code{oo_spct} object, or the attribute is missing
#'   \code{NA} is returned.
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
getInstrumentName <- function(x) {
  if (is.oo_spct(x)) {
    instrument.name <- attr(x, "instrument.name", exact = TRUE)
    stopifnot(!is.null(instrument.name))
  } else {
    instrument.name <- NA_character_
  }
  instrument.name
}

# Set and get serial.number attribute ------------------------------------------------------

#' Set the "serial.number" attribute
#'
#' Function to set by reference the "serial.number" attribute of an existing
#' oo_spct
#'
#' @param x an oo_spct
#' @param serial.number character string with the serial number of the
#' instrument
#'
#' @return x
#'
#' @note if x is not a oo_spct and error is triggered
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
setSerialNumber <- function(x, serial.number = NULL) {
  stopifnot(is.oo_spct(x))
  stopifnot(is.character(serial.number))
  name <- substitute(x)
  attr(x, "serial.number") <- serial.number
  if (is.name(name)) {
    name <- as.character(name)
    assign(name, x, parent.frame(), inherits = TRUE)
  }
  invisible(x)
}

#' Get the "serial.number" attribute
#'
#' Function to read the "serial.number" attribute of an existing oo_spct
#' object.
#'
#' @param x an oo_spct object
#'
#' @return character value
#'
#' @note if x is not a \code{oo_spct} object, or the attribute is missing
#'   \code{NA} is returned.
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
getSerialNumber <- function(x) {
  if (is.oo_spct(x)) {
    serial.number <- attr(x, "serial.number", exact = TRUE)
    stopifnot(!is.null(serial.number))
  } else {
    serial.number <- NA_character_
  }
  serial.number
}

# Set and get measure.type attribute ------------------------------------------------------

#' Set the "measure.type" attribute
#'
#' Function to set by reference the "measure.type" attribute of an existing
#' oo_spct
#'
#' @param x an oo_spct
#' @param measure.type character string from \code{"black.ref", "white.ref",
#'   "dark.ref", "source.ref", "refl.meas", "trans.meas", "irrad.meas",
#'   "filter.meas"}
#'
#' @return x
#'
#' @note if x is not a oo_spct and error is triggered
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
setMeasureType <- function(x, measure.type = NULL) {
  stopifnot(is.oo_spct(x))
  stopifnot(is.character(measure.type))
  stopifnot(measure.type %in%
              c("black.ref", "white.ref", "dark.ref", "source.ref",
                "refl.meas", "trans.meas", "irrad.meas", "filter.meas"))
  name <- substitute(x)
  attr(x, "measure.type") <- measure.type
  if (is.name(name)) {
    name <- as.character(name)
    assign(name, x, parent.frame(), inherits = TRUE)
  }
  invisible(x)
}

#' Get the "measure.type" attribute
#'
#' Function to read the "measure.type" attribute of an existing oo_spct
#' object.
#'
#' @param x an oo_spct object
#'
#' @return character value
#'
#' @note if x is not a \code{oo_spct} object, or the attribute is missing
#'   \code{NA} is returned.
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
getMeasureType <- function(x) {
  if (is.oo_spct(x)) {
    measure.type <- attr(x, "measure.type", exact = TRUE)
    stopifnot(!is.null(measure.type))
  } else {
    measure.type <- NA_character_
  }
  measure.type
}

# Set and get geolocation attribute ------------------------------------------------------

#' Set the "geolocation" attribute
#'
#' Function to set by reference the "geolocation" attribute of an existing
#' oo_spct
#'
#' @param x an oo_spct
#' @param geolocation numeric vector of length two, with latitude and
#' longitude in degrees
#' @param lat numeric vector of length one, with latitude in degrees
#' @param lon numeric vector of length one, with longitude in degrees
#'
#' @return x
#'
#' @note if x is not a oo_spct and error is triggered. \code{lat} and
#'   \code{long} are ignored unless geolacation is null.
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
setGeolocation <- function(x, geolocation = NULL, lat = NULL, lon = NULL) {
  stopifnot(is.oo_spct(x))
  if (is.null(geolocation) && (is.null(lat) || is.null(lon))) {
    geolocation <- c(NA, NA)
  } else if (is.null(geolocation)) {
    geolocation <- c(lat, lon)
  } else if (!is.numeric(geolocation) || length(geolocation) != 2) {
    geolocation <- c(NA, NA)
  }
  name <- substitute(x)
  attr(x, "geolocation") <- geolocation
  if (is.name(name)) {
    name <- as.character(name)
    assign(name, x, parent.frame(), inherits = TRUE)
  }
  invisible(x)
}

#' Get the "geolocation" attribute
#'
#' Function to read the "geolocation" attribute of an existing oo_spct
#' object.
#'
#' @param x an oo_spct object
#'
#' @return numeric vector of length two, with latitude and longutde in degrees
#'
#' @note if x is not a \code{oo_spct} object, or the attribute is missing
#'   \code{NA} is returned.
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
getIntrumentName <- function(x) {
  if (is.oo_spct(x)) {
    geolocation <- attr(x, "geolocation", exact = TRUE)
    stopifnot(!is.null(geolocation))
  } else {
    geolocation <- rep(NA_real_, 2)
  }
  geolocation
}

# Set and get time.stamp attribute ------------------------------------------------------

#' Set the "time.stamp" attribute
#'
#' Function to set by reference the "time.stamp" attribute of an existing
#' oo_spct
#'
#' @param x an oo_spct
#' @param time.stamp POSIXct with time of measurement
#'
#' @return x
#'
#' @note if x is not a oo_spct and error is triggered
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
setTimeStamp <- function(x, time.stamp = NULL) {
  stopifnot(is.oo_spct(x))
  stopifnot(is.POSIXct(time.stamp))
  name <- substitute(x)
  attr(x, "time.stamp") <- time.stamp
  if (is.name(name)) {
    name <- as.character(name)
    assign(name, x, parent.frame(), inherits = TRUE)
  }
  invisible(x)
}

#' Get the "time.stamp" attribute
#'
#' Function to read the "time.stamp" attribute of an existing oo_spct
#' object.
#'
#' @param x an oo_spct object
#'
#' @return POSIXct value
#'
#' @note if x is not a \code{oo_spct} object, or the attribute is missing
#'   \code{NA} is returned.
#'
#' @export
#'
#' @family raw counts spectra attribute functions
#'
getTimeStamp <- function(x) {
  if (is.oo_spct(x)) {
    time.stamp <- attr(x, "time.stamp", exact = TRUE)
    stopifnot(!is.null(time.stamp))
  } else {
    time.stamp <- lubridate::origin
  }
  time.stamp
}
