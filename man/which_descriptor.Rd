% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s-irrad-corrected.R
\name{which_descriptor}
\alias{which_descriptor}
\title{Select which instrument descriptor to use}
\usage{
which_descriptor(
  date = lubridate::now(tzone = "UTC"),
  descriptors = ooacquire::MAYP11278_descriptors,
  verbose = getOption("photobiology.verbose", TRUE),
  strict.calib = getOption("photobiology.strict.calib", FALSE),
  entrance.optics = NULL,
  ...
)
}
\arguments{
\item{date}{Any object that \code{anytime::anydate()} will decode as a date
or convert to a date. Used to select a descriptor containing calibration
data valid for a given day.}

\item{descriptors}{A named list of descriptors of the characteristics of
the spectrometer including calibration data.}

\item{verbose}{Logical indicating the level of warnings wanted.}

\item{strict.calib}{Logical indicating the level of validity checks.}

\item{entrance.optics}{character The name or geometry of the diffuser or
entrance optics to select. Only required when there are calibration with
multiple entrance optics for the same spectrometer.}

\item{...}{Currently ignored.}
}
\description{
Select from a list of instrument descriptors which one to use based on
date of measurement.
}
\details{
Calibrations for instruments stored in a list and passed as argument
  to \code{descriptors}, also store the dates between which they are valid.
  This function walks the list searching for a calibration valid for
  \code{date}. If no valid calibration is found and \code{strict.calib =
  FALSE}, the calibration valid closest in time is returned with a warning
  while if no valid calibration is found and \code{strict.calib = TRUE} an
  error is triggered.

  If a character string is passed as argument to \code{date}, it must be
  in a format suitable for \code{anytime::anydate()}. One needs to be
  careful with months and days of the month when supplying them as numbers,
  so using months names or their abbreviations can be safer.
}
\note{
The default argument for \code{verbose} is for this function
  \code{TRUE} as conversion of other objects to a date may fail.
}
\examples{
d1 <- which_descriptor()
d1$spectrometer.sn
d1$entrance.optics$geometry

d2 <- which_descriptor(entrance.optics = "hemispherical")
d2$spectrometer.sn
d2$entrance.optics$geometry

d3 <- which_descriptor(descriptors = ooacquire::MAYP112785_descriptors)
d3$spectrometer.sn
d3$entrance.optics$geometry

# gives error, as expected
# which_descriptor(descriptors = ooacquire::MAYP112785_descriptors,
#                  entrance.optics = "hemispherical")

# gives error
# which_descriptor(date = lubridate::now() + lubridate::years(1))

}
