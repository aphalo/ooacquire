% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oo-calibration.R
\name{oo_calib2irrad_mult}
\alias{oo_calib2irrad_mult}
\title{Convert an OO calibration}
\usage{
oo_calib2irrad_mult(x, area = NULL, diff.type = NULL,
  verbose = getOption("photobiology.verbose", default = FALSE))
}
\arguments{
\item{x}{generic_spct object with variables \code{w.length} and \code{oo.cal}.}

\item{area}{numeric area of the cosine diffuser (mm2).}

\item{diff.type}{character value giving type of diffuser as in OO's documents, case
insensitive.}

\item{verbose}{Logical indicating the level of warnings wanted.}
}
\value{
a \code{generic_spct} object  of the same length as \code{x} containing
the re-scaled calibration factors in variable \code{coeffs}.
}
\description{
Convert an irradiance calibration as supplied by Ocean Optics into the
format used by the functions in this package. #'
}