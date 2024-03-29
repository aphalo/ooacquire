% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-R.R
\name{new_correction_method}
\alias{new_correction_method}
\title{Make default method from descriptor}
\usage{
new_correction_method(
  descriptor,
  stray.light.method = "none",
  stray.light.wl = c(NA_real_, NA_real_),
  flt.dark.wl = c(NA_real_, NA_real_),
  flt.ref.wl = c(NA_real_, NA_real_),
  flt.Tfr = NA_real_
)
}
\arguments{
\item{descriptor}{list, as returned by \code{get_oo_descriptr()}}

\item{stray.light.method}{character Name of method.}

\item{stray.light.wl, flt.dark.wl, flt.ref.wl}{numeric vector with wavelengths (nm).}

\item{flt.Tfr}{numeric Transmittance of filter as a fraction (0...1).}
}
\value{
a list
}
\description{
A function that builds a default method from an instrument descriptor.
Useful when the spectrometers has not been characterized as needed for
the more sofisticated methods. Can use stray light correction
but not slit function correction. Stray light correction is valid
only if it was also used during irradiance callibration. Suitablity
of wavelengths and method depends on the instrument configuration so
they are set to \code{NA} as default..
}
\details{
The currently recognized methods for stray-light correction are
\code{"none"}, \code{"original"}, \code{"sun"}, and \code{"simple"}. With
the default method \code{"none"}, the values of the remaining parameters
are ignored.
}
\note{
Defaults for indexes are for the first channel of the first
  spectrometer currently connected.
}
\seealso{
Other spectral data-processing functions: 
\code{\link{MAYP112785_tail_correction}()},
\code{\link{MAYP11278_tail_correction}()},
\code{\link{check_sn_match}()},
\code{\link{linearize_counts}()},
\code{\link{merge_raw_mspct}()},
\code{\link{ref_correction}()},
\code{\link{trim_counts}()},
\code{\link{uvb_corrections}()}
}
\concept{spectral data-processing functions}
