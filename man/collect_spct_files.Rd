% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file-utils.R
\name{collect_spct_files}
\alias{collect_spct_files}
\title{Collect spectra into a collection}
\usage{
collect_spct_files(
  path = ".",
  range = NULL,
  method = NULL,
  strength = 0,
  name.root = "",
  na.rm = FALSE
)
}
\arguments{
\item{path}{a character string giving
the name of the folder from wich to load .Rda files containing spectra.}

\item{range}{a numeric vector of length two, or any other object for which
function range() will return two.}

\item{method}{a character vector of length 1 or 2, containing the smoothing
method to use, one of "custom",  "lowess", or "supsmu".}

\item{strength}{numeric value to adjust the degree of smoothing.}

\item{name.root}{character string to prepend to the name of the spectra in
the collection.}

\item{na.rm}{a logical value indicating whether NA values should be stripped
before the computation proceeds.}
}
\description{
Read .Rda files as saved during data acquisition and build a collection with
the spectra read, possibly first trimming the range of wavelengths and/or
smoothing the spectral data.
}
\note{
If the argument passed to \code{method} is of length 2, the first
  member will apply to \code{source_spct} objects and the second one to
  \code{filter_spct} and \code{reflector_spct} objects. A numeric vector
  of length 2 passed to \code{strength} is treated in the same way. This
  is only relevant when we are collecting spectra belonging to different
  classes and need to treat them differently with respect to smoohting. It
  also allows different defaults, as transmittance spectra tend to lack
  the fine structure of some emission spectra.
}
\seealso{
Function \code{\link[photobiology]{trim_wl}} is used to trim the
  range of the data to plot, and function
  \code{\link[photobiology]{smooth_spct}} is used for smoothing, and
  prameters \code{range}, and \code{strength} and \code{method} are passed to
  them, respectively.

Other functions for importing spectral data from files: 
\code{\link{map_oofile_header_rows}()},
\code{\link{oofile_data_rows}()},
\code{\link{plot_spct_file}()},
\code{\link{read_files2mspct}()},
\code{\link{read_oo_data}()},
\code{\link{read_oo_ovdata}()},
\code{\link{read_oo_pidata}()},
\code{\link{read_oo_ssdata}()},
\code{\link{set_oo_ssdata_descriptor}()},
\code{\link{set_oo_ssdata_settings}()}
}
\concept{functions for importing spectral data from files}
