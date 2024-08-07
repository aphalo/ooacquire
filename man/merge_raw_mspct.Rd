% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge-raw-mspct.r
\name{merge_raw_mspct}
\alias{merge_raw_mspct}
\title{Merge raw spectra into a single multicolumn spectrum}
\usage{
merge_raw_mspct(x)
}
\arguments{
\item{x}{raw_mspct}
}
\description{
Member spectra are sorted according to integration time stored in the
\code{inst.settings} attribute and merged into a single \code{raw_spct}
object with raw counts variables named \code{counts_1}, \code{counts_2},
etc.
}
\note{
The individual \code{raw_spct} objects contained in \code{x} must have
  identical values in \code{w.length}.
}
\seealso{
Other spectral data-processing functions: 
\code{\link{MAYP112785_tail_correction}()},
\code{\link{MAYP11278_tail_correction}()},
\code{\link{check_sn_match}()},
\code{\link{linearize_counts}()},
\code{\link{new_correction_method}()},
\code{\link{ref_correction}()},
\code{\link{trim_counts}()},
\code{\link{uvb_corrections}()}
}
\concept{spectral data-processing functions}
