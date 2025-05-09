% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acq-interactive-utils.R
\name{f.trigger.message}
\alias{f.trigger.message}
\title{Manual trigger request}
\usage{
f.trigger.message(n = 1L, delay = 0)
}
\arguments{
\item{n}{integer Number of pulses (flashes) to trigger per call.}

\item{delay}{numeric Time in seconds to wait before displaying the message.}
}
\description{
This function is used by default. It prints a message asking the operator
to manually trigger a flash. A more elaborate function, using specific
hardware can be used to automatically trigger a light source or some other
synchronized event such as releasing the shutter of a camera.
}
\note{
When using this function, set an integration time that gives enough
  time for the manual triggering of the flash to reliably fall within the
  integration.
}
\seealso{
\code{\link[base]{Sys.sleep}} for a description of the variation
  in the effective duration of the \code{delay}.

Other interactive acquisition utility functions: 
\code{\link{choose_ch_interactive}()},
\code{\link{choose_sr_interactive}()},
\code{\link{list_srs_interactive}()},
\code{\link{protocol_interactive}()},
\code{\link{set_attributes_interactive}()},
\code{\link{set_folder_interactive}()},
\code{\link{set_seq_interactive}()},
\code{\link{set_session_name_interactive}()},
\code{\link{set_user_name_interactive}()},
\code{\link{set_wlrange_interactive}()},
\code{\link{tune_interactive}()}
}
\concept{interactive acquisition utility functions}
