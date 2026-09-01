#' Compute spectral irradiance update
#'
#' Compute spectral irradiance using the instrument descriptor for the same
#' spectrometer and date of measurement from the current version of 'ooacquire'
#' rather than that available at the time when raw data were acquired.
#'
#' @inheritParams s_irrad_corrected
#' @param y source_spct Object containing spectral irradiance earlier
#'   computed from the same \code{raw_mspct} object as passed to \code{x}.
#' @param which,which.not character Names of attributes to copy or
#'  not to copy.
#' @param verbose logical Enable informative messages.
#'
#' @inherit s_irrad_corrected return
#'
#' @family functions for conversion of raw-counts data
#' @seealso copy_attributes
#'
#' @details When raw counts data are acquired with
#'   \code{acq_irrad_interactive()} or imported from text files from other
#'   software the calibration information is stored together with the data. When
#'   computing spectral irradiance with function \code{s_irrad_corrected()} at
#'   any later time, this stored calibration data are the ones used. This
#'   approach ensures reproducibility as calibrations can be, and have been,
#'   retrospectively modified when 'ooacquire' is updated. In most cases the
#'   updates have only added information about "hot" and "dead" pixels. However,
#'   in one case the range of the calibration data included was incomplete, and
#'   later expanded. These changes can be important in some cases, making it
#'   necessary or preferable to use the updated calibrations when computing
#'   spectral irradiance.
#'
#'   In \code{s_irrad_corrected_updt()} the \code{instr.desc} attribute in
#'   \code{x} is matched based on spectrometer serial number, date and entrance
#'   optics to the possibly updated version of the same calibration data in the
#'   current version of 'ooacquire'. The instrument descriptor is replaced in a
#'   local copy of \code{x} and this local copy used to compute spectral
#'   irradiance with \code{s_irrad_corrected()}, without altering \code{x}.
#'   Explicit arguments passed to parameters in the call to
#'   \code{s_irrad_corrected_updt()} are used in the call to
#'   \code{s_irrad_corrected()}, however, defaults are in some cases different.
#'
#'   If an argument is passed to parameter \code{y} and the value of the
#'   \code{when.measured} attribute and spectrometer serial number match between
#'   \code{x} and \code{y}, default arguments for \code{return.cps} and
#'   \code{correction.method} are based on metadata from  \code{y}. In this case
#'   attributes unrelated to to those in the raw data or their conversion,
#'   possibly added or modified after \code{y} was computed, are copied to the
#'   returned \code{source_spct} object using method
#'   \code{\link[photobiology]{copy_attributes}()} passing \code{which} and
#'   \code{which.not} in the call. This creates, as close as possible an updated
#'   version of \code{y}. Attributes \code{"instr.desc"},
#'   \code{"instr.settings"}, \code{"when.measured"} and several others set by
#'   \code{s_irrad_corrected()} are never updates in the returned value to
#'   ensure they remain valid. Other attributes including
#'   \code{"what.measured"}, \code{"where.measured"}, \code{"how.measured"},
#'   \code{"comment"}, are copied by default. If names are passed in an argument
#'   to \code{which} only these attributes are copied, with user-set attributes
#'   allowed. However, exclusion of attributes named in the union of the
#'   argument passed to \code{which.not} and those not excluded by default takes
#'   precedence.
#'
#' @note Entrance optics metadata have been saved in the instrument descriptor
#'   only since 'ooacquire' (>= 0.5.3) and correction method only from 'ooacquire'
#'   (>= 0.5.6).
#'
#' @return A \code{source_spct} or a \code{cps_spct} object.
#'
#' @export
#'
#' @examples
#'
#' # only metadata from x and the new descriptor
#' s_irrad_corrected_updt(x = white_grow_LED.raw_mspct)
#'
s_irrad_corrected_updt <- function(x,
                                   y = NULL,
                                   spct.names = c(light = "light",
                                                  filter = "filter",
                                                  dark = "dark"),
                                   correction.method = NULL,
                                   hdr.tolerance = getOption("ooacquire.hdr.tolerance", default = 0.05),
                                   return.cps = NULL,
                                   trim.descriptor = NULL,
                                   which = NULL,
                                   which.not = NULL,
                                   verbose = getOption("photobiology.verbose",
                                                       default = FALSE)) {
  if (!photobiology::is.raw_mspct(x)) {
    stop("Argument passed to 'x' should be a 'raw_mspct' ",
         "but its class is: ", class(x)[1])
  }
  if (length(y) && !photobiology::is.source_spct(y)) {
    stop("Argument passed to 'y' should be a 'source_spct' ",
         "but its class is: ", class(x)[1])
  }

  # new descriptor is selected based on the existing one
  x.descriptor <- photobiology::instr_descriptor(x[[1]])

  # entrance optics field is part of the fetched descriptor
  # the geometry is used, possibly unwisely, to select the correct
  # descriptor out of multiple ones available for a given spectrometer
  if (!"entrance.optics" %in% names(x.descriptor)) {
    entrance.optics <- "cosine"
  } else {
    entrance.optics <- x.descriptor[["entrance.optics"]]["geometry"]
  }

  # fetch the descriptor used in current 'ooacquire'
  date <- photobiology::when_measured(x[[1]])
  current.descriptor <-
    default_descriptor(serial_no = x.descriptor[["spectrometer.sn"]],
                       entrance.optics = entrance.optics,
                       date = date)

  # assign the descriptor to the raw_spct objects
  x <- photobiology::msmsply(x,
                             photobiology::setInstrDesc,
                             instr.desc = current.descriptor)

  # fetch correction method
  if (is.null(correction.method)) {
    if (!is.null(y) &&
        !is.null(attr(y, "correction.method", exact = TRUE))) {
      correction.method <- attr(y, "correction.method", exact = TRUE)
      if (verbose) {
        message("Using 'correction.method' from 'y'")
      }
    } else {
      correction.method <- default_method(serial_no = x.descriptor[["spectrometer.sn"]],
                                          descriptor = current.descriptor)
      if (verbose) {
        message("Using default 'correction.method'")
      }
    }
  } else if (verbose) {
    message("Using 'correction.method' from call argument")
  }

  # compute spectral irradiance
  return.cps <- ifelse(is.null(return.cps),
                       !is.null(y) && photobiology::is.cps_spct(y),
                       return.cps)
  trim.descriptor <- ifelse(is.null(trim.descriptor),
                            !return.cps,
                            trim.descriptor)
  z <-
    s_irrad_corrected(x = x,
                      spct.names = spct.names,
                      correction.method = correction.method,
                      hdr.tolerance = hdr.tolerance,
                      return.cps = return.cps,
                      trim.descriptor = trim.descriptor,
                      verbose = verbose)

  # add/replace metadata present in y to the freshly computed one
  # attributes such as what.measured, where.measured and when.measured
  if (!is.null(y)) {
    y.descriptor <- photobiology::instr_descriptor(y)
    matched <-
      y.descriptor[["spectrometer.sn"]] == x.descriptor[["spectrometer.sn"]] &&
      photobiology::when_measured(y) == photobiology::when_measured(z)

    which.not <- union(which.not,
                       c("instr.desc",
                         "instr.settings",
                         "spct.version",
                         "when.measured",
                         "multiple.wl",
                         "time.unit",
                         "straylight.corrected",
                         "slit.corrected",
                         "bswf.used",
                         "QC_dark_pass",
                         "normalized",
                         "normalization",
                         "scaled"))
    if (matched) {
      z <- photobiology::copy_attributes(x = y, y = z,
                                         which.not = which.not,
                                         which = which)
    } else {
      warning("Spectra in 'x' and 'y' are mismatched! Metadata not updated!")
    }
  } else if (verbose) {
    message("No metadata copied as no argument was passed to 'y'")
  }
  z
}
