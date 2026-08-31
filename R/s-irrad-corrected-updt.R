#' Compute spectral irradiance update
#'
#' Compute spectral irradiance using the instrument descriptor
#' for the same spectrometer and date of measurement using the
#' calibration data in the current version of 'ooacquire' rather
#' than that available at the time when raw data were acquired.
#'
#' @inheritParams s_irrad_corrected x
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
#' @details When raw counts data are acquired the calibration information is
#'   stored together with the data. When computing spectral irradiance with
#'   function \code{s_irrad_corrected()} at any later time, this stored
#'   calibration is the one used. This approach ensures reproducibility as
#'   calibrations can be, and have been, retrospectively modified when
#'   'ooacquire' is updated. In most cases the updates have only added
#'   information about "hot" and "dead" pixels. However, in one case the range
#'   of the calibration data included was incomplete, and later expanded. These
#'   changes can be important in some cases, making it necessary or preferable
#'   to use the updated calibrations when computing spectral irradiance.
#'
#'   In \code{s_irrad_refresh()} the \code{instr.desc} attribute in \code{x} is matched
#'   based on spectrometer serial number, date and entrance optics
#'   to the current, and possibly updated, version of the same calibration
#'   data in the current version of 'ooacquire'. The instrument descriptor
#'   is replaced in a local copy of \code{x} and this local copy used to
#'   computed spectral irradiance with \code{s_irrad_corrected()}, without
#'   altering \code{x}.
#'
#'   If an argument is passed to parameter \code{y} and the value of the
#'   \code{when.measured} attribute and spectrometer serial number match between
#'   \code{x} and \code{y}, attributes, possibky added or modified after
#'   \code{y} was computed, are copied to the returned \code{source_spct} object
#'   using method \code{\link[photobiology]{copy_attributes}()} passing
#'   \code{which} and \code{which.not} in the call. This creates, as close as
#'   possible an updated version of \code{y}. Attributes \code{"instr.desc"},
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
#' @export
#'
#' @examples
#'
#' # only metadata from x and the new descriptor
#' s_irrad_corrected_updt(x = white_LED.raw_mspct)
#'
#' # metadata from x and the new descriptor updated to some metadata from y
#' s_irrad_corrected_updt(x = white_LED.raw_mspct, y = white_led.source_spct)
#'
s_irrad_corrected_updt <- function(x,
                                   y = NULL,
                                   which = NULL,
                                   which.not = NULL,
                                   verbose = getOption("photobiology.verbose",
                                                       default = FALSE)) {
  if (!is.raw_mspct(x)) {
    stop("Argument passed to 'x' should be a 'raw_mspct' ",
         "but its class is: ", class(x)[1])
  }
  if (length(y) && !is.source_spct(y)) {
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
  # fetch the descriptor and correction methods used in current 'ooacquire'
  current.defaults <-
    instr_defaults(serial_no = x.descriptor[["spectrometer.sn"]],
                   entrance.optics = entrance.optics,
                   date = photobiology::when_measured(x[[1]]))

  # assign the descriptor to the raw_spct objects
  photobiology::instr_descriptor(x) <- current.defaults$descriptor

  # compute spectral irradiance
  z <-
    s_irrad_corrected(x = x,
                      correction.method =
                        current.defaults$correction.method,
                      return.cps =
                        !is.null(y) &&
                        photobiology::is.cps_spct(y))

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
