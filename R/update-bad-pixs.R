#' Update bad-pixels in instrument descriptor
#'
#' Update field \code{bad.pixs} of the instrument descriptor embedded in
#' \code{raw_spct} and \code{raw_mspct} objects.
#'
#' @param x raw_spct or raw_mspct object with attribute \code{instr.desc} set.
#' @param bad.pixs numeric New vector of indexes to bad pixels in the detector
#'   array. If \code{NULL}, bad pixels are retrieved from calibration data in
#'   the current version of 'ooacquire'.
#' @param action character One of "replace" or "add".
#'
#' @details
#' Spectral objects, including those with raw counts data can contain an
#' instrument descriptor. One member of this attribute is a vector of indexes to
#' bad pixels. New bad pixels can be identified in some cases after data are
#' acquired. Recomputing of physical quantities from raw counts normally reuses
#' the embedded calibration data. Function \code{update_bad_pixs()} makes it
#' possible to update the embedded bad pixels information before recomputing
#' derived quantities. This function can be applied only to
#' \code{raw_spct} objects created with functions from package 'ooacquire'.
#'
#' With defaults arguments for formal parameters \code{bad.pixs} and
#' \code{action}, the \code{bad.pixs} field of the descriptor is updated to
#' match that in the matching calibration data in the version of 'ooacquire'
#' currently loaded. However, if a numeric vector to positions in the detector
#' array is passed as argument, depending on the argument passed to
#' \code{action}, this vector will be used either to replace the existing one,
#' or the indexes in the vector "added" to those already stored, using
#' \code{union()}.
#'
#' @note
#' Only objects of class \code{raw_spct}, individually or as members of a
#' \code{raw_mspct} object, are supported as the update must precede any
#' conversion into physical units, and will propagate to returned values when
#' computations are applied to the updated \code{raw_spct} objects.
#'
#' @return a copy of \code{x} with an updated \code{instr.desc} attribute
#'   embedded.
#'
#' @export
#'
update_bad_pixs <- function(x,
                            bad.pixs = NULL,
                            action = "replace") {
  if (is.raw_mspct(x)) {
    msmsply(x, .fun = update_bad_pixs, bad.pixs = bad.pixs, action = action)
  } else {
    if (!is.raw_spct(x)) {
      warning("'x' must be a 'raw_spct' object")
      return(x)
    }
    descriptor <- getInstrDesc(x)
    if (length(descriptor) == 0) {
      warning("Attribute 'inst.desc' is not set in 'x'")
      return(x)
    }
    if (!exists("inst.calib", descriptor)) {
      warning("No calibration data found in 'x'")
      return(x)
    }
    if (is.null(bad.pixs)) {
      inst.serial.number <- descriptor[["spectrometer.sn"]]
      when.measured <- when_measured(x)
      new.descriptor <-
        which_descriptor(date = when.measured,
                         descriptors = get(paste(inst.serial.number,
                                                 "descriptors", sep = "_")))
      bad.pixs <- new.descriptor[["bad.pixs"]]
    }
    if (action == "replace") {
      descriptor[["bad.pixs"]] <- bad.pixs
    } else if (action == "add") {
      descriptor[["bad.pixs"]] <- union(descriptor[["bad.pixs"]], bad.pixs)
    } else {
      stop("'action' must be one of \"replace\" or \"add\", not \"",
           action, "\".")
    }
    descriptor[["bad.pixs"]] <- sort(descriptor[["bad.pixs"]])
    setInstrDesc(x, descriptor)
  }
}

#' Remove java wrapper from descriptor
#'
#' Clean up left-behind wrappers used to communicate with Java code using
#' 'rJava'.
#'
#' @details
#' Field \code{w}  contains a wrapper on a Java object used to communicate with
#' the OmniDriver driver during data acquisition. Once the current connection
#' between R and a spectrometer ends, these wrappers are invalidated, becoming
#' useless. However, if present they create a dependency on 'rJava', possibly
#' triggering errors. In recent versions of 'ooacquire' this wrapper is removed
#' immediately after acquisition. However, the instrument descriptor of spectral
#' objects created with versions of 'ooacquire' for some years ago can contain a
#' member storing a useless Java wrapper. This function removes this field if
#' present.
#'
#' @note
#' Method \code{getInstrDesc()} removes member field \code{w} from the returned
#' value but does not modify its argument.
#'
#' @param x raw_spct or raw_mspct object with attribute \code{instr.desc} set.
#'
#' @return a copy of \code{x} possibly with an updated
#'   \code{instr.desc} attribute embedded.
#'
#' @export
#'
rm_jwrapper <- function(x) {
  if (is.raw_mspct(x)) {
    msmsply(x, .fun = rm_jwrapper)
  } else {
    if (!is.raw_spct(x)) {
      warning("'x' must be a 'raw_spct' object")
      return(x)
    }
    descriptor <- getInstrDesc(x)
    if (length(descriptor) == 0) {
      warning("Attribute 'inst.desc' is not set in 'x'")
      return(x)
    }
    if (exists("w", descriptor, inherits = FALSE)) {
      descriptor[["w"]] <- NULL
      x <- setInstrDesc(x, descriptor)
    }
    x
  }
}

#' Update whole instrument descriptor
#'
#' Update the instrument descriptor embedded in \code{raw_spct} and
#' \code{raw_mspct} objects.
#'
#' @param x raw_spct or raw_mspct object with attribute \code{instr.desc} set.
#' @param which.descriptor.args named list Of arguments to use to fetch the
#'   replacement descriptor.
#'
#' @details
#' Recomputing of physical quantities from raw counts normally reuses the
#' embedded calibration data in \code{\link[photobiology]{raw_spct}} objects.
#' Function \code{update_inst_desc()} makes it possible to update the embedded
#' calibration before recomputing derived quantities.
#'
#' Spectral objects, including those with raw counts data contain an
#' instrument descriptor when measured or imported using functions fron
#' 'ooacquire'. The descriptor can include calibration data as well
#' as details of the instrument used. If a calibration has been retroactively
#' modified after data acquisition, the descriptor can be refreshed in
#' existing \code{raw_spct} or \code{raw_mspct} objects. By default the
#' the version of the new descriptor is that in the loaded version of
#' 'ooacquire', corresponding to the same instrument and data acquisition date.
#'
#' If for some reason, an instrument descriptor for the wrong spectrometer,
#' for a wrong data acquisition date, or wrong entrance optics a non-matching
#' replacement can be requested by passing suitable arguments through parameter
#' \code{which.descriptor.arg}. See \code{\link{which_descriptor}} for the
#' details.
#'
#' @section Warning!: This function is intended only to repair objects that
#' have corrupted information. Typical cases are acquisition using a computer
#' with bad clock settings, unrecognized instrument serial number, or
#' measurements done before the valid calibration was added to 'ooacquire'.
#'
#' The special case when the only change to the calibration is adding newly
#' identified bad pixels in the sensor array, function
#' \code{\link{update_bad_pixs}} provides a "light weight" alternative.
#'
#' @note The replacement of the instrument descriptor in not supported for
#' spectral data not expressed as raw counts, because this create an
#' inconsistency between the data and its metadata. To achieve such a change
#' data in physical units need to be recomputed from raw instrument counts.
#'
#' @return a copy of \code{x} with an updated \code{instr.desc} attribute
#'   embedded.
#'
#' @export
#'
update_instr_desc <-
  function(x, which.descriptor.args = list()) {
  if (is.raw_mspct(x)) {
    msmsply(x,
            .fun = update_instr_desc,
            which.descriptor.args = which.descriptor.args)
  } else {
    if (!is.raw_spct(x)) {
      warning("'x' must be a 'raw_spct' object")
      return(x)
    }
    descriptor <- photobiology::getInstrDesc(x)
    if (length(descriptor) == 0) {
      stop("Attribute 'inst.desc' is not set in 'x'")
    }
    if (!exists("inst.calib", descriptor)) {
      warning("No calibration data found in 'x'")
    }
    if (is.null(which.descriptor.args$date)) {
      which.descriptor.args$date <- photobiology::when_measured(x)
    }
    if (is.null(which.descriptor.args$entrance.optics)) {
      if (exists("entrance.optics", descriptor)) {
        which.descriptor.args$entrance.optics <- descriptor$entrance.optics$geometry
      } else {
        which.descriptor.args$entrance.optics <- "cosine"
      }
    }
    if (is.null(which.descriptor.args$descriptors)) {
      inst.serial.number <- descriptor[["spectrometer.sn"]]
      which.descriptor.args$descriptors <-
        get(paste(inst.serial.number, "descriptors", sep = "_"))
    }

    new.descriptor <-
      do.call(which_descriptor, args = which.descriptor.args)

    photobiology::setInstrDesc(x, new.descriptor)
  }
}

