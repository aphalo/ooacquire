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
#' derived quantities. This function can be applied only to \code{raw_spct}
#' objects created with functions from package 'ooacquire'.
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
#' @return a copy of \code{x} possibly with an updated \code{instr.desc}
#'   attribute embedded.
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
