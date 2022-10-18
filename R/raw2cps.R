#' Convert raw detector counts into counts per second (cps).
#'
#' These functions simply divide "raw counts" by the integration time used
#' for acquisition.
#'
#' @param x an R object
#' @param ... not used in current version
#'
#' @family functions for conversion of raw-counts data
#'
#' @export raw2cps
#'
#' @note In the case of objects of class "raw_spct" the columns with names
#' starting with "counts" are processed and renamed to columns with names
#' starting with "cps". All other columns are left unchanded.
#'
raw2cps <- function(x, ...) UseMethod("raw2cps")

#' @describeIn raw2cps Default method
#'
#' @export
#'
raw2cps.default <- function(x, ...) {
  cps_spct()
}

#' @describeIn raw2cps Method for spectral data expressed as raw instrument counts.
#'
#' @export
#'
#' @return an object of class "cps_spct"
#'
raw2cps.raw_spct <- function(x,
                             ...) {
  acq_settings <- getInstrSettings(x)

  # guard against attempts to reapply linearization
  if (!acq_settings[["linearized"]]) {
    x <- linearize_counts(x)
  }
  integ.time <- acq_settings[["integ.time"]]
  counts.cols <- names(x)[grep("^counts", names(x))]
  stopifnot(length(counts.cols) == length(integ.time))
  cps.cols <- gsub("^counts", "cps", counts.cols)
  if (exists("num.exposures", acq_settings)) {
    num.exposures <- acq_settings[["num.exposures"]]
  } else {
    num.exposures <- -1L
  }
  if (length(num.exposures) == 1L) {
    num.exposures <- rep(num.exposures, length(integ.time))
  } else {
    stopifnot(length(num.exposures) == length(integ.time))
    # consistently spectral irradiance (-1L) or spectral fluence (>= 0L)
    stopifnot(all(num.exposures < 0L) || all(num.exposures >= 0L))
  }
  other.cols <- setdiff(names(x), counts.cols)
  z <- as.generic_spct(x)[other.cols]
  max.counts <- numeric(length(counts.cols))
  for (i in seq_along(counts.cols)) {
    max.counts[i] <- max(x[[counts.cols[i]]], na.rm = TRUE)
    if (num.exposures[i] >= 1L) {
      # counts per flash
      z[[cps.cols[i]]] <- x[[counts.cols[i]]] / num.exposures[i]
      z <- setCpsSpct(z, time.unit = "exposure") # tag as counts per exposure!!
    } else if (num.exposures[i] < 0L) {
      # counts per second
      z[[cps.cols[i]]] <- x[[counts.cols[i]]] / integ.time[i] * 1e6
      z <- setCpsSpct(z, time.unit = "second")
    }
  }
  colnames(z)[-1] <- cps.cols # needed for 'photobiology' >= 0.10.14
  descriptor <- getInstrDesc(x)
  setInstrDesc(z, descriptor)
  if (length(acq_settings[["rel.signal"]]) == 0L || is.na(acq_settings[["rel.signal"]])) {
    # e.g. when read from files
    acq_settings[["rel.signal"]] <-
      min(max.counts, na.rm = TRUE) / descriptor[["max.counts"]]
  }
  z <- copy_attributes(x, z)
  setInstrSettings(z, acq_settings)
  z
}

#' @describeIn raw2cps Method for collections of raw-counts spectra
#'
#' @export
#'
#' @return an object of class "cps_mspct"
#'
raw2cps.raw_mspct <- function(x, ...) {
  if (length(x) == 1) {
    # temporary fix for bug in photobiology::msmsply()
    z <- cps_mspct(list(raw2cps(x[[1]], ...)))
    names(z) <- names(x)
    z
  } else {
    msmsply(x, raw2cps, ...)
  }
}






