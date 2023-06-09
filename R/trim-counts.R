#' Replace out-of-range instrument counts
#'
#' Trim out-of-range counts values in data stored in a "raw_spct" object. The
#' values are replaced with the supplied \code{fill} value.
#'
#' @param x raw_spct object
#' @param range integer vector of length two
#' @param fill an integer value (including NA) to be used to replace the values
#'   that are outside range.
#'
#' @family spectral data-processing functions
#'
#' @export
#'
#' @return a copy of x with values replaced as needed in all counts columns
#'   present.
#'
trim_counts <- function(x,
                        range = c(NA, getInstrDesc(x)[["max.counts"]] - 1),
                        fill = NA) {
  stopifnot(is.raw_spct(x))
  if (is.na(range[1])) {
    range[1] <- -Inf
  }
  if (is.na(range[2])) {
    range[2] <- Inf
  }
  counts.cols <- grep("^counts", names(x), value = TRUE)
  for (col in counts.cols) {
    if (min(x[[col]]) < 0) {
      if (min(x[[col]]) < 100) {
        warning("Negative raw counts in data!\n",
                "These are not true raw detector counts.\n",
                "A dark correction may have been applied.\n")
      }
      shift <- min(x[[col]])
    } else {
      shift <- 0
    }
    x[[col]] <- ifelse(x[[col]] < range[1] | x[[col]] > range[2] + shift,
                       fill,
                       x[[col]])
  }
  x
}

#' Replace bad pixels
#'
#' Replace bad pixels with the average of the counts from the two neighbouring
#' pixels.
#'
#' @param x raw_spct object
#'
#' @export
#'
#' @return  a copy of x with values replaced as needed in all counts columns
#'   present.
#'
skip_bad_pixs <- function(x) {
  stopifnot(is.raw_spct(x))
  bad.pixs <- getInstrDesc(x)[["bad.pixs"]]
  if (length(bad.pixs) == 0) {
    # nothing to do
    return(x)
  }
  counts.cols <- grep("^counts", names(x), value = TRUE)
  for (col in counts.cols) {
    x[bad.pixs, col] <-
      (x[bad.pixs - 1, col] + x[bad.pixs + 1, col]) / 2
  }
  x
}

#' Merge counts per second data
#'
#'
#' In a \code{cps_spct} object with multiple columns of CPS data, each acquired
#' using a different integration time, merge columns into a single column.
#'
#' @details Pixels affected directly or by neighbourhood by clipping, should be
#' set to \code{NA} before passing the spectrum as argument to this function.
#' Starting from the variable corresponding to the longest integration time, NA
#' values are replaced by cps values from the next shorter integration. The
#' procedure is repeated until no \code{NA} remains or until no shorter
#' integration time data are available.
#'
#' When measuring daylight different exposures for HDR are taken sequentially,
#' and if light conditions change rapidly the cps values may be inconsistent. If
#' the mean ratio of cps values is outside plus/minus the tolerance, instead of
#' merging, the data for the longer of the two exposures is discarded instead of
#' merged (or spliced) with the longer exposure, in which case a message is
#' emitted.
#'
#' @param x cps_spct object
#' @param tolerance numeric Tolerance for mean deviation among cps columns as
#'   a fraction of one.
#'
#' @export
#'
#' @return  a copy of x with values replaced as needed in all counts columns
#'   present.
#'
merge_cps <- function(x,
                      tolerance = 0.10) {
  stopifnot(is.cps_spct(x))
  counts.cols <- grep("^cps", names(x), value = TRUE)
  if (length(counts.cols) == 1) {
    return(x)
  }
  instr.desc <- getInstrDesc(x)
  instr.settings <- getInstrSettings(x)
  integ.times <- instr.settings[["integ.time"]]
  num.exposures <- instr.settings[["num.exposures"]]
  if (all(num.exposures < 0L)) { # -1L is used for continuous light
    cols <- counts.cols[order(integ.times, decreasing = TRUE)]
  } else {
    cols <- counts.cols[order(num.exposures, decreasing = TRUE)]
  }
  x[["cps"]] <- x[[cols[1]]]

  for (i in 2:length(cols)) {
    to.replace.idx <- is.na(x[["cps"]])
    if (sum(!to.replace.idx) < 30 ||
        anyNA(x[["cps"]][x[["w.length"]] < 400])) {
      # if there is saturation in UV we replace the whole column
      x[["cps"]] <- x[[cols[i]]]
    } else {
      # replace only clipped pixels if cps consistent across columns
      columns.ratio <-
          sum(x[[cols[i]]][!to.replace.idx]) / sum(x[["cps"]][!to.replace.idx])
      if (columns.ratio > (1 - tolerance) && columns.ratio < (1 + tolerance)) {
        x[["cps"]] <- ifelse(is.na(x[["cps"]]),
                             x[[cols[i]]],
                             x[["cps"]] )
      } else {
        message("Inconsistent cps in HDR exposures, replacing instead of merging")
        x[["cps"]] <- x[[cols[i]]]
      }
    }
    if (!anyNA(x[["cps"]])) {
      break()
    }
  }
  z <- x[ , c("w.length", "cps")]
  z <- photobiology::copy_attributes(x, z)
  z
}

#' Expand NA's to neighbouring pixels
#'
#' Replace "good" data from pixels adjacent to NAs with NAs as data from pixels
#' not saturated but located in the neighbourhood of saturated pixels can return
#' unreliable data. This correction is needed by a phenomenon similar to
#' "blooming" in camera sensors whereby when a sensor well gets saturated some
#' of the charge migrates to adjacent wells in the detector increasing their
#' readings.
#'
#' @param x raw_spct object
#' @param n integer Number of pixels to set to NAs.
#'
#' @export
#'
#' @return  a copy of x with values replaced by NAs as needed in all counts
#'   columns present.
#'
#' @note Avoid using very large n values as n pixels at each end of the array
#'   are assumed not to be ever saturated. The value of n needed for each
#'   detector type/instrument needs to be found through testing. As rule of
#'   thumb use 5 < n < 10 for Sony's ILxxx and 8 < n < 14 for Hamamatsu xxxx. At
#'   the moment we use a symmetric window although "blooming" could be asymmetric.
#'
bleed_nas <- function(x, n = 10) {
  stopifnot(is.raw_spct(x))
  counts.cols <- grep("^counts", names(x), value = TRUE)
  # this is a "quick and dirty" algorithm that assumes that we do not need to
  # check for NAs the first n and last n pixels of the detector array, which in
  # practice are almost never used for anything but dark reference and so very
  # unlikely to be exposed to an irradiance saturating their response.
  z <- x
  for (i in counts.cols) {
    for (j in n:(nrow(x) - n)) {
      if (anyNA(x[(j-n):(j+n), i])) {
        z[j, i] <- NA
      }
    }
  }
  z
}

#' Quality control of dark spectra
#'
#' Function used to check the number of pixels that deviate from the median.
#'
#' @details The expectation is that in a spectrum measured in the dark, to be
#'   used as a reference, the variation among pixel counts is small. This also
#'   applies in some cases to ranges of pixels protected from radiation by a
#'   long pass or short pass filter. By default the whole spectrum is included
#'   in the QC check, but if needed an argument can be passed to \code{range}
#'   to select a smaller region.
#'
#' @param x cps_spct or raw_spct A spectrum measured in darkness.
#' @param range numeric A wavelength range [nm].
#' @param tol.margin numeric in [0..small integer] indicating the tolerance
#'   margin as a fraction of the median counts.
#' @param max.hot,max.cold integer Maximum number of hot and cold pixels
#'   accepted.
#' @param spct.label character A character string to use in message.
#' @param verbose logical If true a message is emitted in addition to returning
#'   the outcome.
#'
#' @return A logical value.
#'
#' @export
#'
QC_dark <-
  function(x,
           range = NULL,
           tol.margin = 0.5,
           max.hot = 10,
           max.cold = 10,
           spct.label = "Spectrum ",
           verbose = getOption("photobiology.verbose", default = TRUE)) {
  stopifnot(is.raw_spct(x) || is.cps_spct(x))
  if (!is.null(range)) {
    x <- clip_wl(x = x, range = range)
  }
  cols <- setdiff(colnames(x), "w.length")
  num.hot <- 0
  num.cold <- 0
  for (col in cols) {
    if (!is.numeric(x[[col]])) next()
    median.cps <- stats::median(x[[col]], na.rm = TRUE)
    num.hot <- max(num.hot,
                   sum(x[[col]] > median.cps * (1 + tol.margin), na.rm = TRUE))
    num.cold <- max(num.cold,
                    sum(x[[col]] < median.cps * (1 - tol.margin), na.rm = TRUE))

  }
  if (num.hot > max.hot || num.cold > max.cold) {
    warning(spct.label, " failed QC: ", num.hot, " hot, ", num.cold, " cold pixels",
            call. = FALSE, immediate.	= TRUE, domain = NA)
    FALSE
  } else {
#    message(spct.label, " passed QC: ", num.hot, " hot, ", num.cold, " cold pixels")
    TRUE
  }
}

