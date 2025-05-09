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
#' @details Pixels affected directly, or by neighbourhood, by clipping, should
#'   be set to \code{NA} in each variable of CPS values, before passing the
#'   spectrum as argument to parameter \code{x} of this function.
#'
#'   The merging of the CPS variables starts from the one corresponding to the
#'   longest integration time, expected to contain the largest number of NA
#'   values, by replacing NA values by cps values from the variable
#'   corresponding to the next shorter integration. The procedure is repeated
#'   until no \code{NA} remains or until no shorter integration time data are
#'   available. \emph{The process stops when all NAs have been replaced, and,
#'   even when available, CPS values for unnecesarilly short integration times
#'   discarded.}
#'
#'   When measuring daylight different exposures for HDR are taken sequentially,
#'   and if light conditions change rapidly the cps values may be inconsistent.
#'   If the mean ratio of cps values is outside plus/minus the tolerance,
#'   instead of merging, the data for the longer of the two exposures is
#'   discarded instead of merged (or spliced) with the longer exposure, in which
#'   case a message is emitted. Ratio is computed after discarding low signal
#'   pixels as these readings are affected by noise, distorting the ratio when
#'   the light source-spectrum has only narrow peaks of emission.
#'
#' @param x cps_spct object
#' @param tolerance numeric Tolerance for mean deviation among cps columns as
#'   a fraction of one.
#' @param verbose Logical indicating the level of warnings and messages wanted.
#'
#' @export
#'
#' @return  a copy of x with values replaced as needed in all counts columns
#'   present.
#'
merge_cps <- function(x,
                      tolerance = 0.05,
                      verbose = getOption("photobiology.verbose", default = FALSE)) {
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

  merged <- logical(length(cols)) # allocate, filled with FALSE

  if (tolerance < 0) {
    # only the shortest integ.time or smaller num.exposures
    merged.cps <- x[[rev(cols)[1]]]
    merged[length(cols)] <- TRUE
  } else {
    merged.cps <- x[[cols[1]]]

    merged[1] <- TRUE
    for (i in 2:length(cols)) {
      to.replace.idx <- is.na(merged.cps)
      if (sum(!to.replace.idx) < 30 ||
          anyNA(merged.cps[x[["w.length"]] < 400])) {
        # if there is saturation in UV we replace the whole column
        merged.cps <- x[[cols[i]]]
        merged[1:(i - 1)] <- FALSE
      } else {
        # compute the ratio only from high-signal pixels as we are interested
        # in detecting changes in irradiance
        cps.qt <- stats:: quantile(merged.cps, probs = c(0.75, 0.9),
                                   names = FALSE, na.rm = TRUE)
        ratio.idx <- !is.na(x[[cols[i]]]) & !to.replace.idx &
          merged.cps > cps.qt[1] & merged.cps < cps.qt[2]
        if (sum(ratio.idx) > 40) {
          columns.ratio <-
            sum(x[[cols[i]]][ratio.idx]) / sum(merged.cps[ratio.idx])
        } else {
          columns.ratio <- 1
        }
        # replace clipped pixels only if cps consistent across columns
        if (columns.ratio > (1 - tolerance) && columns.ratio < (1 + tolerance)) {
          merged.cps[is.na(merged.cps)] <- x[[cols[i]]][is.na(merged.cps)]
          if (verbose) {
            message("HDR CPS ratio = ", signif(columns.ratio, 3), "; merging '",
                    cols[i - 1], "' with '", cols[i], "'.")
          }
        } else {
          message("HDR CPS ratio = ", signif(columns.ratio, 3),
                  "; replacing '",
                  cols[i - 1], "' by '", cols[i], "' instead of splicing.")
          merged.cps <- x[[cols[i]]]
          merged[1:(i - 1)] <- FALSE
        }
      }
      merged[i] <- TRUE
      if (!anyNA(merged.cps)) {
        if (verbose && i < length(cols)) {
          cat("HDR: splicing completed early at", cols[i], "!\n")
        }
        break()
      }
    }
  }

  z <- cps_spct(w.length = x[["w.length"]], cps = merged.cps)
  z <- photobiology::copy_attributes(x, z)
  attr(x = z, which = "merged.cps.cols") <- cols[merged]
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
  # as NAs are mostly contiguous using rle() should speed up the code
  for (i in counts.cols) {
    if (!anyNA(x[[i]])) {
      next()
    }
    rle.na <- rle(is.na(x[[i]]))
    rle.idx <- which(rle.na$values)
    vec.idx <- cumsum(rle.na$lengths)
    if (vec.idx[1] < n) {
      vec.idx[1] <- n
    }
    if (vec.idx[length(vec.idx)] < nrow(x) - n) {
      vec.idx[length(vec.idx)] <- nrow(x) - n
    }
    for (j in rle.idx) {
      window.bleed <- (vec.idx[j - 1] - n + 1):(vec.idx[j] + n)
      x[window.bleed, i] <- NA
    }
  }
  x
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
#' @param tol.margin numeric A multiplier applied to MAD, the default 1
#'   corresponds to 1 pixel per 1000 pixels as the expected random Normal noise.
#' @param max.hot,max.cold integer Maximum number of hot and cold pixels
#'   exceeding the tolerance per 1000 array pixels.
#' @param spct.label character A character string to use in message.
#' @param verbose logical If true a message is emitted in addition to returning
#'   the outcome.
#' @param QC.enabled logical If FALSE return NA skipping QC.
#'
#' @note Disabling the quality control is necessary when the "dark" reference is
#'   a measurement of ambient light instead of true darkness; i.e., when the
#'   irradiance of one light source is measured as the difference between
#'   background illumination and background illuminations plus the target
#'   light source.
#'
#' @return A logical value.
#'
#' @export
#'
QC_dark <-
  function(x,
           range = NULL,
           tol.margin = 1,
           max.hot = 60,
           max.cold = 20,
           spct.label = "Spectrum",
           verbose = getOption("photobiology.verbose", default = TRUE),
           QC.enabled = getOption("ooacquire.qc.enabled", default = TRUE)) {
  stopifnot(is.raw_spct(x) || is.cps_spct(x))
  if (!QC.enabled) {
    return(NA)
  }
  if (!is.null(range)) {
    x <- clip_wl(x = x, range = range)
  }
  cols <- setdiff(colnames(x), "w.length")
  # scales tolerance to approx. 3 SD, or P = 0.001
  tol.margin <- tol.margin * 4.5

  num.hot <- 0
  num.cold <- 0
  for (col in cols) {
    if (!is.numeric(x[[col]])) next()
    # compute the median and a weighted mad approx. 3 * SD * tol.margin
    median.cps <- stats::median(x[[col]], na.rm = TRUE)
    wt.mad.cps <- stats::mad(x[[col]], center = median.cps,
                             constant = tol.margin, na.rm = TRUE)
    num.hot <- max(num.hot,
                   sum(x[[col]] > median.cps + wt.mad.cps, na.rm = TRUE))
    num.cold <- max(num.cold,
                    sum(x[[col]] < median.cps - wt.mad.cps, na.rm = TRUE))

  }

  # express as counts per 1000 pixels
  num.hot <- round(num.hot * 1000 / length(x[[1]]))
  num.cold <- round(num.cold * 1000 / length(x[[1]]))
  if (num.hot > max.hot || num.cold > max.cold) {
    warning(spct.label, " failed QC: ", num.hot, " hot, ", num.cold, " cold per 1000 pixels",
            call. = FALSE, immediate.	= TRUE, domain = NA)
    cat("\"Dark\" reference not measured in darkness (=o.k.?) or spectrometer too warm (=bad data!)\n")
    FALSE
  } else {
    if (verbose) {
      cat(spct.label, " passed QC: ", num.hot, " hot, ", num.cold, " cold per 1000 pixels\n", sep = "")
    }
    TRUE
  }
}

