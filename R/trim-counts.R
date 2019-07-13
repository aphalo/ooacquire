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
      warning("Negative raw counts in data!\n",
              "These are not raw detector counts.")
    }
    x[[col]] <- ifelse(x[[col]] < range[1] | x[[col]] > range[2],
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
#' Replace NAs in longer integration time cps with cps values from shorter
#' integration.
#'
#' @param x cps_spct object
#'
#' @export
#'
#' @return  a copy of x with values replaced as needed in all counts columns
#'   present.
#'
merge_cps <- function(x) {
  stopifnot(is.cps_spct(x))
  counts.cols <- grep("^cps", names(x), value = TRUE)
  if (length(counts.cols) == 1) {
    return(x)
  }
  instr.desc <- getInstrDesc(x)
  instr.settings <- getInstrSettings(x)
  integ.times <- instr.settings[["integ.time"]]
  num.exposures <- instr.settings[["num.exposures"]]
  if (all(num.exposures < 0L)) {
    cols <- counts.cols[order(integ.times, decreasing = TRUE)]
  } else {
    cols <- counts.cols[order(num.exposures, decreasing = TRUE)]
  }
  x[["cps"]] <- x[[cols[1]]]
  for (i in 2:length(cols)) {
    x[["cps"]] <- ifelse(is.na(x[["cps"]]),
                          x[[cols[i]]],
                          x[["cps"]] )
  }
  z <- x[ , c("w.length", "cps")]
  z <- photobiology::copy_attributes(x, z)
  z
}

#' Expand NA's to neighbouring pixels
#'
#' Replace "good" data from pixels adyacent to NAs with NAs as data from pixels
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
#'   the moment we use a symetric window although "blooming" could be asymetric.
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
