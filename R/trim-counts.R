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
                        range = c(0L, getInstrDesc(x)[["max.counts"]] - 1),
                        fill = NA) {
  if (is.na(range[1])) {
    range[1] <- -Inf
  }
  if (is.na(range[2])) {
    range[2] <- Inf
  }
  stopifnot(is.raw_spct(x))
  counts.cols <- grep("^counts", names(x), value = TRUE)
  for (col in counts.cols) {
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
  bad.pixs <- getInstrDesc(x)$bad.pixs
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
  instr.desc <- getInstrDesc(x)
  instr.settings <- getInstrSettings(x)
  counts.cols <- grep("^cps", names(x), value = TRUE)
  integ.times <- getInstrSettings(x)$integ.time
  cols <- counts.cols[order(integ.times, decreasing = TRUE)]
  x[["cps"]] <- x[[cols[1]]]
  for (i in 2:length(cols)) {
    x[["cps"]] <- ifelse(is.na(x[["cps"]]),
                          x[[cols[i]]],
                          x[["cps"]] )
  }
  x <- x[ , c("w.length", "cps")]
  setInstrDesc(x, instr.desc)
  setInstrSettings(x, instr.settings)
  x
}

#' Expand NA's to neighbouring pixels
#'
#' Replace "good" data from pixels adyacent to NAs with NAs as data from pixels
#' not saturated but located in the neighbourhood of saturated pixels can return
#' unreliable data. This correction is needed by a phenomenon similar to
#' "blooming" in camera sensors whereby when a sensor well gets saturated some
#' of charge migrates to adjacent wells in the detector increasing their
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
#' @note Avoid using large n values as n pixels at each end of the array are
#'   skipped. The value of n needed for each detector type/instrument
#'   needs to be found through testing. As rule of thumb use 5 < n < 10 for
#'   Sony's ILxxx and 8 < n < 14 for Hamamatsu xxxx. At the moment we use
#'   a symetric window although "blooming" could be asymetric.
#'
#' @references
#'
#'
bleed_nas <- function(x, n = 10) {
  stopifnot(is.raw_spct(x))
  instr.desc <- getInstrDesc(x)
  instr.settings <- getInstrSettings(x)
  counts.cols <- grep("^counts", names(x), value = TRUE)
  # this is a "quick and dirty" algorithm that assumes that we can ignore the
  # first n and last n pixels of the detector array, which in practice are
  # almost never used for anything but dark reference and so very unlikely
  # to be exposed to an irradiance saturating their response.
  z <- x
  for (i in counts.cols) {
    for (j in n:(nrow(x) - n)) {
      if (anyNA(x[(j-n):(j+n), i])) {
        z[j, i] <- NA
      }
    }
  }
#  setInstrDesc(z, instr.desc)
#  setInstrSettings(z, instr.settings)
  z
}