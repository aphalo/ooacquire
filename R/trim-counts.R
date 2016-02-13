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
  counts.cols <- grep("^cps", names(x), value = TRUE)
  integ.times <- getInstrSettings(x)$integ.time
  cols <- counts.cols[order(integ.times, decreasing = TRUE)]
  x[["cps"]] <- x[[cols[1]]]
  for (i in 2:length(cols)) {
    x[["cps"]] <- ifelse(is.na(x[["cps"]]),
                          x[[cols[i]]],
                          x[["cps"]] )
  }
  x[ , c("w.length", "cps")]
}