#' Apply filter-based stray-light correction
#'
#' Apply to a counts-per-second spectrum corrections based of a paired reading
#' obtained with a polycarbonate filter (long-pass with cut-off at 400 nm). This
#' is a bit more sofisticated than simple subtracting the filter reading from
#' the mesurement as the effect of the filter itself on stray light is corrected
#' for.
#'
#' @param x,flt,dark \code{cps_spct} objects.
#' @param method Method variant used "original" (Ylianttila), "full" (Aphalo),
#'   "sun" (full with trimming).
#' @param stray.light.wl numeric vector of length 2 giving the range of
#'   wavelengths to use for the final stray light correction.
#' @param flt.dark.wl,flt.ref.wl numeric vectors of length 2 giving the ranges
#'   of wavelengths to use for the "dark" and "illuminated" regions of the
#'   array in the filter correction.
#' @param worker_fun function actually doing the correction on the w.lengths and
#'   counts per second vectors.
#' @param trim a numeric value to be used as argument for mean
#' @param verbose Logical indicating the level of warnings wanted. Defaults to
#'   \code{FALSE}.
#' @param ... additional params passed to worker_fun.
#'
#' @return A copy of \code{x} with the "cps" data replaced by the corrected
#'   ones.
#' @author Algorithm for method "original" developed by Lasse Ylianttila. Other
#'   methods are modified from Ylianttila's method by Pedro J. Aphalo.
#' @export
#'
#' @references \url{http://www.r4photobiology.info}
#'
#' @note The default \code{worker_fun} is just an example. Corrections are
#' specific to each individual spectrometer unit (not type or configuration)
#' and need to be written for each use case. The slit function tail correction
#' requires the characterization of the shape of the slit function by
#' measuring one or more laser beams at suitable wavelengths.
#'
uvb_corrections <- function(x,
                            flt,
                            dark,
                            method = "original",
                            stray.light.wl = c(218.5, 228.5),
                            flt.dark.wl = c(193, 209.5),
                            flt.ref.wl = c(360, 379.5),
                            worker_fun = maya_tail_correction,
                            trim = 0,
                            verbose = FALSE,
                            ...) {
  x <- trim_counts(x)
  x <- linearize_counts(x)
  x <- raw2cps(x)
  x <- fshift(x, range = flt.dark.wl)
  x <- merge_cps(x)
  flt <- trim_counts(flt)
  flt <- linearize_counts(flt)
  flt <- raw2cps(flt)
  flt <- fshift(flt, range = flt.dark.wl)
  flt <- merge_cps(flt)
  dark <- trim_counts(dark)
  dark <- linearize_counts(dark)
  dark <- raw2cps(dark)
  dark <- fshift(dark, range = flt.dark.wl)
  dark <- merge_cps(dark)
  x   <- x - dark
  flt <- flt - dark

  y <- filter_correction(x, flt,
                         method = method,
                         flt.dark.wl = flt.dark.wl,
                         flt.ref.wl = flt.ref.wl,
                         trim = trim,
                         verbose = verbose)

  z <- slit_function_correction(y, worker_fun = worker_fun, ...)

  stray.light <-
    mean(z[z$w.length > stray.light.wl[1] & z$w.length < stray.light.wl[2]][["cps"]],
         trim = trim,
         na.rm = TRUE)
  if (method == "original") {
    stray.light <- stray.light +
      stats::sd(z[z$w.length > stray.light.wl[1] & z$w.length < stray.light.wl[2]][["cps"]],
         na.rm = TRUE)
  }
  if (stray.light < 0.0) {
    stray.light <- 0.0
  }

  z - stray.light
}

#' @rdname uvb_corrections
#'
#' @export
#'
slit_function_correction <- function(x,
                                     worker_fun = maya_tail_correction,
                                     ...) {
  stopifnot(is.cps_spct(x))
  # check number of cps columns
  counts.cols <- grep("^cps", names(x), value = TRUE)
  if (counts.cols > 1) {
    warning("Multiple 'cps' variables found: merging them before continuing!")
    x <- merge_cps(x)
  }
  new.cps <- worker_fun(x[["w.length"]], x[["cps"]], ...)
  x[["cps"]] <- new.cps
  attr(x, "slit corrected") <- TRUE
  x
}

#' @rdname uvb_corrections
#'
#' @export
#'
filter_correction <- function(x,
                              flt,
                              method = "original",
                              flt.dark.wl = c(193, 209.5),
                              flt.ref.wl = c(360, 379.5),
                              trim = 0,
                              verbose = FALSE) {
  stopifnot(is.cps_spct(x) && is.cps_spct(flt))
  stopifnot(range(x) == range(flt) && length(x) == length(flt))
  counts.cols <- grep("^cps", names(x), value = TRUE)
  if (counts.cols > 1) {
    warning("Multiple 'cps' variables found in 'x': merging them before continuing!")
    x <- merge_cps(x)
  }
  counts.cols <- grep("^cps", names(flt), value = TRUE)
  if (counts.cols > 1) {
    warning("Multiple 'cps' variables found in 'flt': merging them before continuing!")
    flt <- merge_cps(flt)
  }

  if (verbose && any(flt[["cps"]] < 0.0)) {
    warning(paste(sum(flt[["cps"]] < 0.0)), 'negative values in flt[["cps"]].')
  }
  ## stray light estimate
  # ranges by name
  flt[["range"]] <- ifelse(x[["w.length"]] > flt.dark.wl[1] & x[["w.length"]] < flt.dark.wl[2],
                           "short",
                           ifelse(x[["w.length"]] > flt.ref.wl[1] & x[["w.length"]] < flt.ref.wl[2],
                                  "medium", "other"))
  mean_flt_cs_short <-
    mean(dplyr::filter(flt, range == "short")[["cps"]],
         trim = trim, na.rm = TRUE)
  mean_merged_cs_short <-
    mean(dplyr::filter(x, range == "short")[["cps"]],
         trim = trim, na.rm = TRUE)

  if (method == "original") {
    flt[["filter_ratio"]] <- flt[["cps"]] / x[["cps"]]
    if (verbose && any(is.na(dplyr::filter(flt, range = "short")[["filter_ratio"]]))) {
      warning(paste(sum(is.na(dplyr::filter(flt, range = "short")[["filter_ratio"]])),
                    "NAs in filter_ratio"))
    }
    mean_flt_ratio_short <-
      mean(dplyr::filter(cts, range == "short")[["filter_ratio"]],
           trim = trim, na.rm = TRUE)
  } else if (method == "full" || method == "sun" || method == "raw") {
    # attempt to avoid overcorrection
    if ((mean_merged_cs_short - mean_flt_cs_short) < 0.0) {
      mean_flt_ratio_short <- 1.0
    } else {
      mean_flt_ratio_short <- mean_flt_cs_short / mean_merged_cs_short
    }
  } else {
    stop(paste("method", method, "not supported"))
  }

  # diagnosis and correction of bad estimates
  if (is.na(mean_flt_ratio_short)) {
    warning("NA in mean_flt_ratio_short, replaced with 1.0")
    mean_flt_ratio_short <- 1.0 # we could the actual filter transmittance
  }
  if (mean_flt_ratio_short < 0.8) {
    # This is a guess based on PC filter transmittance of about 83%.
    if (verbose) warning("mean_flt_ratio_short < 0.8, was ", signif(mean_flt_ratio_short, 4), " reset to 1.0")
    mean_flt_ratio_short <- 1.0 # we could the actual filter transmittance
  } else if (mean_flt_ratio_short > 1.1) {
      # This could be set to 1.0 as it makes no sense to have more noise with the filter than without it!
      if (verbose) warning("mean_flt_ratio_short > 1.1, was ", signif(mean_flt_ratio_short, 4), " reset to 1.0")
      mean_flt_ratio_short <- 1.0
  } else {
    if (verbose) message("mean_flt_ratio_short is ", signif(mean_flt_ratio_short, 4))
  }
  mean_flt_cs_medium <-
    mean(dplyr::filter(flt, range == "medium")[["cps"]],
         trim = trim, na.rm = TRUE)
  if (verbose && (mean_flt_cs_medium / mean_flt_cs_short) > 1.0) {
    warning("There is more noise at", flt.ref.wl[1], " to ", flt.ref.wl[2],
            " nm than at ", flt.dark.wl[1],
            " to ", flt.dark.wl[2], " nm, ratio: ",
            signif(mean_flt_cs_medium / mean_flt_cs_short, 3))
  }

  # substraction of stray light
  first_correction <- ifelse(mean_flt_cs_medium > 0.0, mean_flt_cs_medium/mean_flt_ratio_short, 0.0)
  flt <- dplyr::mutate(flt, cps = ifelse(cps < 0.0, 0.0, cps))
  x   <- dplyr::mutate(x, cps = ifelse(w.length < flt.ref.wl[2],
                                       cps - flt[["cps"]] / mean_flt_ratio_short,
                                       cps - first_correction))
  x
}
