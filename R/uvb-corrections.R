#' Apply filter-based stray-light correction
#'
#' Apply to a counts-per-second spectrum corrections based of a paired reading
#' obtained with a polycarbonate filter (long-pass with cut-off at 400 nm). This
#' is a bit more sofisticated than simple subtracting the filter reading from
#' the mesurement as the effect of the filter itself on stray light is corrected
#' for.
#'
#' @param x,flt,dark \code{cps_spct} objects.
#' @param stray.light.method Method variant used "original" (Ylianttila).
#' @param stray.light.wl numeric vector of length 2 giving the range of
#'   wavelengths to use for the final stray light correction.
#' @param flt.dark.wl,flt.ref.wl numeric vectors of length 2 giving the ranges
#'   of wavelengths to use for the "dark" and "illuminated" regions of the
#'   array in the filter correction.
#' @param inst.dark.pixs numeric vector with indexes to array pixels that
#'   are in full drakness by instrument design.
#' @param worker.fun function actually doing the correction on the w.lengths and
#'   counts per second vectors.
#' @param trim a numeric value to be used as argument for mean
#' @param verbose Logical indicating the level of warnings wanted.
#' @param ... additional params passed to worker.fun.
#'
#' @return A copy of \code{x} with the "cps" data replaced by the corrected
#'   ones.
#' @author Algorithm for method "original" developed by Lasse Ylianttila. Other
#'   methods are modified from Ylianttila's method by Pedro J. Aphalo.
#' @export
#'
#' @references \url{http://www.r4photobiology.info}
#'
#' @note The default \code{worker.fun} is just an example. Corrections are
#' specific to each individual spectrometer unit (not type or configuration)
#' and need to be written for each use case. The slit function tail correction
#' requires the characterization of the shape of the slit function by
#' measuring one or more laser beams at suitable wavelengths.
#'
uvb_corrections <- function(x,
                            flt,
                            dark,
                            stray.light.method = "original",
                            stray.light.wl = c(218.5, 228.5),
                            flt.dark.wl = c(193, 209.5),
                            flt.ref.wl = c(360, 379.5),
                            inst.dark.pixs = c(1:4),
                            worker.fun = ooacquire::maya_tail_correction,
                            trim = 0,
                            verbose = FALSE,
                            ...) {

  spct.worker <- function(x) {
    x <- skip_bad_pixs(x)
    x <- trim_counts(x)
    x <- bleed_nas(x)
    x <- linearize_counts(x, verbose = verbose)
    if (length(inst.dark.pixs) && is.numeric(inst.dark.pixs)) {
      x <- fshift(x, range = x[["w.length"]][inst.dark.pixs])
    }
    x <- raw2cps(x)
    merge_cps(x)
  }

  if (length(x) == 0) {
    if (verbose) {
      warning("No 'light' measurement available: aborting")
    }
    return(source_spct())
  }
  when.measured <- getWhenMeasured(x)
  where.measured <- getWhereMeasured(x)
  what.measured <- getWhatMeasured(x)
  descriptor <- getInstrDesc(x)
  inst.settings <- getInstrSettings(x)
  x <- spct.worker(x)

  if (length(dark) > 0) {
    dark <- spct.worker(dark)
  }
  if (length(flt) == 0) {
    if (verbose) {
      warning("No filter spectra available: continuing without filter correction")
    }
  } else {
    flt <- spct.worker(flt)
  }
  if (length(dark) == 0) {
    if (verbose) {
      warning("No 'dark' measurement available: using internal reference")
    }
  } else if (length(dark) > 0) {
    x <- x - dark
    if (length(flt) > 0) {
      flt <- flt - dark
    }
  }

  if (length(flt) > 0 &&
        average_spct(clip_wl(x, range = flt.ref.wl)) < 0.1 * max(x[["cps"]])) {
    warning("Too low cps in filter reference region, skipping filter correction.")
    flt.flag <- FALSE
  } else {
    flt.flag <- TRUE
  }

  if (!is.null(flt) && flt.flag) {
    x <- filter_correction(x, flt,
                           stray.light.method = stray.light.method,
                           stray.light.wl = stray.light.wl,
                           flt.dark.wl = flt.dark.wl,
                           flt.ref.wl = flt.ref.wl,
                           trim = trim,
                           verbose = verbose)
  } else {
    if (verbose) {
      warning("Assuming pure stray light in ",
              stray.light.wl[1], " to ", stray.light.wl[2], " nm")
    }
    x <- no_filter_correction(x,
                              stray.light.wl = stray.light.wl,
                              flt.dark.wl = flt.dark.wl,
                              trim = trim,
                              verbose = verbose)
  }

  if (is.null(worker.fun)) {
    z <- x
  } else {
    z <- slit_function_correction(x, worker.fun = worker.fun, ...)
  }

  z <- setInstrDesc(z, descriptor)
  z <- setInstrSettings(z, inst.settings)
  z <- setWhenMeasured(z, when.measured)
  z <- setWhereMeasured(z, where.measured)
  z <- setWhatMeasured(z, what.measured)
  z
}

#' @rdname uvb_corrections
#'
#' @export
#'
slit_function_correction <- function(x,
                                     worker.fun = ooacquire::maya_tail_correction,
                                     verbose = TRUE,
                                     ...) {
  stopifnot(is.cps_spct(x))
  stopifnot(is.null(attr(x, "slit.corrected")) || !attr(x, "slit.corrected"))
  # check number of cps columns
  counts.cols <- grep("^cps", names(x), value = TRUE)
  if (length(counts.cols) > 1) {
    if (verbose) {
      warning("Multiple 'cps' variables found: merging them before continuing!")
    }
    x <- merge_cps(x)
    counts.cols <- grep("^cps", names(x), value = TRUE)
    if (length(counts.cols) > 1) {
      if (verbose) {
        stop("Multiple 'cps' variables found: merge operation failed!")
      }
    }
  }
  new.cps <- worker.fun(x[["w.length"]], x[["cps"]], ...)
  x[["cps"]] <- x[["cps"]] - new.cps[["tail"]]
  attr(x, "slit.corrected") <- TRUE
  x
}

#' @rdname uvb_corrections
#'
#' @export
#'
filter_correction <- function(x,
                              flt,
                              stray.light.method = "original",
                              stray.light.wl = c(218.5, 228.5),
                              flt.dark.wl = c(193, 209.5),
                              flt.ref.wl = c(360, 379.5),
                              trim = 0,
                              verbose = FALSE) {
  stopifnot(is.null(attr(x, "straylight.corrected")) || !attr(x, "straylight.corrected"))
  stopifnot(is.cps_spct(x) && is.cps_spct(flt))
  stopifnot(range(x) == range(flt) && length(x) == length(flt))
  counts.cols <- length(grep("^cps", names(x), value = TRUE))
  if (counts.cols > 1) {
    if (verbose) {
      warning("Multiple 'cps' variables found in 'x': merging them before continuing!")
    }
    x <- merge_cps(x)
  }
  counts.cols <- length(grep("^cps", names(flt), value = TRUE))
  if (counts.cols > 1) {
    if (verbose) {
      warning("Multiple 'cps' variables found in 'flt': merging them before continuing!")
    }
    flt <- merge_cps(flt)
  }

  # Find maximum cps

  max_x_cs <- max(x[["cps"]], na.rm = TRUE)

  # Correct for filter dark counts
  flt_clip_dark <- clip_wl(flt, range = flt.dark.wl)
  x_clip_dark <- clip_wl(x, range = flt.dark.wl)

  # !! NEEDS TO BE CHANGED TO TEST ONLY RELEVANT wl range
  if (verbose && any(flt_clip_dark[["cps"]] < 0.0)) {
    warning(paste(sum(flt_clip_dark[["cps"]] < 0.0)),
            ' negative values in flt_clip_dark[["cps"]].')
  }

  mean_flt_cs_short <- mean(flt_clip_dark[["cps"]],
                            trim = trim, na.rm = TRUE)
  mean_x_cs_short <- mean(x_clip_dark[["cps"]],
                          trim = trim, na.rm = TRUE)

  if (stray.light.method == "original") {
    mean_flt_ratio_short <- mean(flt_clip_dark[["cps"]] / x_clip_dark[["cps"]])
    flt_clip_dark[["filter_ratio"]] <-
      flt_clip_dark[["cps"]] / x_clip_dark[["cps"]]
    mean_flt_ratio_short <- mean(flt_clip_dark[["filter_ratio"]],
                                 trim = trim, na.rm = TRUE)
    if (verbose && anyNA(flt_clip_dark[["filter_ratio"]])) {
      warning(paste(sum(is.na(flt_clip_dark[["filter_ratio"]])),
                    " NAs in filter_ratio"))
    }
  } else if (stray.light.method == "full" ||
             stray.light.method == "sun" ||
             stray.light.method == "raw") {
    # attempt to avoid overcorrection
    if ((mean_x_cs_short - mean_flt_cs_short) < 0.0) {
      mean_flt_ratio_short <- 1.0
    } else {
      mean_flt_ratio_short <- mean_flt_cs_short / mean_x_cs_short
    }
  } else {
    stop(paste("method '", stray.light.method, "' not supported"))
  }

  # diagnosis and correction of bad estimates
  if (is.na(mean_flt_ratio_short)) {
    warning("NA in mean_flt_ratio_short, skipping correction")
  } else {
    if (mean_flt_ratio_short < 0.75) {
      # This is a guess based on PC filter transmittance of about 83%.
      if (verbose || abs(mean_x_cs_short / max_x_cs) > 1e-3) {
        warning("mean_flt_ratio_short < 0.75, was ",
                signif(mean_flt_ratio_short, 4),
                "; light source emits in UVC",
                "; or instrument dark reading unstable",
                ", using 0.85")
      }
      mean_flt_ratio_short <- 0.85 # we use the actual filter transmittance
    } else if (mean_flt_ratio_short > 1.1) {
      # This could be set to 1.0 as it makes no sense to have more noise with the filter than without it!
      if (verbose || abs(mean_x_cs_short / max_x_cs) > 1e-3)
        warning("mean_flt_ratio_short > 1.1, was ",
                signif(mean_flt_ratio_short, 4),
                "; instrument dark reading unstable")
#      mean_flt_ratio_short <- 1.0
    } else {
      if (verbose) message("mean_flt_ratio_short is ", signif(mean_flt_ratio_short, 4))
    }
    selector <- x[["w.length"]] < flt.ref.wl[2] & x[["w.length"]] > flt.dark.wl[2]
    # Apply correction
    x[selector, "cps"] <-
      x[selector, "cps"] - flt[selector, "cps"] / mean_flt_ratio_short
  }

  # correct "long" side of spectrum for stray light
  flt_clip_ref <- clip_wl(flt, range = flt.ref.wl)
  x_clip_ref <- clip_wl(x, range = flt.ref.wl)

  # !! NEEDS TO BE CHANGED TO TEST ONLY RELEVANT wl range
  if (verbose && any(flt_clip_ref[["cps"]] < 0.0)) {
    warning(paste(sum(flt_clip_ref[["cps"]] < 0.0)),
            ' negative values in flt_clip_ref[["cps"]].')
  }

  mean_flt_cs_medium <- mean(flt_clip_ref[["cps"]],
                            trim = trim, na.rm = TRUE)
  mean_x_cs_medium <- mean(x_clip_ref[["cps"]],
                          trim = trim, na.rm = TRUE)

  if (verbose && ((mean_flt_cs_medium / mean_flt_cs_short) > 1.0)) {
    message("There is more noise at ", flt.ref.wl[1], " to ", flt.ref.wl[2],
            " nm than at ", flt.dark.wl[1],
            " to ", flt.dark.wl[2], " nm, ratio: ",
            signif(mean_flt_cs_medium / mean_flt_cs_short, 4))
  }

  selector <- x[["w.length"]] >= flt.ref.wl[2]
  stray_light_correction <- mean_flt_cs_medium / mean_flt_ratio_short
  # Apply correction
  x[selector, "cps"] <-
    x[selector, "cps"] - stray_light_correction

  attr(x, "straylight.corrected") <- TRUE

  x
}

#' @rdname uvb_corrections
#'
#' @export
#'
no_filter_correction <- function(x,
                                 stray.light.wl = c(218.5, 228.5),
                                 flt.dark.wl = c(193, 209.5),
                                 flt.ref.wl = c(360, 379.5),
                                 trim = 0,
                                 verbose = FALSE) {
  stopifnot(is.null(attr(x, "straylight.corrected")) || !attr(x, "straylight.corrected"))
  stopifnot(is.cps_spct(x))
  counts.cols <- length(grep("^cps", names(x), value = TRUE))
  if (counts.cols > 1) {
    if (verbose) {
      warning("Multiple 'cps' variables found in 'x': merging them before continuing!")
    }
    x <- merge_cps(x)
  }

  # Correct for dark counts
  x_clip_dark <- clip_wl(x, range = flt.dark.wl)

  mean_x_cs_short <- mean(x_clip_dark[["cps"]],
                          trim = trim, na.rm = TRUE)

  #
  selector <- x[["w.length"]] < flt.ref.wl[2] & x[["w.length"]] > flt.dark.wl[2]
  # Apply correction
  x[selector, "cps"] <-
    x[selector, "cps"] - mean_x_cs_short

  # correct "long" side of spectrum for stray light
  x_clip_stray <- clip_wl(x, range = stray.light.wl)

  # !! NEEDS TO BE CHANGED TO TEST ONLY RELEVANT wl range
  if (verbose && any(x_clip_stray[["cps"]] < 0.0)) {
    warning(paste(sum(x_clip_stray[["cps"]] < 0.0)),
            ' negative values in x_clip_stray[["cps"]].')
  }

  mean_x_cs_medium <- mean(x_clip_stray[["cps"]],
                           trim = trim, na.rm = TRUE)

  if (verbose && ((mean_x_cs_medium / mean_x_cs_short) > 1.0)) {
    message("There stronger signal at ", stray.light.wl[1], " to ", stray.light.wl[2],
            " nm than at ", flt.dark.wl[1],
            " to ", flt.dark.wl[2], " nm, ratio: ",
            signif(mean_x_cs_medium / mean_x_cs_short, 3))
  }

  selector <- x[["w.length"]] >= flt.ref.wl[2]
  # Apply correction
  x[selector, "cps"] <-
    x[selector, "cps"] - mean_x_cs_medium

  attr(x, "straylight.corrected") <- TRUE

  x
}
