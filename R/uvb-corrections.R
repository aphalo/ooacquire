#' Apply filter-based stray-light correction
#'
#' Apply to a counts-per-second spectrum corrections based of a paired reading
#' obtained with a polycarbonate filter (long-pass with cut-in at 400 nm). This
#' is a bit more sophisticated than simple subtracting the filter reading from
#' the measurement as the effect of the filter itself on stray light is corrected
#' for.
#'
#' @param x \code{raw_mspct} The raw counts from measurements.
#' @param spct.names named character vector of length three.
#' @param stray.light.method Method variant used, "original" (Ylianttila),
#'   "simple", "full", "sun", "raw", "none".
#' @param stray.light.wl numeric vector of length 2 giving the range of
#'   wavelengths to use for the final stray light correction.
#' @param flt.dark.wl,flt.ref.wl numeric vectors of length 2 giving the ranges
#'   of wavelengths to use for the "dark" and "illuminated" regions of the
#'   array in the filter correction.
#' @param flt.Tfr numeric fractional transmittance of the filter to the source
#'   of stray light, used only for method "simple".
#' @param inst.dark.pixs numeric vector with indexes to array pixels that
#'   are in full darkness by instrument design.
#' @param worker.fun function actually doing the correction on the w.lengths and
#'   counts per second vectors, or the name of the function as a character string.
#' @param trim a numeric value to be used as argument for mean
#' @param hdr.tolerance numeric Tolerance for mean deviation among cps columns as
#'   a fraction of one. Used in check of HDR consistency.
#' @param verbose Logical indicating the level of warnings wanted.
#' @param ... additional parameters passed to worker.fun.
#'
#' @return A \code{cps_spct} object with the corrected count-per-second values
#'   in coulmn "cps".
#'
#' @author Algorithm for method "original" developed by Lasse Ylianttila. Other
#'   methods are modified from Ylianttila's method by Pedro J. Aphalo.
#'
#' @family spectral data-processing functions
#'
#' @export
#'
#' @references \url{http://www.r4photobiology.info}
#'
#' @note \code{stray.light.method = "none"} is a valid argument only for
#'   function \code{uvb_corrections()}. The default \code{worker.fun} is just an
#'   example. Corrections are specific to each individual spectrometer unit (not
#'   just type or configuration) and code needs to be written for most
#'   individual use cases. The slit function tail correction requires the
#'   characterization of the shape of the slit function by measuring one or more
#'   laser beams at suitable wavelengths.
#'
uvb_corrections <-
  function(x,
           spct.names = c(light = "light", filter = "filter", dark = "dark"),
           stray.light.method = "original",
           stray.light.wl = c(218.5, 228.5),
           flt.dark.wl = c(193, 209.5),
           flt.ref.wl = c(360, 379.5),
           flt.Tfr = 0.9,
           inst.dark.pixs = 1:4,
           worker.fun = NULL,
           trim = 0.05,
           hdr.tolerance = getOption("ooacquire.hdr.tolerance", default = 0.05),
           verbose = getOption("photobiology.verbose", default = FALSE),
           ...) {

    stopifnot(length(x) > 0L)
    stopifnot(length(spct.names) > 0L)
    stopifnot(names(spct.names) %in% c("light", "filter", "dark"))
    stopifnot(all(names(x) %in% spct.names))

    if (!all(spct.names == names(spct.names))) {
      # rename columns
      names.to.map <- spct.names != names(spct.names) & spct.names %in% names(x)
      names(x)[names(x) == spct.names[names.to.map]] <- names(spct.names[names.to.map])
    }
    stopifnot("light" %in% names(x)) # no measurement

    flt.flag <- !is.na(stray.light.method) && stray.light.method != "none"

    if (!flt.flag && "filter" %in% names(x)) {
      # if method does not support a filter measurement
      # we discard the filter data
      x[["filter"]] <- NULL
    }
    spct.names <- names(x)

    if (is.raw_mspct(x)) {
      y <- raw2corr_cps(x,
                        ref.pixs.range = inst.dark.pixs,
                        hdr.tolerance = hdr.tolerance)
    } else if (is.cps_mspct(x)) {
      y <- x
    }

    if (is.character(worker.fun)) {
      worker.fun <- get(worker.fun,
                        mode = "function")
    }

    if (length(setdiff(c("light", "filter", "dark"), spct.names)) == 0) {
      y <- ref_correction(y, ref_name = "dark")
    } else if (length(setdiff(c("light", "dark"), spct.names)) == 0) {
      if (verbose && !stray.light.method %in% c("none")) {
        warning("No 'filter' measurement available: ",
                "continuing without filter correction")
      }
      flt.flag <- FALSE   # overrides flt.flag <- TRUE set above based on method
      y <- ref_correction(y, ref_name = "dark")
    } else if (length(setdiff(c("light", "filter"), spct.names)) == 0) {
      # added 2019-01-09
      if (verbose) {
        warning("No 'dark' measurement available: using internal reference")
      }
    } else if (length(setdiff("light", spct.names)) == 0) {
      if (verbose) {
        warning("No 'dark' or 'filter' measurements available: ",
                "using internal reference")
      }
      flt.flag <- FALSE  # overrides flt.flag  <- TRUE set above based on method
    } else {
      if (verbose) {
        warning("No 'light' measurement available: aborting")
      }
      return(cps_spct())
    }

    if (flt.flag && stray.light.method != "simple" &&
        average_spct(clip_wl(y[["light"]], range = flt.ref.wl)) < 0.001 *
        max(y[["light"]][["cps"]], na.rm = TRUE)) {
      warning("Too low cps in filter reference region, ",
              "setting method to 'simple'.\n")
      stray.light.method <- "simple"
    }

    if (flt.flag) {
      z <- filter_correction(x = y[["light"]],
                             flt = y[["filter"]],
                             stray.light.method = stray.light.method,
                             stray.light.wl = stray.light.wl,
                             flt.dark.wl = flt.dark.wl,
                             flt.ref.wl = flt.ref.wl,
                             flt.Tfr = flt.Tfr,
                             trim = trim,
                             hdr.tolerance = hdr.tolerance,
                             verbose = verbose)
    } else if (stray.light.method != "none") {
      if (verbose) {
        warning("Assuming pure stray light in ",
                stray.light.wl[1], " to ", stray.light.wl[2], " nm")
      }
      z <- no_filter_correction(x = y[["light"]],
                                stray.light.wl = stray.light.wl,
                                flt.dark.wl = flt.dark.wl,
                                flt.Tfr = flt.Tfr,
                                trim = trim,
                                hdr.tolerance = hdr.tolerance,
                                verbose = verbose)
    }  else {
      z <- y[["light"]]
    }

    if (is.null(worker.fun) && stray.light.method != "none") {
      if (verbose) {
        warning("Skipping slit function tail correction: no function available.")
      }
    } else {
      z <- slit_function_correction(z,
                                    worker.fun = worker.fun,
                                    hdr.tolerance = hdr.tolerance,
                                    ...)
    }

    z
  }

#' @rdname uvb_corrections
#'
#' @export
#'
slit_function_correction <-
  function(x,
           worker.fun = NULL,
           hdr.tolerance = getOption("ooacquire.hdr.tolerance", default = 0.05),
           verbose = getOption("photobiology.verbose", default = FALSE),
           ...) {
    stopifnot(is.cps_spct(x))

    slit.corrected <- attr(x, "slit.corrected", exact = TRUE)
    if (!is.null(slit.corrected) && !is.na(slit.corrected) && slit.corrected) {
      if (verbose) {
        warning("Skipping slit function tail correction: already corrected.")
      }
      return(x)
    }

    if (is.character(worker.fun)) {
      worker.fun = get(worker.fun,
                       mode = "function")
    }

    if (is.null(worker.fun)) {
      if (verbose) {
        warning("Skipping slit-function tail correction: no function available.")
      }
      return(x)
    }
    if (verbose) {
      message("Applying slit-function tail correction.")
    }

    # check number of cps columns and merge if needed
    x <- merge_cps(x, tolerance = hdr.tolerance)

    new.cps <- worker.fun(x[["w.length"]], x[["cps"]], ...)
    x[["cps"]] <- x[["cps"]] - new.cps[["tail"]]
    attr(x, "slit.corrected") <- TRUE
    x
  }

#' Correct for stray light
#'
#' Correct cps readings for stray light, using either measured stray light, or
#' using a non-excited region of the detector array.
#'
#' @param x,flt cps_spct objects, containing spectral data from which to subtract
#'   stray light, and measured stray light, respectively.
#' @param stray.light.method Method variant used, "original" (Ylianttila),
#'   "simple", "full", "sun", "raw", "none".
#' @param stray.light.wl numeric vector of length 2 giving the range of
#'   wavelengths to use for the final stray light correction.
#' @param flt.dark.wl,flt.ref.wl numeric vectors of length 2 giving the ranges
#'   of wavelengths to use for the "dark" and "illuminated" regions of the array
#'   in the filter correction.
#' @param flt.Tfr numeric fractional transmittance of the filter to the source
#'   of stray light, used only for method "simple".
#' @param trim a numeric value to be used as argument for mean
#' @param filter.nir.adjust logical Flag indicating if the cps in the
#'   "filter" reference spectrum need to be adjust based on NIR region cps
#'   in the "light" spectrum. EXPERIMENTAL!!
#' @param hdr.tolerance numeric Tolerance for mean deviation among cps columns
#'   as a fraction of one. Used in check of HDR consistency.
#' @param verbose Logical indicating the level of warnings wanted.
#'
#' @export
#'
filter_correction <-
  function(x,
           flt,
           stray.light.method = "original",
           stray.light.wl = c(218.5, 228.5),
           flt.dark.wl = c(193, 209.5),
           flt.ref.wl = c(360, 379.5),
           flt.Tfr = 1,
           trim = 0.05,
           filter.nir.adjust = FALSE,
           hdr.tolerance = getOption("ooacquire.hdr.tolerance", default = 0.10),
           verbose = getOption("photobiology.verbose", default = FALSE)) {
    stopifnot(is.cps_spct(x) && is.cps_spct(flt))
    stopifnot(all(wl_range(x) == wl_range(flt)) && nrow(x) == nrow(flt))

    straylight.corrected <- attr(x, "straylight.corrected", exact = TRUE)
    if (!is.null(straylight.corrected) &&
        !is.na(straylight.corrected) &&
        straylight.corrected) {
      if (verbose) {
        warning("Skipping straylight correction: already corrected.")
      }
      return(x)
    }

    # check number of cps columns and merge if needed
    x   <- merge_cps(x, tolerance = hdr.tolerance)
    flt <- merge_cps(flt, tolerance = hdr.tolerance)

    if (filter.nir.adjust) {

      # Rescale flt reference assuming stray light is all NIR
      max_x_nir_cps <- mean(clip_wl(x, range = c(950, 1020))[["cps"]], na.rm = TRUE)
      max_flt_nir_cps <- mean(clip_wl(flt, range = c(950, 1020))[["cps"]], na.rm = TRUE)

      x2flt.k <- max_x_nir_cps * 0.85 / max_flt_nir_cps # assumed filter transmittance

      # this is an attempt to deal with changing light conditions
      if (x2flt.k < 0.8 || x2flt.k > 1.2) {
        #     message("Rescaling \"filter\" spectrum by ", signif(x2flt.k, 3))
        flt <- flt * x2flt.k
      }

    }

    # Find maximum cps
    max_x_cps <- max(x[["cps"]], na.rm = TRUE)

    # compute filter short wl "dark" cps
    if (anyNA(flt.dark.wl)) {
      mean_flt_cps_short <- 0
      mean_x_cps_short <- 0
    } else {
      flt_clip_dark <- clip_wl(flt, range = flt.dark.wl)
      x_clip_dark <- clip_wl(x, range = flt.dark.wl)

      mean_flt_cps_short <- mean(flt_clip_dark[["cps"]],
                                 trim = trim, na.rm = TRUE)
      mean_x_cps_short <- mean(x_clip_dark[["cps"]],
                               trim = trim, na.rm = TRUE)
    }

    # compute filter "reference" wl cps
    flt_clip_ref <- clip_wl(flt, range = flt.ref.wl)
    x_clip_ref <- clip_wl(x, range = flt.ref.wl)

    mean_flt_cps_ref <- mean(flt_clip_ref[["cps"]],
                             trim = trim, na.rm = TRUE)
    mean_x_cps_ref <- mean(x_clip_ref[["cps"]],
                           trim = trim, na.rm = TRUE)

    # We try to avoid spureous warnings by using the mean
    if (verbose && mean_flt_cps_short < -1e4 * max_x_cps) {
      warning("Negative mean cps in \"filter\" spectrum's ",
              "internal dark reference: ",
              mean_flt_cps_short)
    }

    if (verbose && mean_x_cps_short < -1e4 * max_x_cps) {
      warning("Negative mean cps in \"measured\" spectrum's ",
              "internal dark reference: ",
              mean_x_cps_short)
    }

    # Lasse's first correction
    if (stray.light.method == "original") {
      if (verbose && anyNA(flt_clip_dark[["filter_ratio"]])) {
        warning(paste(sum(is.na(flt_clip_dark[["filter_ratio"]])),
                      " NAs in filter_ratio"))
      }
      flt_clip_dark[["filter_ratio"]] <-
        flt_clip_dark[["cps"]] / x_clip_dark[["cps"]]
      mean_flt_ratio_short <- mean(flt_clip_dark[["filter_ratio"]],
                                   trim = trim, na.rm = TRUE)
    } else if (stray.light.method == "full" ||
               stray.light.method == "sun" ||
               stray.light.method == "raw") {
      # attempt to avoid overcorrection
      if ((mean_x_cps_short - mean_flt_cps_short) < 0.0) {
        mean_flt_ratio_short <- 1.0
      } else {
        mean_flt_ratio_short <- mean_flt_cps_short / mean_x_cps_short
      }
    } else if (stray.light.method == "simple") {
      # trust filter spectral transmittance
      mean_flt_ratio_short <- flt.Tfr
    }else {
      stop(paste("stray.light.method '", stray.light.method, "' not supported"))
    }

    # diagnosis and correction of bad estimates
    if (is.na(mean_flt_ratio_short)) {
      warning("NA in mean_flt_ratio_short, skipping correction")
    } else {
      if (mean_flt_ratio_short < 0.75) {
        # This is a guess based on PC filter transmittance of 90% or more in IR.
        if (verbose || abs(mean_x_cps_short / max_x_cps) > 1e-2) {
          warning("mean_flt_ratio_short < 0.75, was ",
                  signif(mean_flt_ratio_short, 4),
                  "; light source emits in UVC",
                  "; or instrument dark reading unstable",
                  ", using 0.85")
        }
        mean_flt_ratio_short <- 0.85 # we use the actual filter transmittance
      } else if (mean_flt_ratio_short > 1.5) {
        # This is set to 1.5 as it makes no sense to have a lot more noise with the filter than without it!
        if (verbose || abs(mean_x_cps_short / max_x_cps) > 1e-2)
          warning("mean_flt_ratio_short > 1.5, was ",
                  signif(mean_flt_ratio_short, 4),
                  ", set to 1.5; possible stray light unstable")
        mean_flt_ratio_short <- 1.5
      } else {
        if (verbose) message("mean_flt_ratio_short is ",
                             signif(mean_flt_ratio_short, 4))
      }
      selector <-
        x[["w.length"]] > flt.dark.wl[2] & x[["w.length"]] < flt.ref.wl[2]
      # Apply correction
      x[selector, "cps"] <-
        x[selector, "cps"] - flt[selector, "cps"] / mean_flt_ratio_short
    }

    # correct "long" side of spectrum for stray light
    flt_clip_ref <- clip_wl(flt, range = flt.ref.wl)
    x_clip_ref <- clip_wl(x, range = flt.ref.wl)

    mean_flt_cps_medium <- mean(flt_clip_ref[["cps"]],
                                trim = trim, na.rm = TRUE)
    mean_x_cps_medium <- mean(x_clip_ref[["cps"]],
                              trim = trim, na.rm = TRUE)

    # Because of pixel to pixel random noise this could easily be true per pixel
    if (verbose && mean_flt_cps_medium < 0.0) {
      warning("Mean cps of ", mean_flt_cps_medium,
              " in reference region of \"filter\" scan!")
    }

    if (verbose && mean_x_cps_medium < 0.0) {
      warning("Mean cps of ", mean_x_cps_medium,
              " in reference region of \"measurement\" scan!")
    }

    if (verbose && ((mean_flt_cps_medium / mean_flt_cps_short) > 1.0)) {
      message("There is more noise at ", flt.ref.wl[1], " to ", flt.ref.wl[2],
              " nm than at ", flt.dark.wl[1],
              " to ", flt.dark.wl[2], " nm, ratio: ",
              signif(mean_flt_cps_medium / mean_flt_cps_short, 4))
    }

    selector <- x[["w.length"]] >= flt.ref.wl[2]
    stray_light_correction <- mean_flt_cps_medium / mean_flt_ratio_short
    # Apply correction
    if (stray_light_correction > 0) {
      x[selector, "cps"] <-
        x[selector, "cps"] - stray_light_correction
    } else {
      warning("No stray light correction applied to long end of spectrum.")
    }

    attr(x, "straylight.corrected") <- TRUE

    x
  }

#' @rdname filter_correction
#'
#' @export
#'
no_filter_correction <-
  function(x,
           stray.light.wl = c(218.5, 228.5),
           flt.dark.wl = c(193, 209.5),
           flt.ref.wl = NULL,
           flt.Tfr = 1,
           trim = 0,
           hdr.tolerance = getOption("ooacquire.hdr.tolerance", default = 0.10),
           verbose = getOption("photobiology.verbose", default = FALSE)) {
    stopifnot(is.null(attr(x, "straylight.corrected")) ||
                !attr(x, "straylight.corrected"))
    stopifnot(is.cps_spct(x))
    counts.cols <- length(grep("^cps", names(x), value = TRUE))
    if (counts.cols > 1) {
      if (verbose) {
        warning("Multiple 'cps' variables found in 'x': ",
                "merging them before continuing!")
      }
      x <- merge_cps(x, tolerance = hdr.tolerance)
    }

    # Estimate for dark counts
    x_clip_dark <- clip_wl(x, range = flt.dark.wl)

    mean_x_cps_short <- mean(x_clip_dark[["cps"]],
                             trim = trim, na.rm = TRUE)

    # Estimate for stray light + dark count
    x_clip_stray <- clip_wl(x, range = stray.light.wl)

    mean_x_cps_medium <- mean(x_clip_stray[["cps"]],
                              trim = trim, na.rm = TRUE)

    # We test whether any stray light has been detected
    if ((mean_x_cps_medium - mean_x_cps_short) < 0.0 &&
        mean_x_cps_short < (1e-3 * max(x[["cps"]], na.rm = TRUE))) {
      if (verbose) {
        warning("No stray light detected, skipping correction.")
      }
    } else {
      x <- x - mean_x_cps_medium
    }

    attr(x, "straylight.corrected") <- TRUE

    x
  }
