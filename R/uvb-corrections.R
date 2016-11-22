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

  if (is.null(x)) {
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

  if (!is.null(dark)) {
    dark <- spct.worker(dark)
  }
  if (is.null(flt)) {
    if (verbose) {
      warning("No filter spectra available: continuing without filter correction")
    }
  } else {
    flt <- spct.worker(flt)
  }
  if (is.null(dark)) {
    if (verbose) {
      warning("No 'dark' measurement available: using internal reference")
    }
  } else {
    x <- x - dark
    if (!is.null(flt)) {
      flt <- flt - dark
    }
  }

  if (!is.null(flt)) {
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
    if (mean_flt_ratio_short < 0.8) {
      # This is a guess based on PC filter transmittance of about 83%.
      if (verbose) warning("mean_flt_ratio_short < 0.8, was ",
                           signif(mean_flt_ratio_short, 4),
                           "; light source emits in UVC",
                           "; or it was off during filter measurement",
                           ", using 0.8")
      mean_flt_ratio_short <- 0.8 # we use the actual filter transmittance
    } else if (mean_flt_ratio_short > 1.1) {
      # This could be set to 1.0 as it makes no sense to have more noise with the filter than without it!
      if (verbose)
        warning("mean_flt_ratio_short > 1.1, was ",
                signif(mean_flt_ratio_short, 4),
                "; probably no filter used ",
                " reset to 1.0")
      mean_flt_ratio_short <- 1.0
    } else {
      if (verbose) message("mean_flt_ratio_short is ", signif(mean_flt_ratio_short, 4))
    }
    selector <- x[["w.length"]] < 378
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

  selector <- x[["w.length"]] >= 378
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
  selector <- x[["w.length"]] < 378
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
    message("There stronger signal at", stray.light.wl[1], " to ", stray.light.wl[2],
            " nm than at ", stray.light.wl[1],
            " to ", stray.light.wl[2], " nm, ratio: ",
            signif(mean_x_cs_medium / mean_x_cs_short, 3))
  }

  selector <- x[["w.length"]] >= 378
  # Apply correction
  x[selector, "cps"] <-
    x[selector, "cps"] - mean_x_cs_medium

  attr(x, "straylight.corrected") <- TRUE

  x
}

#' Convert raw counts data into spectral irradiance
#'
#' @param x A named list of one to hree vectors of file names, with names
#'   "light", "filter", and "dark".
#' @param method A named list of constants and functions defining the
#'   method to be sued for stray light and dark signal corrections.
#' @param return.cps logical Useful when there is no need to apply a calibration,
#'   such as when computing new calibration multipliers.
#' @param descriptor A named list with a descriptor of the characteristics of
#'   the spectrometer (if serial number does not agree an error is triggered).
#' @param locale	The locale controls defaults that vary from place to place. The
#'   default locale is US-centric (like R), but you can use
#'   \code{\link[readr]{locale}} to create your own locale that controls things
#'   like the default time zone, encoding, decimal mark, big mark, and day/month
#'   names.
#' @param verbose Logical indicating the level of warnings wanted.
#' @param ... Named argument passed to \code{photobiology::cps2irrad} which is
#'   the final calculation step.
#'
#' @note Currently \code{s_irrad_corrected.list} allows processing of files
#'   written by OceanOptics' SpectraSuite software, from protocols with
#'   integration-time bracketing or not, with a dark reference measurement or
#'   not. Three measurements components are recognized: a "light" measurement, a
#'   "filter" measurement using a polycarbonate filter and a dark measurement.
#'   Only "light" is mandatory. Data should be raw counts, either corrected for
#'   detector non-linearity or not. All three spectra should be acquired using
#'   the same instrument settings to achieve good accuracy.
#'
#' @export
#'
s_irrad_corrected <- function(x, ...) UseMethod("s_irrad_corrected")

#' @describeIn s_irrad_corrected Default for generic function.
#' @export
s_irrad_corrected.default <- function(x, ...) {
  stop("'s_irrad_corrected' not implemented for class '", class(x))
}

#' @describeIn s_irrad_corrected Default for generic function.
#' @param time a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} no date variable is added
#' @export
s_irrad_corrected.list <- function(x,
                                   time = NULL,
                                   method,
                                   return.cps = FALSE,
                                   descriptor,
                                   locale,
                                   verbose = FALSE,
                                   ...) {
  comment.txt <- paste(names(x),
                       sapply(x, paste, collapse = ", "),
                       sep = ": ", collapse = "\n")

  if(length(x[["light"]]) == 0) {
    if (verbose) {
      warning("Filename(s) with name 'light' missing")
    }
    if (return.cps) {
      z <- cps_spct()
      comment(z) <- comment.txt
      return(z)
    } else {
      z <- source_spct()
      comment(z) <- comment.txt
      return(z)
    }
  }
  raw.mspct <-
    ooacquire::read_files2mspct(x,
                                time = time,
                                locale = locale,
                                descriptor = descriptor,
                                verbose = verbose)

  corrected.spct <- s_irrad_corrected(x = raw.mspct,
                         method = method,
                         return.cps = return.cps,
                         verbose = verbose,
                         ...)
  setWhatMeasured(comment.txt)
  comment(corrected.spct) <- paste("Processed on ", lubridate::today(),
                                   "\nwith 's_irrad_corrected()' from 'ooacquire' ver. ",
                                   utils::packageVersion("ooacquire"),
                                   "\n\nfrom files:\n", comment.txt, sep = "")
  corrected.spct

}

#' @describeIn s_irrad_corrected Default for generic function.
#' @export
s_irrad_corrected.raw_mspct <- function(x,
                                        method,
                                        return.cps = FALSE,
                                        verbose = FALSE,
                                        ...) {
  if (length(x[["light"]]) == 0) {
    if (verbose) {
      warning("'raw_spct' object with name 'light' missing")
    }
    if (return.cps) {
      return(cps_spct())
    } else {
      return(source_spct())
    }
  }

  corrected.spct <-
         ooacquire::uvb_corrections(x = x[["light"]],
                                    flt = x[["filter"]],
                                    dark = x[["dark"]],
                                    stray.light.method = method[["stray.light.method"]],
                                    stray.light.wl = method[["stray.light.wl"]],
                                    flt.dark.wl = method[["flt.dark.wl"]],
                                    flt.ref.wl = method[["flt.ref.wl"]],
                                    worker.fun = method[["worker.fun"]],
                                    trim = method[["trim"]],
                                    verbose = verbose)

  if (return.cps) {
    corrected.spct
  } else {
    photobiology::cps2irrad(corrected.spct, ...)
  }
}

#' @describeIn s_irrad_corrected Default for generic function.
#' @export
s_irrad_corrected.raw_spct <- function(x,
                                       method,
                                       return.cps = FALSE,
                                       verbose = FALSE,
                                       ...) {
  raw.mspct <- raw_mspct(list(light = x))
  s_irrad_corrected(x = raw.mspct,
                    method = method,
                    return.cps = return.cps,
                    verbose = verbose,
                    ...)
}

#' Select which instrument descriptor to use
#'
#' Select from a list of instrument descriptors which one to use based on
#' date of measurement.
#'
#' @param date Any object that \code{anytime::anydate()} will decode as a date
#'   or convert to a date. Used to select a descriptor containing calibration
#'   data valid for a given day.
#' @param descriptors A named list of descriptors of the characteristics of
#'   the spectrometer including calibration data.
#' @param verbose Logical indicating the level of warnings wanted.
#' @param ... Currently ignored.
#'
#' @note The default argument for \code{verbose} is for this function
#'   \code{TRUE} unless the argument to \code{date} is a date object, as
#'   conversion of other objects to a date may fail.
#'
#'   If a character string is passed as argument to \code{date}, it must be
#'   in a format suitable for \code{anytime::anydate()}. One needs to be
#'   careful with months and days of the month when supplying them as numbers,
#'   so using months names or their abbreviations may be safer.
#'
#' @export
#'
which_descriptor <- function(date = lubridate::today(),
                             descriptors = ooacquire::MAYP11278_descriptors,
                             verbose = NULL,
                             ...) {
  if (is.null(verbose)) {
    verbose <- !lubridate::is.instant(date)
  }

  if (!lubridate::is.Date(date)) {
    date <- anytime::anydate(date)
  }

  for (d in rev(names(descriptors))) {
    if (descriptors[[d]][["inst.calib"]][["start.date"]] < date &
        descriptors[[d]][["inst.calib"]][["end.date"]] > date) {
      if (verbose) {
        message("Descriptor ", d, " selected for ", date)
      }
      return(descriptors[[d]])
    }
  }

  if (verbose) {
    warning("No valid descriptor found for ", date)
  } else {
    message("No valid descriptor found for ", date)
  }
  return(list())
}