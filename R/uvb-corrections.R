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
                            worker_fun = ooacquire::maya_tail_correction,
                            trim = 0,
                            verbose = FALSE,
                            ...) {

  spct.worker <- function(x) {
    x <- trim_counts(x)
    x <- bleed_nas(x)
    x <- linearize_counts(x)
    if (length(flt.dark.wl) >= 1) {
      x <- fshift(x, range = flt.dark.wl)
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
  inst.descriptor <- getInstrDesc(x)
  inst.settings <- getInstrDesc(x)
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
                           method = method,
                           flt.dark.wl = flt.dark.wl,
                           flt.ref.wl = flt.ref.wl,
                           trim = trim,
                           verbose = verbose)
  }

  z <- slit_function_correction(x, worker_fun = worker_fun, ...)

  z.cps <- z[["cps"]][z$w.length > stray.light.wl[1] &
                        z$w.length < stray.light.wl[2]]
  stray.light <-
    mean(z.cps, trim = trim, na.rm = TRUE)
  if (method == "original") {
    stray.light <- stray.light +
      stats::sd(z.cps, na.rm = TRUE)
  }
  if (stray.light < 0.0) {
    if (verbose) {
      warning("Straylight estimate < 0")
    }
    # stray.light <- 0
    # Lasse discards straylight estimate if < 0
    # changed to avoid negative counts but should be changed
  }
  z <- z - stray.light

  z <- setInstrDesc(z, inst.descriptor)
  z <- setInstrSettings(z, inst.settings)
  z
}

#' @rdname uvb_corrections
#'
#' @export
#'
slit_function_correction <- function(x,
                                     worker_fun = ooacquire::maya_tail_correction,
                                     verbose = TRUE,
                                     ...) {
  stopifnot(is.cps_spct(x))
  stopifnot(is.null(attr(x, "slit.corrected")) || !attr(x, "slit.corrected"))
#  x <- clean(x)
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
  new.cps <- worker_fun(x[["w.length"]], x[["cps"]], ...)
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
                              method = "original",
                              flt.dark.wl = c(193, 209.5),
                              flt.ref.wl = c(360, 379.5),
                              trim = 0,
                              verbose = FALSE) {
  stopifnot(is.null(attr(x, "flt.corrected")) || !attr(x, "flt.corrected"))
  stopifnot(is.cps_spct(x) && is.cps_spct(flt))
  stopifnot(range(x) == range(flt) && length(x) == length(flt))
  counts.cols <- length(grep("^cps", names(x), value = TRUE))
  if (counts.cols > 1) {
    warning("Multiple 'cps' variables found in 'x': merging them before continuing!")
    x <- merge_cps(x)
  }
  counts.cols <- length(grep("^cps", names(flt), value = TRUE))
  if (counts.cols > 1) {
    warning("Multiple 'cps' variables found in 'flt': merging them before continuing!")
    flt <- merge_cps(flt)
  }

  if (verbose && any(flt[["cps"]] < 0.0)) {
    warning(paste(sum(flt[["cps"]] < 0.0)), 'negative values in flt[["cps"]].')
  }
  ## stray light estimate
  # ranges by name
  x[["range"]] <- flt[["range"]] <- ifelse(x[["w.length"]] > flt.dark.wl[1] & x[["w.length"]] < flt.dark.wl[2],
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
      mean(dplyr::filter(flt, range == "short")[["filter_ratio"]],
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
  flt[["cps"]] <- ifelse(flt[["cps"]] < 0.0, 0.0, flt[["cps"]])
  x[["cps"]]  <- ifelse(x[["w.length"]] < flt.ref.wl[2],
                                       x[["cps"]] - flt[["cps"]] / mean_flt_ratio_short,
                                       x[["cps"]] - first_correction)
  attr(x, "flt.corrected") <- TRUE

  x
}

#' Convert raw counts data into spectral irradiance
#'
#' @param x A named list of one to hree vectors of file names, with names
#'   "light", "filter", and "dark".
#' @param method.data A named list of constants and functions defining the
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
#' @export
s_irrad_corrected.list <- function(x,
                                   method.data,
                                   return.cps = FALSE,
                                   descriptor,
                                   locale,
                                   verbose = FALSE,
                                   ...) {
  if(length(x[["light"]]) == 0) {
    if (verbose) {
      warning("Filename(s) with name 'light' missing")
    }
    if (return.cps) {
      return(cps_spct())
    } else {
      return(source_spct())
    }
  }
  raw.mspct <-
    ooacquire::read_files2mspct(x,
                                locale = locale,
                                inst.descriptor = descriptor)
  s_irrad_corrected(x = raw.mspct,
                    method.data = method.data,
                    return.cps = return.cps,
                    verbose = verbose,
                    ...)
}

#' @describeIn s_irrad_corrected Default for generic function.
#' @export
s_irrad_corrected.raw_mspct <- function(x,
                                        method.data,
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
    with(method.data,
         ooacquire::uvb_corrections(x = x[["light"]],
                                    flt = x[["filter"]],
                                    dark = x[["dark"]],
                                    method = method,
                                    stray.light.wl = stray.light.wl,
                                    flt.dark.wl = flt.dark.wl,
                                    flt.ref.wl = flt.ref.wl,
                                    worker_fun = worker_fun,
                                    trim = trim,
                                    verbose = verbose)
    )
  if (return.cps) {
    corrected.spct
  } else {
    photobiology::cps2irrad(corrected.spct, ...)
  }
}

#' Select which instrument descriptor to use
#'
#' Select from a list of instrument descriptors which one to use based on
#' date of measurement.
#'
#' @param date Any object that lubridate::as_date() will decode as a date or
#'   convert to a date. Used only to override the selection of the descriptor
#'   containing calibration data.
#' @param descriptors A named list of descriptors of the characteristics of
#'   the spectrometer including calibration data.
#' @param verbose Logical indicating the level of warnings wanted.
#' @param ... Currently ignored.
#'
#' @export
#'
which_descriptor <- function(date = lubridate::today(),
                             descriptors = ooacquire::MAYP11278_descriptors,
                             verbose = FALSE,
                             ...) {
  for (d in rev(names(descriptors))) {
    if (descriptors[[d]][["inst.calib"]][["start.date"]] < date &
        descriptors[[d]][["inst.calib"]][["end.date"]] > date) {
      if (verbose) {
        message("Descriptor ", d, "selected")
      }
      return(descriptors[[d]])
    }
  }
  if (verbose) {
    warning("No valid descriptor found")
  }
  return(list())
}