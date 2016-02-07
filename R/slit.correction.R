#' Low level function for applying the slit and stray light corrections, and
#' bracketing.
#'
#' @param cts data table with wavelengths (nm) and counts
#' @param bracketing logical.
#' @param integration_time_short Short integration time in us.
#' @param integration_time_long Long integration time in us.
#' @param cal_idx A numeric index used to access calibration data
#' @param method Method variant used "original" (Ylianttila), "full" (Aphalo),
#'   "sun" (full with trimming)
#' @param has_filter a logical
#' @param trim a numeric value to be used as argument for mean
#' @param verbose Logical indicating the level of warnings wanted. Defaults to
#'   \code{FALSE}.
#' @return Data table with two numeric components "wavelength" and "cts_second".
#'   \describe{
#'   \item{w.length}{numeric array of wavelengths in nm.}
#'   \item{cts_second}{uncalibrated but corrected detector counts in counts per
#'   second}
#'   \item{cal.multiplier}{calibration multipliers}}
#' @author Pedro J. Aphalo, algorithm developed by Lasse Ylianttila.
#' @export
#' @references \url{http://www.r4photobiology.info}
#' @keywords misc
#'
slit_correction <- function(cts,
                            bracketing,
                            integration_time_short,
                            integration_time_long = NA,
                            cal_idx,
                            method,
                            has_filter,
                            trim=NULL,
                            verbose = FALSE)
{
  if (is.null(trim)) {
    if (method=="original") {
      trim <- 0.0
    } else {
      trim <- 0.2
    }
  }
  if (is.null(trim)) {
    message("Thsi message should never get printed!")
  }
  # substract dark signal and linearize then convert counts to counts / second
  cts$short_ctsc <- linearize_count(cal_idx, cts$short_meas_cts - cts$short_dark_cts,
                                         force_zero=FALSE, verbose=verbose))
  cts$short_cs <- cts$short_ctsc / integration_time_short
  if (verbose && any(is.na(cts$short_cs))) {
    warning("NAs in short_cs")
  }
  if (bracketing){
    cts$long_ctsc <- linearize_count(cal_idx, cts$long_meas_cts - cts$long_dark_cts,
                                     force_zero=FALSE, verbose=verbose)
    cts$long_cs <- cts$long_ctsc  / integration_time_long
    if (verbose && any(is.na(cts$long_cs))) {
      warning("NAs in long_cs")
    }
    if (has_filter) {
      cts$filter_ctsc <- linearize_count(cal_idx, cts$long_filter_cts - cts$long_dark_cts,
                                         force_zero=method=="original", verbose=verbose)
      cts$filter_cs <- cts$filter_ctsc / integration_time_long
      if (verbose && any(is.na(cts$filter_cs))) {
        warning("NAs in filter_cs")
      }
    }
  } else {
    if (has_filter) { # not as in Lasse's worksheet
      cts$filter_ctsc <- linearize_count(cal_idx, cts$short_filter_cts - cts$short_dark_cts,
                                         force_zero=method=="original", verbose=verbose)]
      cts$filter_cs := cts$filter_ctsc / integration_time_short
      if (verbose && any(is.na(cts$filter_cs))) {
        warning("NAs in filter_cs")
      }
    }
  }
  # combine short and long measurements
#  setkey(cts, w.length)
  switch_point_counts <- 35000 # Lasse used 35000 and long_meas_cts
  if (bracketing) {
    cts <- dplyr::mutate(cts,
                         merged_cs = ifelse(long_meas_cts > switch_point_counts, short_cs, long_cs))
  }
  else {
    cts$merged_cs <- cts$short_cs
  }
  if (verbose && any(cts$merged_cs < 0.0)) {
    warning(paste(with(cts, sum(merged_cs < 0.0)), "negative values in merged_cs."))
  }
  if (verbose && any(cts$merged_cs == 0.0)) {
    warning(paste(with(cts, sum(merged_cs == 0.0)), "zeros in merged_cs, at wavelengths."))
  }
  if (has_filter) {
    if (verbose && any(cts$filter_cs < 0.0)) {
      warning(paste(with(cts, sum(filter_cs < 0.0)), "negative values in filter_cs."))
    }
    ## stray light estimate
    # constant values
    filter_cutoff <- 379.5
    short_wl_range <- c(193, 209.5)
    medium_wl_range <- c(360, filter_cutoff)

    # ranges by name
    # nested ifelse seems to slow down and eventually crash R
#    setkey(cts, w.length)
    cts$range <- with(cts, ifelse(w.length > short_wl_range[1] & w.length < short_wl_range[2],
                                  "short",
                                  ifelse(w.length > medium_wl_range[1] & w.length < medium_wl_range[2],
                                         "medium", "other")))
#    setkey(cts, range)

    # stray light estimates
    # cts["short", filter_cs := runmed(filter_cs, 3, endrule="median") ]
    # cts["short", merged_cs := runmed(merged_cs, 3, endrule="median") ]
    if (!verbose) {
#      suppressWarnings(mean_flt_cs_short <- cts["short", mean(filter_cs, trim=trim, na.rm=TRUE)])
#      suppressWarnings(mean_merged_cs_short <- cts["short", mean(merged_cs, trim=trim, na.rm=TRUE)])
       mean_flt_cs_short <-
         mean(dplyr::filter(cts, range == "short")[["filter_cs"]],
              trim = trim, na.rm = TRUE)
       mean_merged_cs_short <-
         mean(dplyr::filter(cts, range == "short")[["merged_cs"]],
              trim = trim, na.rm = TRUE)
    } else {
      mean_flt_cs_short <-
        mean(dplyr::filter(cts, range == "short")[["filter_cs"]],
             trim = trim, na.rm = TRUE)
      mean_merged_cs_short <-
        mean(dplyr::filter(cts, range == "short")[["merged_cs"]],
             trim = trim, na.rm = TRUE)
    }
    if (method=="original") {
      cts <- dplyr::mutate(cts, filter_ratio = filter_cs / merged_cs)
      if (verbose && any(is.na(dplyr::filter(cts, range = "short")[["filter_ratio"]]))) {
        warning(paste(sum(is.na(dplyr::filter(cts, range = "short")[["filter_ratio"]])),
                      "NAs in filter_ratio"))
      }
      if (!verbose) {
#        suppressWarnings(mean_flt_ratio_short <- cts["short", mean(filter_ratio, trim=trim, na.rm=TRUE)])
        mean_flt_ratio_short <-
          mean(dplyr::filter(cts, range == "short")[["filter_ratio"]],
               trim = trim, na.rm = TRUE)
      } else {
        mean_flt_ratio_short <-
          mean(dplyr::filter(cts, range == "short")[["filter_ratio"]],
               trim = trim, na.rm = TRUE)
      }
    }
    else if (method == "full" || method == "sun" || method == "raw") {
      if (mean_flt_cs_short <= 0.0 || mean_merged_cs_short <= 0.0) {
        mean_flt_ratio_short <- 1.0
      } else {
        mean_flt_ratio_short <- mean_flt_cs_short / mean_merged_cs_short
      }
    }
    else {
      stop(paste("method", method, "not supported"))
    }

    # diagnosis and correction of bad estimates
    if (is.na(mean_flt_ratio_short)) {
      warning("NA in mean_flt_ratio_short, replaced with 1.0")
      mean_flt_ratio_short <- 1.0
    }
    if (mean_flt_ratio_short < 0.8) {
      # This is a guess based on PC filter transmittance of about 83%.
      if (verbose) warning("mean_flt_ratio_short < 0.8, was ", signif(mean_flt_ratio_short, 4), " reset to 1.0")
      mean_flt_ratio_short <- 1.0
    } else
    if (mean_flt_ratio_short > 1.1) {
      # This could be set to 1.0 as it makes no sense to have more noise with the filter than without it!
      if (verbose) warning("mean_flt_ratio_short > 1.1, was ", signif(mean_flt_ratio_short, 4), " reset to 1.0")
      mean_flt_ratio_short <- 1.0
      } else {
      if (verbose) warning("mean_flt_ratio_short is ", signif(mean_flt_ratio_short, 4))
    }
#    mean_flt_cs_medium <- cts["medium", mean(filter_cs, na.rm=TRUE)]
    mean_flt_cs_medium <-
      mean(dplyr::filter(cts, range == "medium")[["filter_cs"]],
           trim = trim, na.rm = TRUE)
    if (verbose && (mean_flt_cs_medium / mean_flt_cs_short) > 1.0) {
      warning("There is more noise at", medium_wl_range[1], " to ", medium_wl_range[2], " nm than at ", short_wl_range[1],
              " to ", short_wl_range[2], " nm, ratio: ", signif(mean_flt_cs_medium / mean_flt_cs_short, 3))
    }

    # substraction of stray light
    first_correction <- ifelse(mean_flt_cs_medium > 0.0, mean_flt_cs_medium/mean_flt_ratio_short, 0.0)
    cts <- dplyr::mutate(cts,
                         filetr_cs = ifelse(filter_cs < 0.0, 0.0, filter_cs),
                         merged_fltc_cs = ifelse(w.length < filter_cutoff,
                                                 merged_cs - filter_cs / mean_flt_ratio_short,
                                                 merged_cs - first_correction))
  }
  else {
    cts <- dplyr::mutate(cts,
                         merged_fltc_cs = merged_cs)
  }
  # tail correction
  tail_add <- apply_tail_correction(cts[["w.length"]], cts[["merged_fltc_cs"]])
  cts <- cbind(cts, tail_add)
#  cts[ , names(tail_add):=tail_add]
#  cts[ , tail_removed := merged_fltc_cs - tail]
#  setkey(cts, w.length) # ensures sorted w.length in returned object
  if (!verbose) {
    suppressWarnings(stray_light <- cts[w.length > 218.5 & w.length < 228.5, mean(tail_removed, trim=trim, na.rm=TRUE)])
  } else {
    stray_light <- cts[w.length > 218.5 & w.length < 228.5, mean(tail_removed, trim=trim, na.rm=TRUE)]
  }
#  if (method=="original") {
    stray_light <- stray_light + cts[w.length > 218.5 & w.length < 228.5, sd(tail_removed, na.rm=TRUE)]
#  }
  if (stray_light < 0.0) stray_light <- 0.0
  cts[ , cts_second := tail_removed - stray_light]
  cts[ cts_second < 0.0, cts_second := 0.0]
  # bad pixels and out-of-range pixels are marked as NA in cal.multiplier
  # in which case we remove the whole row of data for those pixels
  if (verbose) return(cts[!is.na(cal.multiplier), ]) else return(cts[!is.na(cal.multiplier), list(w.length, cts_second, cal.multiplier)])
}
