filter_correction <- function(x, reference_range, target_range) {
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

}