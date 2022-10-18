filter_correction <- function(x, reference_range, target_range) {
  ## stray light estimate
  # constant values
  filter_cutoff <- 379.5
  short_wl_range <- c(193, 209.5)
  medium_wl_range <- c(360, filter_cutoff)

  # ranges by name
  cts$range <- with(cts, ifelse(w.length > short_wl_range[1] & w.length < short_wl_range[2],
                                "short",
                                ifelse(w.length > medium_wl_range[1] & w.length < medium_wl_range[2],
                                       "medium", "other")))
  short.idxs <- cts[["range"]] == "short"
  # stray light estimates
  mean_flt_cs_short <-
    mean(cts[["filter_cs"]][short.idxs],
         trim = trim, na.rm = TRUE)
  mean_merged_cs_short <-
    mean(cts[["merged_cs"]][short.idxs],
         trim = trim, na.rm = TRUE)
  if (method=="original") {
    cts[["filter_ratio"]] <- with(cts, filter_cs / merged_cs)
    if (verbose && any(is.na(cts[["filter_ratio"]][short.idxs]))) {
      warning(paste(sum(is.na(cts[["filter_ratio"]][short.idxs])),
                    "NAs in filter_ratio"))
    }
    mean_flt_ratio_short <-
        mean(cts[["filter_ratio"]][short.idxs],
             trim = trim, na.rm = TRUE)
  }

}
