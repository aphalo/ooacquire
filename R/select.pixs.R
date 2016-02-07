#' Function for selecting pixels that belong to a waveband
#'
#' This function returns a logical vector, with TRUE values for pixels
#' in the waveband and FALSE for other pixels.
#'
#' @param cal_idx numeric index
#' @param w.band a "w.band" object defining a waveband of the spectrum.
#' @keywords internal
#'
select_pixs <- function(cal_idx, w.band) {
  calib <- calibs.df[["calib"]][[cal_idx]]
  return(calib$w.length >= min(w.band) & calib$w.length >= max(w.band))
}
