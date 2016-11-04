# read_m_oo_ssdata <- function(files,
#                              names = NULL,
#                              date = NULL,
#                              geocode = NULL,
#                              label = NULL,
#                              tz = NULL,
#                              locale = readr::default_locale()) {
#   names <- names(files)
#   spcts <- list()
#   i <- 0L
#   for (f in file) {
#     print(f)
#     i <- i + 1L
#     temp.spct <-
#       photobiologyInOut::read_oo_ssdata(f, date, geocode, label, tz, locale)
#     spcts[[names[i]]] <- temp.spct
#   }
#   raw_mspct(spcts)
# }

#' Merge raw spectra into a single multicolumn spectrum
#'
#' @export
#'
merge_raw_mspct <- function(x) {

  stopifnot(photobiology::is.raw_mspct(x))

  num.reps <- length(x)

  if (num.reps <= 1) {
    # nothing to merge
    return(x)
  }

  # we sorth the members of the collection in ascending order of
  # integration time. This allows use of a simpler algorithm.
  integ.times <-
    photobiology::msaply(x,
            .fun = function(xx)
            {getInstrSettings(xx)[["integ.time"]]})

  x <- x[order(integ.times)]

  # a weak data sanity check that ensures that at least all spectra
  # are consistent.

  for (s in x[-1]) {
    stopifnot(all.equal(x[[1]][["w.length"]], s[["w.length"]]))
  }


  z <- tibble::tibble(w.length = x[[1]][["w.length"]])

  integ.time <- numeric()
  num.scans <- numeric()
  for (i in 1:num.reps) {
    temp.settings <- getInstrSettings(x[[i]])
    integ.time <- c(integ.time, temp.settings[["integ.time"]])
    num.scans <- c(num.scans, temp.settings[["num.scans"]])
    counts.name <- paste("counts", i, sep = "_")
    z[[counts.name]] <- x[[i]][["counts"]]
   }
  z <- photobiology::as.raw_spct(z)
  z <- photobiology::copy_attributes(x[[1]], z)

  # merge metadata
  settings <- getInstrSettings(x[[1]])
  settings[["integ.time"]] <- integ.time
  settings[["num.scans"]] <- num.scans
  settings[["total.time"]] <- integ.time * num.scans

  photobiology::setInstrSettings(z, settings)
  z
}
