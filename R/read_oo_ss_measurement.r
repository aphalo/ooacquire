read_m_oo_ssdata <- function(file,
                             names = NULL,
                             date = NULL,
                             geocode = NULL,
                             label = NULL,
                             tz = NULL,
                             locale = readr::default_locale()) {
  if (is.null(names)) {
    names <- names(file)
  }
  spcts <- list()
  i <- 0L
  for (f in file) {
    print(f)
    i <- i + 1L
    temp.spct <-
      photobiologyInOut::read_oo_ssdata(f, date, geocode, label, tz, locale)
    spcts[[names[i]]] <- temp.spct
  }
  raw_mspct(spcts)
}

#' Read one (HDR) measurement from files
#'
#' Read
merge_raw_mspct <- function(x,
                            label = NULL) {

  num.reps <- length(x)

  if (num.reps <= 1) {
    # nothing to merge
    return(x)
  }

  # we sorth the members of the collection in ascending order of
  # integration time. This allows use of a simpler algorithm.
  integ.times <-
    msapply(mspct,
            .fun = function(x)
            {getInstrSettings(x)[["integ.time"]]})

  x <- x[order(integ.times)]

  # a weak data sanity check that ensures that at least all spectra
  # are consistent.

  for (s in x[-1]) {
    stopifnot(all.equal(x[[1]][["w.length"]], x[[s]][["w.length"]]))
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
  z <- as.raw_spct(z)
  z <- copy_attributes(x[[1]], z)


  # merge metadata
  settings[["integ.time"]] <- integ.time
  settings[["num.scans"]] <- num.scans
  settings[["total.time"]] <- integ.time * num.scans


  new_settings <- settings[[1]]
  new_settings[["integ.time"]] <- c()

  photobiology::setInstrDesc(z, y)
  photobiology::setInstrSettings(z, x)
  photobiology::setWhenMeasured(z, start.time)
  photobiology::setWhereMeasured(z, where.measured)
  photobiology::setWhatMeasured(z, what.measured)
  z
}
