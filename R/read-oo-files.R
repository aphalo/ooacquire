#' Read Files Saved by Ocean Optics' instruments or software.
#'
#' Reads and parses the header of a processed data file as output by
#' SpectraSuite to extract the whole header remark field. The time field is
#' retrieved and decoded.
#'
#' @param file character string
#' @param time a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} no date variable is added
#' @param geocode A data frame with columns \code{lon} and \code{lat}.
#' @param label character string, but if \code{NULL} the value of \code{file} is
#'   used, and if \code{NA} the "what.measured" attribute is not set.
#' @param descriptor list A list describing the instrument used.
#' @param tz character Time zone is by default read from the file.
#' @param locale	The locale controls defaults that vary from place to place. The
#'   default locale is US-centric (like R), but you can use
#'   \code{\link[readr]{locale}} to create your own locale that controls things
#'   like the default time zone, encoding, decimal mark, big mark, and day/month
#'   names.
#' @param verbose Logical indicating the level of warnings wanted.
#'
#' @return A raw_spct object.
#' @export
#'
read_oo_data <- function(file,
                           time = NULL,
                           geocode = NULL,
                           label = NULL,
                           descriptor = NULL,
                           tz = NULL,
                           locale = NULL,
                           verbose = getOption("photobiology.verbose", default = FALSE)) {

  line01 <- scan(file = file, nlines =  1, skip = 0,
                 what = "character", quiet = !verbose)
  if (line01[1] == "SpectraSuite") {
    .read.fun <- read_oo_ssdata
  } else if (line01[1] == "Data" && line01[4] == "Node") {
    # we guess that we are dealing with an OceanView file
    .read.fun <- read_oo_ovdata
  } else {
    stop("File format of '", basename(file), "' not recognized!")
  }
  .read.fun(file = file,
                 time = time,
                 geocode = geocode,
                 label = label,
                 descriptor = descriptor,
                 tz = tz,
                 locale = locale,
                 verbose = verbose)
}

#' Read multiple files into raw_mspct object
#'
#' Read multiple files and return a collection of raw spectra as a raw_spct
#' object.
#'
#' @param files a named list of character strings of file names
#' @param read.f a function which expects in its first parameter a file name or
#' file path
#' @param ... additional arguments passed by name to the function passed as
#' argument to \code{read.f}
#'
#' @note Depending of the function passed to \code{read.f} more or less complete
#' metadata information will be stored as attributes in the raw_spct objects.
#'
#' @export
#'
read_files2mspct <- function(files, read.f = ooacquire::read_oo_data, ...) {
  stopifnot(is.list(files))
  spectra.lst <- list()
  i <- 0
  for (f in files) {
    i <- i + 1
    if (length(f) == 1) {
      spectra.lst[[ names(files)[i] ]] <- read.f(f, ...)
    } else if (length(f > 1)) {
      temp.lst <- list()
      j <- 0
      for (ff in f) {
        j <- j + 1
        temp.lst[[j]] <- read.f(ff, ...)
      }
      temp.mspct <- raw_mspct(temp.lst)
      spectra.lst[[ names(files)[i] ]] <- merge_raw_mspct(temp.mspct)
    }
  }
  raw_mspct(spectra.lst)
}
