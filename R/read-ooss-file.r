#' Read File Saved by Ocean Optics' SpectraSuite.
#'
#' Reads and parses the header of a processed data file as output by
#' SpectraSuite to extract the whole header remark field. The time field is
#' retrieved and decoded.
#'
#' @param file character string
#' @param date a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} no date variable is added
#' @param geocode A data frame with columns \code{lon} and \code{lat}.
#' @param label character string, but if \code{NULL} the value of \code{file} is
#'   used, and if \code{NA} the "what.measured" attribute is not set.
#' @param tz character Time zone is by default read from the file.
#' @param locale	The locale controls defaults that vary from place to place. The
#'   default locale is US-centric (like R), but you can use
#'   \code{\link[readr]{locale}} to create your own locale that controls things
#'   like the default time zone, encoding, decimal mark, big mark, and day/month
#'   names.
#'
#' @return A raw_spct object.
#' @export
#'
read_oo_ssdata <- function(file,
                           date = NULL,
                           geocode = NULL,
                           label = NULL,
                           tz = NULL,
                           locale = readr::default_locale()) {
  if (is.null(tz)) {
    tz <- locale$tz
  }
  if (is.null(label)) {
    label <- paste("File:", file)
  }
  line01 <- scan(file = file, nlines =  1, skip = 0, what = "character")
  if (line01[1] != "SpectraSuite") {
    warning("Input file was not created by SpectrSuite as expected: skipping")
    return(NA)
  }
  file_header <- scan(file = file, nlines = 20,
                      skip = 0, what = "character", sep = "\n")

  if (is.null(date)) {
    line03 <- sub("Date: [[:alpha:]]{3} ", "", file_header[3])
    if (is.null(tz)) {
      tz <- sub("^(.{16})([[:upper:]]{3,4})(.{5})$", "\\2", line03)
      if (nchar(tz) == 4) {
        tz <- sub("S", "", tz)
      }
    }
    date <- lubridate::parse_date_time(line03, "m*!d! hms y", tz = tz)
  }

  data.rows <- oofile_data_rows(file_header)

  z <- readr::read_tsv(
    file = file,
    col_names = c("w.length", "counts"),
    skip =  data.rows[["skip"]],
    n_max = data.rows[["npixels"]],
    locale = locale
  )

  old.opts <- options("photobiology.strict.range" = NA)
  z <- photobiology::as.raw_spct(z)
  options(old.opts)

  comment(z) <-
    paste(paste(file_header[1:(data.rows[["skip"]] - 1)], collapse = "\n"),
          "^^^^ end of file header ^^^^",
          paste("Ocean Optics Spectra Suite raw counts file '", file, "' imported on ",
                lubridate::now(tzone = "UTC"), " UTC", sep = ""),
          sep = "\n")

  photobiology::setWhenMeasured(z, date)
  photobiology::setWhereMeasured(z, geocode)
  photobiology::setWhatMeasured(z, label)
  z
}
