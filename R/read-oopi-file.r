#' Read File Saved by Ocean Optics' Raspberry Pi software.
#'
#' Reads and parses the header of a raw data file as output by the server
#' running on a Raspberry Pi board to extract the whole header remark field. The
#' time field is retrieved and decoded.
#'
#' @param file character string
#' @param date a \code{POSIXct} object, but if \code{NULL} file modification
#'   date is used with a warning and if \code{NA} date is set to NA.
#' @param geocode A data frame with columns \code{lon} and \code{lat}.
#' @param label character string, but if \code{NULL} the value of \code{file} is
#'   used, and if \code{NA} the "what.measured" attribute is not set.
#' @param tz character Time zone is not saved to the file.
#' @param locale	The locale controls defaults that vary from place to place. The
#'   default locale is US-centric (like R), but you can use
#'   \code{\link[readr]{locale}} to create your own locale that controls things
#'   like the default time zone, encoding, decimal mark, big mark, and day/month
#'   names.
#' @param descriptor list as returned by function \code{get_oo_descriptor}.
#' @param corr.sensor.nl logical, indicating if spectral data is already
#'   linearized. If TRUE the spectrum is marked as linearized, and
#'   linearization skipped during processing.
#' @param spectrometer.name,spectrometer.sn character.
#' @param npixels integer Number of pixels in spectral data.
#'
#' @return A raw_spct object.
#'
#' @family functions for importing spectral data from files
#'
#' @export
#'
#' @references \url{http://www.r4photobiology.info}
#'
#' @note The header in these files has very little information, so the user
#' needs to supply the number of pixels in the array as well as the date-time.
#' The file contains a date in milliseconds but as the Raspberry Pi board
#' contains no real-time clock, it seems to default to number of milliseconds
#' since the Pi was switched on.
#'
read_oo_pidata <- function(file,
                           date = NULL,
                           geocode = NULL,
                           label = NULL,
                           tz = NULL,
                           locale = readr::default_locale(),
                           descriptor = NULL,
                           corr.sensor.nl = FALSE,
                           spectrometer.name = "Unknown via Raspberry Pi",
                           spectrometer.sn = "Unknown via Raspberry Pi",
                           npixels = 2048) {
  if (is.null(tz)) {
    tz <- locale$tz
  }
  if (is.null(label)) {
    label <- paste("File:", file)
  }
  if (is.null(date)) {
    date <- file.mtime(file)
    warning("Setting spectrum date to file modification date: ", format(date), "\nCheck! (used to select calibration).")
  } else if (is.na(date)) {
    warning("Setting date to 'NA'.\nAdd! (used to select calibration).")
    date <- as.POSIXct(NA_real_, origin = lubridate::origin)
  } else if (!is.POSIXct(date)) {
    stop("Argument passed to 'date' should be a POSIXct object.\nUse functions from package 'lubirdate' or 'anytime' for conversions.")
  }
  file.header <- scan(file = file, nlines = 10,
                      skip = 0, what = "character", sep = "\n")

  my.gr <- dplyr::data_frame_(list(
    feature = ~c("date",
                 "boxcar",
                 "integ.time",
                 "scans"),
    pattern = ~c("Saved at time:",
                 "Boxcar smoothing:",
                 "Integration time:",
                 "Scans to average:")
  ))

  lines.map <- map_oofile_header_rows(file.header,
                                      header.end = 10,
                                      grammar = my.gr)

  # if (is.null(date)) {
  #   date <- sub("Saved at time: ", "",
  #               file.header[lines.map[["date"]]], fixed = TRUE)
  #   # needs further decoding if possible
  # }

  inst.settings <-
    list(
      # user settings
      pix.selector = TRUE,
      # instrument settings
      correct.elec.dark =
        NA_integer_,
      corr.sensor.nl =
        corr.sensor.nl,
      correct.stray.light =
        NA_integer_,
      boxcar.width =
        as.integer(stringr::str_split(file.header[lines.map[["boxcar"]]], " ")[[1]][3]),
      integ.time =
        as.numeric(stringr::str_split(file.header[lines.map[["integ.time"]]], " ")[[1]][3]), # * 1e-6 # micro-seconds -> seconds
      num.scans =
        as.integer(stringr::str_split(file.header[lines.map[["scans"]]], " ")[[1]][4])
    )
  # processing flag
  inst.settings[["linearized"]] <- as.logical(inst.settings[["corr.sensor.nl"]])
  # diagnosis
  inst.settings[["tot.time"]] = with(inst.settings, integ.time * num.scans)

  if (is.null(descriptor)) {
    descriptor <-
      list(
        time = date,
        w = NULL,
        sr.index = NA_integer_,
        ch.index = NA_integer_,
        spectrometer.name = spectrometer.name,
        spectrometer.sn =  spectrometer.sn,
        bench.grating = NA_character_,
        bench.filter = NA_character_,
        bench.slit = NA_character_,
        min.integ.time = NA_real_,
        max.integ.time = NA_real_,
        max.counts = NA_integer_,
        wavelengths = NA_real_,
        bad.pixs = numeric(),
        inst.calib = NA
      )
  }

  skip <- which(stringr::str_detect(file.header, "Wavelengths"))

  z <- readr::read_tsv(
    file = file,
    col_names = c("w.length", "counts"),
    skip = skip,
    n_max = npixels,
    locale = locale
  )

  z <- photobiology::as.raw_spct(z)

  inst.settings[["rel.signal"]] = max(z[["counts"]]) / descriptor[["max.counts"]]

  comment(z) <-
    paste(paste(file.header[1:(skip - 1)], collapse = "\n"),
          "^^^^ end of file header ^^^^",
          paste("Ocean Optics Raspberry Pi raw counts file '", file, "' imported on ",
                lubridate::now(tzone = "UTC"), " UTC", sep = ""),
          sep = "\n")

  photobiology::setWhenMeasured(z, date)
  photobiology::setWhereMeasured(z, geocode)
  photobiology::setWhatMeasured(z, label)
  photobiology::setInstrDesc(z, descriptor)
  photobiology::setInstrSettings(z, inst.settings)
  z
}

