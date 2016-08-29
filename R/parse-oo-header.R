#' Set the values of instrument settings from file header
#'
#' Parse the header of the file returned by SpectraSuite for corrections,
#' smotthing and acquisition parameters used. These values are used to set
#' the "inst.settings" atrribute of \code{x}.
#'
#' @param x generic_spct, although with defaults only raw_spct.
#' @param file.header character string The header of the file output by
#'   SpectraSuite.
#' @param overwrite logical A flag indicating if an existing instrument
#'   descriptor in \code{x} should be overwritten.
#'
#' @return A copy of \code{x} with updated attributes.
#' @export
#'
set_oo_ssdata_settings <- function(x,
                                   file.header = comment(x),
                                   overwrite = FALSE) {
  stopifnot(length(file.header) == 1)
  stopifnot(overwrite || is.na(getInstrSettings(x)))
  lines <- stringr::str_split(file.header, "\n")[[1]]

  my.gr <- dplyr::data_frame_(list(
    feature = ~c("dark.corr", "lin.corr", "stray.corr", "boxcar",
                 "integ.time", "scans", "sn"),
    pattern = ~c("Correct for Electrical Dark",
                 "Correct for Detector Non-linearity:",
                 "Correct for Stray Light:",
                 "Boxcar Smoothing:",
                 "Integration Time",
                 "Spectra Averaged:",
                 "Spectrometers:")
  ))

  lines.map <- map_oofile_header_rows(lines,
                                      header.end = 20,
                                      grammar = my.gr)

  inst.settings <-
    list(
      # user settings
      pix.selector = TRUE,
      # instrument settings
      correct.elec.dark =
        as.integer(grepl("Yes", lines[lines.map[["dark.corr"]]], fixed = TRUE)),
      corr.sensor.nl =
        as.integer(grepl("Yes", lines[lines.map[["lin.corr"]]], fixed = TRUE)),
      correct.stray.light =
        as.integer(grepl("Yes", lines[lines.map[["stray.corr"]]], fixed = TRUE)),
      boxcar.width =
        as.integer(stringr::str_split(lines[lines.map[["boxcar"]]], " ")[[1]][3]),
      integ.time =
        as.numeric(stringr::str_split(lines[lines.map[["integ.time"]]], " ")[[1]][4]),
                      # micro-seconds -> seconds
      num.scans =
        as.integer(stringr::str_split(lines[lines.map[["scans"]]], " ")[[1]][3])
      )
  # processing flag
  inst.settings[["linearized"]] <- as.logical(inst.settings[["corr.sensor.nl"]])
  # diagnosis
  inst.settings[["tot.time"]] = with(inst.settings, integ.time * num.scans)
  inst.settings[["rel.signal"]] = NA

  photobiology::setInstrSettings(x, inst.settings)
  x
}

#' Set the instrument description.
#'
#' Model and serial number are retrieved from the file header, the remaining
#' unknown parameter values are set to NA. These values are used to set
#' the "inst.desc" atrribute of \code{x}.
#'
#' @param x generic_spct, although with defaults only raw_spct.
#' @param file.header character string The header of the file output by
#'   SpectraSuite.
#' @param inst.descriptor list An already built instrument descriptor,
#'   in which case \code{file.header} is used only to validate the serial
#'   number.
#' @param overwrite logical A flag indicating if an existing instrument
#'   descriptor in \code{x} should be overwritten.
#'
#' @return A copy of \code{x} with updated attributes.
#'
#' @export
#'
set_oo_ssdata_descriptor <- function(x,
                                     file.header = comment(x),
                                     inst.descriptor = NULL,
                                     overwrite = FALSE) {
  stopifnot(overwrite || is.na(getInstrDesc(x)))
  stopifnot(length(file.header) == 1)
  lines <- stringr::str_split(file.header, "\n")[[1]]
  idx <- which(stringr::str_detect(lines, "Spectrometers: "))[1]
  spectrometer.sn <- sub("Spectrometers: ", "", lines[idx], fixed = TRUE)

  if (!is.null(inst.descriptor)) {
    # user supplied descriptor
    stopifnot(spectrometer.sn == inst.descriptor[["spectrometer.sn"]])
  } else {
    # decode as much data as possible from header
    inst.descriptor <-
      list(
        time = photobiology::getWhenMeasured(x),
        w = NA,
        sr.index = NA_integer_,
        ch.index = NA_integer_,
        spectrometer.name = NA,
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
  photobiology::setInstrDesc(x, inst.descriptor)
  x
}

oo.minimum.gr <-
  dplyr::data_frame_(list(feature = ~c("spectrometer.name", "spectrometer.sn"),
                     pattern = ~c("Spectrometers: ", "Spectrometers: ")))

#' Set the instrument description.
#'
#' Locate in which lines of the file header different metadata features are
#'   located.
#'
#' @param lines character The data file read line by line.
#' @param header.end integer The index to the last line of the longest
#'   header expected.
#' @param grammar data.frame With character variables "feature" and "pattern"
#'   that will be used to locate the lines containing each type of metadata.
#'
#' @return A data frame with two variables, "feature" of class character, and
#'   "line.idx" of class integer.
#'
#' @export
#'
map_oofile_header_rows <- function(lines,
                                   header.end = NULL,
                                   grammar = oo.minimum.gr) {
  if (is.null(header.end)) {
    header.end <- which(stringr::str_detect(lines, "^>>>>>Begin ")) - 1L
  }
  line.idxs <- numeric(nrow(grammar))
  for (i in seq_along(grammar$feature)) {
    idx <- which(stringr::str_detect(lines[1:header.end], grammar$pattern[i]))
    stopifnot(length(idx) <= 1)
    line.idxs[i] <- which(stringr::str_detect(lines[1:header.end], grammar$pattern[i]))[1]
  }
  names(line.idxs) <- grammar$feature
  line.idxs
}

#' Find range of lines in file containing spectral data.
#'
#' Ocean Optics files can have varying numbers of rows containing spectral
#'   data, but these rows are fenced between two lines containing recognizable
#'   character strings. This function, detects these strings.
#'
#' @param lines character The data file read line by line.
#'
#' @return integer vector of length two, containing the indexes to the first
#'   last rows containing spectral data.
#'
#' @note This is a rather slow operation, so if all files to be read have
#'   the same format, it is inefficient to call this function for each file.
#'
#' @export
#'
oofile_data_rows <- function(lines) {
  skip <- which(stringr::str_detect(lines, "^>>>>>Begin "))
  stopifnot(length(skip) == 1L)
  npixels.row <- which(stringr::str_detect(lines[1:20], "Number of Pixels in Processed Spectrum: "))
  if (length(npixels.row)) {
    npixels <- as.integer(sub("Number of Pixels in Processed Spectrum: ", "",
                              lines[npixels.row], fixed = TRUE))
  } else {
    end.line <- which(stringr::str_detect(lines, "^>>>>>End "))
    stopifnot(length(end.line) == 1L)
    npixels <- end.line - skip - 1
  }
  list(skip = skip, npixels = npixels)
}
