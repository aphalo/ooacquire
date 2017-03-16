#' Set the values of instrument settings from file header
#'
#' Parse the header of the file returned by SpectraSuite for corrections,
#' smotthing and acquisition parameters used. These values are used to set
#' the "inst.settings" atrribute of \code{x}.
#'
#' @param x generic_spct, although with defaults only raw_spct.
#' @param file.header character string The header of the file output by
#'   SpectraSuite.
#' @param overwrite logical A flag indicating if an existing valid instrument
#'   descriptor in \code{x} should be overwritten.
#'
#' @return A copy of \code{x} with updated attributes.
#' @export
#'
set_oo_ssdata_settings <- function(x,
                                   file.header = comment(x),
                                   overwrite = TRUE) {

  parse_integ_time <- function(line, pos) {
    time.str <- stringr::str_split(line, " ")[[1]][pos]
    if (grepl("(usec)", line, fixed = TRUE)) {
      as.numeric(time.str)
    } else if (grepl("(sec)", line, fixed = TRUE)) {
      time.str <- gsub(",", ".", time.str)
      as.numeric(time.str) * 1e6
    }
  }

  if (!overwrite && isValidInstrSettings(x)) {
    return(x)
  }

  stopifnot(length(file.header) == 1)


  lines <- stringr::str_split(file.header, "\n")[[1]]

  if (grepl("SpectraSuite", lines[1])) {
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
    position  <- c(-1, -1, -1, 3, 4, 3, -1)
    names(position) <- my.gr[["feature"]]
  } else { # we assume OceanView file
    my.gr <- dplyr::data_frame_(list(
      feature = ~c("dark.corr", "lin.corr", "stray.corr", "boxcar",
                   "integ.time", "scans", "sn"),
      pattern = ~c("Electric dark correction enabled:",
                   "Nonlinearity correction enabled:",
                   NA_character_,
                   "Boxcar width:",
                   "Integration Time",
                   "Scans to average:",
                   "Spectrometer:")
    ))
    position  <- c(-1, -1, -1, 3, 4, 4, -1)
    names(position) <- my.gr[["feature"]]
  }

  lines.map <- map_oofile_header_rows(lines,
                                      header.end = 20,
                                      grammar = my.gr)

  inst.settings <-
    list(
      # user settings
      pix.selector = TRUE,
      # instrument settings
      correct.elec.dark =
        as.integer(grepl("Yes|true", lines[lines.map[["dark.corr"]]], fixed = FALSE)),
      corr.sensor.nl =
        as.integer(grepl("Yes|true", lines[lines.map[["lin.corr"]]], fixed = FALSE)),
      correct.stray.light =
        as.integer(!is.na(lines.map[["stray.corr"]]) &&
                     (grepl("Yes", lines[lines.map[["stray.corr"]]], fixed = TRUE))),
      boxcar.width =
        as.integer(stringr::str_split(lines[lines.map[["boxcar"]]], " ")[[1]][position["boxcar"]]),
      integ.time = parse_integ_time(lines[lines.map[["integ.time"]]],
                                    position["integ.time"]), # micro seconds
      num.scans =
        as.integer(stringr::str_split(lines[lines.map[["scans"]]], " ")[[1]][position["scans"]])
    )
  # processing flag
  inst.settings[["linearized"]] <- as.logical(inst.settings[["corr.sensor.nl"]])
  # diagnosis
  inst.settings[["tot.time"]] <- inst.settings[["integ.time"]] * inst.settings[["num.scans"]]
  inst.settings[["rel.signal"]] <- NA

  photobiology::setInstrSettings(x, inst.settings)
  if (!isValidInstrSettings(x)) {
    warning("Setting of attribute \"instr.settings\" from fileheader has failed.")
  }
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
#' @param descriptor list An already built instrument descriptor, to be
#'   used as basis when action == "merge" or action == "keep". Defaults,
#'   to that stored in \code{x}.
#' @param action character A flag indicating if an existing instrument
#'   descriptor in \code{x} should be overwritten, merged or kept as is
#'   if present.
#'
#' @return A copy of \code{x} with updated attributes.
#'
#' @export
#'
set_oo_ssdata_descriptor <- function(x,
                                     file.header = comment(x),
                                     descriptor = photobiology::getInstrDesc(x),
                                     action = "merge") {

  stopifnot(action %in% c("keep", "merge", "overwrite"))
  stopifnot(length(file.header) == 1)
  stopifnot(action == "overwrite" || length(descriptor) >= 4)

  if (action %in% c("keep", "merge")) {
    if (is.na(descriptor[["spectrometer.sn"]])) {
 #     warning("'action == ", action, "' but 'x' contains no descriptor: set 'action = \"overwrite\"")
      action <- "overwrite"
    }
  }

  if (action != "keep") {
    lines <- stringr::str_split(file.header, "\n")[[1]]
    idx <- which(stringr::str_detect(lines, "Spectrometers: |Spectrometer: "))[1]
    spectrometer.sn <- sub("Spectrometers: |Spectrometer: ", "", lines[idx], fixed = FALSE)
    if (grepl("MAYP", spectrometer.sn)) {
      spectrometer.name <- "MayaPro2000"
    } else {
      spectrometer.name <- NA_character_
    }

    # decode as much data as possible from header
    file.descriptor <-
      list(
        time = photobiology::getWhenMeasured(x),
        w = NA,
        sr.index = NA_integer_,
        ch.index = NA_integer_,
        spectrometer.name = spectrometer.name,
        spectrometer.sn =  spectrometer.sn
      )

    if (action == "merge") {
      if (file.descriptor[["spectrometer.sn"]] != descriptor[["spectrometer.sn"]]) {
        warning("Merging descriptors from different instruments, using s.n. from file.")
      }
      for (i in names(file.descriptor)) {
        if (length(file.descriptor[[i]]) != 0) {
          descriptor[[i]] <- file.descriptor[[i]]
        }
      }
    } else { # actio == "overwrite"
      descriptor <- file.descriptor
    }
  }

  photobiology::setInstrDesc(x, descriptor)
  if (!isValidInstrDesc(x)) {
    warning("Setting of attribute \"instr.desc\" from fileheader has failed.")
  }
  x
}

oo.minimum.gr <-
  dplyr::data_frame_(list(feature = ~c("spectrometer.name", "spectrometer.sn"),
                     pattern = ~c("Spectrometers: |Spectrometer: ",
                                  "Spectrometers: |Spectrometer: ")))

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
    header.end <- which(stringr::str_detect(lines,
                                            "^>>>>>Begin |end of file header")) - 1L
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
  npixels.row <- which(stringr::str_detect(lines[1:20], "Number of Pixels in "))
  if (length(npixels.row)) {
    pattern <- c("^Number of Pixels in Processed Spectrum: ",
                 "^Number of Pixels in Spectrum: ")
    pattern <- paste(pattern, collapse = "|")
    npixels <- as.integer(sub(pattern, "",
                              lines[npixels.row], fixed = FALSE))
  } else {
    end.line <- which(stringr::str_detect(lines, "^>>>>>End "))
    stopifnot(length(end.line) == 1L)
    npixels <- end.line - skip - 1
  }
  list(skip = skip, npixels = npixels)
}
