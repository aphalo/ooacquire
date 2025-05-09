#' Convert raw counts data into spectral irradiance or fluence
#'
#' @param x A named list of one to three vectors of file names, with names
#'   "light", "filter", and "dark". Or a raw_mspt object, or a raw_spct object.
#' @param spct.names named character vector of length three, to map names in
#'   \code{x} to those expected.
#' @param correction.method A named list of constants and functions defining the
#'   method to be used for stray light and dark signal corrections.
#' @param hdr.tolerance numeric Tolerance for mean deviation among cps columns as
#'   a fraction of one. Used in check of HDR consistency. A negative value
#'   disables merging using only the data for the shortest integration time.
#' @param return.cps logical Useful when there is no need to apply a calibration,
#'   such as when computing new calibration multipliers.
#' @param trim.descriptor logical If \code{TRUE} the spectrometer calibration
#'   constants, pixel wavelengths, slit-function "tail-correction"
#'   function code and other calibration-related information is deleted.
#' @param descriptor A named list with a descriptor of the characteristics of
#'   the spectrometer (if serial number does not agree an error is triggered).
#' @param locale The locale controls defaults that vary from place to place. The
#'   default locale is US-centric (like R), but you can use
#'   \code{\link[readr]{locale}} to create your own locale that controls things
#'   like the default time zone, encoding, decimal mark, big mark, and day/month
#'   names.
#' @param verbose Logical indicating the level of warnings and messages wanted.
#' @param ... Named arguments passed to \code{photobiology::cps2irrad} which is
#'   the final calculation step.
#'
#' @note Currently \code{s_irrad_corrected.list} allows processing of files
#'   written by OceanOptics' SpectraSuite software, from protocols with
#'   integration-time bracketing or not, with a dark reference measurement or
#'   not. Three measurements components are recognized: a "light" measurement, a
#'   "filter" measurement using a polycarbonate filter and a dark measurement.
#'   Only "light" is mandatory. Data should be raw counts, either corrected for
#'   detector non-linearity or not. All three spectra should be acquired using
#'   the same instrument settings to achieve good accuracy.
#'
#'   Enabling \code{trim.descriptor} ensures that the data objects are
#'   free of references to code in 'ooacquire', which is crucial for the
#'   portability of the spectral data.
#'
#' @family functions for conversion of raw-counts data
#'
#' @export
#'
s_irrad_corrected <- function(x, ...) UseMethod("s_irrad_corrected")

#' @describeIn s_irrad_corrected Default for generic function.
#' @export
s_irrad_corrected.default <- function(x, ...) {
  stop("'s_irrad_corrected' not implemented for class '", class(x))
}

#' @describeIn s_irrad_corrected Default for generic function.
#' @param time a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA}, date is set to NA.
#' @export
s_irrad_corrected.list <-
  function(x,
           time = NULL,
           correction.method,
           hdr.tolerance = getOption("ooacquire.hdr.tolerance", default = 0.05),
           return.cps = FALSE,
           trim.descriptor = !return.cps,
           descriptor,
           locale = NULL,
           verbose = getOption("photobiology.verbose", default = FALSE),
           ...) {
    comment.txt <- paste(names(x),
                         sapply(x, paste, collapse = ", "),
                         sep = ": ", collapse = "\n")

    raw.mspct <-
      ooacquire::read_files2mspct(x,
                                  time = time,
                                  locale = locale,
                                  descriptor = descriptor,
                                  verbose = verbose)

    corrected.spct <-
      s_irrad_corrected(x = raw.mspct,
                        spct.names = c(light = "light",
                                       filter = "filter",
                                       dark = "dark"),
                        correction.method = correction.method,
                        hdr.tolerance = hdr.tolerance,
                        return.cps = return.cps,
                        trim.descriptor = trim.descriptor,
                        verbose = verbose,
                        ...)

    photobiology::setWhatMeasured(corrected.spct, comment.txt)
    comment(corrected.spct) <-
      paste("Processed on ", lubridate::today(tzone = "UTC"),
            "\nwith 's_irrad_corrected()' from 'ooacquire' ver. ",
            utils::packageVersion("ooacquire"),
            "\n\nfrom files:\n", comment.txt, sep = "")

    corrected.spct
  }

#' @describeIn s_irrad_corrected Default for generic function.
#' @export
s_irrad_corrected.raw_mspct <-
  function(x,
           spct.names = c(light = "light",
                          filter = "filter",
                          dark = "dark"),
           correction.method,
           hdr.tolerance = getOption("ooacquire.hdr.tolerance", default = 0.05),
           return.cps = FALSE,
           trim.descriptor = !return.cps,
           verbose = getOption("photobiology.verbose", default = FALSE),
           ...) {

    # remove unused name mappings (dependent on protocol)
    if ("dark" %in% names(spct.names) && !spct.names[["dark"]] %in% names(x)) {
      spct.names <- spct.names[names(spct.names) != "dark"]
    }
    if ("filter" %in% names(spct.names) && !spct.names[["filter"]] %in% names(x)) {
      spct.names <- spct.names[names(spct.names) != "filter"]
    }

    # experimental, issues warning in case of failure and tags spectrum
    QC_spct <- TRUE
    if ("dark" %in% names(spct.names)) {
      QC_spct <-
        QC_dark(x[[spct.names[["dark"]]]],
                spct.label = "Dark spectrum", verbose = verbose)
    }
    if ("filter" %in% names(spct.names)) {
      QC_spct <-
        QC_spct &&
        QC_dark(x[[spct.names[["filter"]]]], range = c(NA, 390),
                spct.label = "Filter spectrum", verbose = verbose)
    }

    if (is.list(spct.names) && (
      length(spct.names[["light"]]) > 1L || spct.names[["light"]][1] == "*")) {
      # "*" is a placeholder for all other spectra
      if (spct.names[["light"]][1] == "*") {
        spct.names <- as.list(spct.names)
        all.spct.names <- names(x)
        spct.names[["light"]] <-
          setdiff(all.spct.names, c(spct.names[["dark"]], spct.names[["filter"]]))
      }
      # if we have a series we use recursion for each spectrum
      corrected.mspct <- list() # a list is enough
      new.names <- gsub("^light", "time", spct.names[["light"]])
      job.length <- length(spct.names[["light"]])
      show.progress <- verbose || (interactive() && job.length > 30)
      if (show.progress) {
        step.length <-
          if (job.length > 1000) 20
        else if (job.length > 100) 10
        else 4
        progress.step <- job.length %/% step.length
        progress <- 0
        cat("s_irrad_corrected(): 0%, ")
      } else if (interactive()) {
        cat("s_irrad_corrected(): ")
      }
      for (i in seq_along(spct.names[["light"]])) {
        if (show.progress) {
          progress <- progress + 1
          if ((progress %% progress.step) == 0) {
            cat(progress / job.length * 100, "%, ", sep = "")
          }
        }
        temp.spct.names <- spct.names
        temp.spct.names[["light"]] <- spct.names[["light"]][i]
        temp.spct.names <- unlist(temp.spct.names, use.names = TRUE) # convert list into vector
        corrected.mspct[[new.names[i]]] <-
          s_irrad_corrected(x[unname(temp.spct.names)], # extraction needed because of tests
                            spct.names = temp.spct.names,
                            correction.method = correction.method,
                            hdr.tolerance = hdr.tolerance,
                            return.cps = return.cps,
                            trim.descriptor = trim.descriptor,
                            verbose = verbose,
                            ...)
      }
      # convert collection into spectrum in long form
      corrected.spct <-
        photobiology::rbindspct(corrected.mspct, attrs.simplify = TRUE)
      if (interactive()) {
        cat("ready!\n")
      }
    } else {

      check_spct_prev_state <-
        photobiology::disable_check_spct() # avoid spurious warnings
      on.exit(photobiology::set_check_spct(check_spct_prev_state), add = TRUE)

      check_sn_match(x, correction.method, missmatch.action = stop)

      if (length(setdiff(names(x), spct.names)) > 0L) {
        stop("Bad member names in 'spct.names': ", names(spct.names))
      }

      if (length(x[[ spct.names[["light"]] ]]) == 0) {
        if (verbose) {
          warning("'raw_spct' object for 'light' scans missing")
        }
        if (return.cps) {
          return(photobiology::cps_spct())
        } else {
          return(photobiology::source_spct())
        }
      }

      if (is.null(correction.method[["worker.fun"]])) {
        worker.fun <- NULL
      } else if (is.na(correction.method[["worker.fun"]])) {
        worker.fun <- NULL
      } else if (is.character(correction.method[["worker.fun"]])) {
        worker.fun <- get(correction.method[["worker.fun"]],
                          mode = "function",
                          envir = as.environment("package:ooacquire"))
      } else {
        worker.fun <- correction.method[["worker.fun"]]
      }

      stopifnot(is.null(worker.fun) || is.function(worker.fun))

      corrected.spct <-
        uvb_corrections(x = x,
                        spct.names = spct.names,
                        stray.light.method = correction.method[["stray.light.method"]],
                        stray.light.wl = correction.method[["stray.light.wl"]],
                        flt.dark.wl = correction.method[["flt.dark.wl"]],
                        flt.ref.wl = correction.method[["flt.ref.wl"]],
                        flt.Tfr = correction.method[["flt.Tfr"]],
                        inst.dark.pixs = correction.method[["inst.dark.pixs"]],
                        worker.fun = worker.fun,
                        trim = correction.method[["trim"]],
                        hdr.tolerance = hdr.tolerance,
                        verbose = verbose)

      if (return.cps) {
        corrected.spct <-
          photobiology::check_spct(corrected.spct)
      } else {
        descriptor <- photobiology::getInstrDesc(corrected.spct)
        if (any(is.na(descriptor$inst.calib$irrad.mult)) ||
            length(descriptor$inst.calib$irrad.mult) !=
            length(descriptor$wavelengths)) {
          stop("The 'instrument descriptor' lacks valid irradiance calibration data!")
        }
        corrected.spct <-
          photobiology::check_spct(photobiology::cps2irrad(corrected.spct, ...))
      }
    }

    if (trim.descriptor) {
      # delete calibration data and remove dependency on 'ooacquire'
      photobiology::trimInstrDesc(corrected.spct)
    }

    attributes(corrected.spct) <- c(attributes(corrected.spct),
                                    list(QC_dark_pass = QC_spct))
    corrected.spct
  }

#' @describeIn s_irrad_corrected Default for generic function.
#' @export
s_irrad_corrected.raw_spct <- function(x,
                                       time = NULL,
                                       correction.method,
                                       hdr.tolerance = getOption("ooacquire.hdr.tolerance", default = 0.05),
                                       return.cps = FALSE,
                                       trim.descriptor = !return.cps,
                                       verbose = getOption("photobiology.verbose", default = FALSE),
                                       ...) {
  raw.mspct <- photobiology::raw_mspct(list(light = x))
  s_irrad_corrected(x = raw.mspct,
                    time = time,
                    correction.method = correction.method,
                    hdr.tolerance = hdr.tolerance,
                    return.cps = return.cps,
                    trim.descriptor = trim.descriptor,
                    verbose = verbose,
                    ...)
}

#' Select which instrument descriptor to use
#'
#' Select from a list of instrument descriptors which one to use based on
#' date of measurement.
#'
#' @param date Any object that \code{anytime::anydate()} will decode as a date
#'   or convert to a date. Used to select a descriptor containing calibration
#'   data valid for a given day.
#' @param descriptors A named list of descriptors of the characteristics of
#'   the spectrometer including calibration data.
#' @param verbose Logical indicating the level of warnings wanted.
#' @param strict.calib Logical indicating the level of validity checks.
#' @param entrance.optics character The name or geometry of the diffuser or
#'   entrance optics to select. Only required when there are calibration with
#'   multiple entrance optics for the same spectrometer.
#' @param ... Currently ignored.
#'
#' @details Calibrations for instruments stored in a list and passed as argument
#'   to \code{descriptors}, also store the dates between which they are valid.
#'   This function walks the list searching for a calibration valid for
#'   \code{date}. If no valid calibration is found and \code{strict.calib =
#'   FALSE}, the calibration valid closest in time is returned with a warning
#'   while if no valid calibration is found and \code{strict.calib = TRUE} an
#'   error is triggered.
#'
#'   If a character string is passed as argument to \code{date}, it must be
#'   in a format suitable for \code{anytime::anydate()}. One needs to be
#'   careful with months and days of the month when supplying them as numbers,
#'   so using months names or their abbreviations can be safer.
#'
#' @note The default argument for \code{verbose} is for this function
#'   \code{TRUE} as conversion of other objects to a date may fail.
#'
#' @export
#'
#' @examples
#' d1 <- which_descriptor()
#' d1$spectrometer.sn
#' d1$entrance.optics$geometry
#'
#' d2 <- which_descriptor(entrance.optics = "hemispherical")
#' d2$spectrometer.sn
#' d2$entrance.optics$geometry
#'
#' d3 <- which_descriptor(descriptors = ooacquire::MAYP112785_descriptors)
#' d3$spectrometer.sn
#' d3$entrance.optics$geometry
#'
#' # gives error, as expected
#' # which_descriptor(descriptors = ooacquire::MAYP112785_descriptors,
#' #                  entrance.optics = "hemispherical")
#'
#' # gives error
#' # which_descriptor(date = lubridate::now() + lubridate::years(1))
#'

which_descriptor <-
  function(date = lubridate::now(tzone = "UTC"),
           descriptors = ooacquire::MAYP11278_descriptors,
           verbose = getOption("photobiology.verbose", TRUE),
           strict.calib = getOption("photobiology.strict.calib", FALSE),
           entrance.optics = NULL,
           ...) {

    # old objects can have no descriptor for entrance optics. However,
    # even if we do not know s/n, etc., we know its geometry was "cosine"
    # In 'ooacquire' (>= 0.5.3) all built-in calibrations have the field
    # entrance.optics set.
    if (is.null(entrance.optics) ||
        entrance.optics %in% c("unknown", "default")) {
      entrance.optics <- "cosine"
    }

    if (!lubridate::is.instant(date)) {
      date <- anytime::utctime(date)
    }

    if (!is.POSIXct(date)) {
      date <- as.POSIXct(date)
      if (!is.POSIXct(date)) {
        stop("'date' could not be converted into 'POSIXct' class")
      }
    }

    if (date > lubridate::now(tzone = "UTC") + lubridate::days(1)) {
      warning("Looking up calibration for a date in the future!!", call. = FALSE)
    }

    descriptor <- list()
    # descriptors are stored from oldest to most recent calibration
    # we search backwards, from most recent to oldest
    for (d in rev(names(descriptors))) {
      if (date >= descriptors[[d]][["inst.calib"]][["start.date"]] &
          date <= descriptors[[d]][["inst.calib"]][["end.date"]] &
          (entrance.optics == descriptors[[d]][["entrance.optics"]][["model"]] ||
           entrance.optics == descriptors[[d]][["entrance.optics"]][["geometry"]])) {
        descriptor <- descriptors[[d]]
        if (verbose) {
          message("Descriptor \"", d, "\" selected for ", round(date, units = "days"),
                  " and entrance.optics \"", entrance.optics, "\"")
        }
        break()
      }
    }

    if (!length(descriptor)) {
      past.end <- date > descriptors[[length(descriptors)]][["inst.calib"]][["end.date"]]
      before.start <- date < descriptors[[1]][["inst.calib"]][["start.date"]]
      in.gap <- !xor(before.start, past.end)
      if (strict.calib && any(c(past.end, before.start, in.gap))) {
        stop("No valid calibration available for ", date,
             " and entrance.optics \"", entrance.optics, "\"")
      } else {
        if (past.end) {
          # we search backwards, from most recent to oldest
          # search is needed to match entrance optics type
          for (d in rev(names(descriptors))) {
            if (date > descriptors[[d]][["inst.calib"]][["start.date"]] &
                (entrance.optics == descriptors[[d]][["entrance.optics"]][["model"]] ||
                 entrance.optics == descriptors[[d]][["entrance.optics"]][["geometry"]])) {
              descriptor <- descriptors[[d]]
              if (verbose) {
                message("Descriptor \"", d, "\" selected for ", round(date, units = "days"),
                        " and entrance.optics \"", entrance.optics, "\"")
              }
              break()
            }
          }
          days.past <- format(round(date - descriptor[["inst.calib"]][["end.date"]]))
          message.text <- paste("Using a calibration ",
                                days.past,
                                " past its validity",
                                sep = "")
        } else if (before.start) {
          # we search forwards, from oldest to most recent
          # search is needed to match entrance optics type
          for (d in names(descriptors)) {
            if (date < descriptors[[d]][["inst.calib"]][["start.date"]] &
                (entrance.optics == descriptors[[d]][["entrance.optics"]][["model"]] ||
                 entrance.optics == descriptors[[d]][["entrance.optics"]][["geometry"]])) {
              descriptor <- descriptors[[d]]
              if (verbose) {
                message("Descriptor \"", d, "\" selected for ", round(date, units = "days"),
                        " and entrance.optics \"", entrance.optics, "\"")
              }
              break()
            }
          }
          days.before <- format(round(date - descriptor[["inst.calib"]][["start.date"]]))
          message.text <- paste("Using a calibration ",
                                days.before,
                                " before its validity",
                                sep = "")
        } else if (in.gap) {
          descriptor <- list()
          # we search backwards, from most recent to oldest
          # search is needed to match entrance optics type
          for (d in rev(names(descriptors))) {
            if (date > descriptors[[d]][["inst.calib"]][["start.date"]] &
                descriptors[[d]][["inst.calib"]][["end.date"]] +
                lubridate::years(2) > date &
                (entrance.optics == descriptors[[d]][["entrance.optics"]][["model"]] ||
                 entrance.optics == descriptors[[d]][["entrance.optics"]][["geometry"]])) {
              descriptor <- descriptors[[d]]
              if (verbose) {
                message("Descriptor \"", d, "\" selected for ", round(date, units = "days"),
                        " and entrance.optics \"", entrance.optics, "\"")
              }
              break()
            }
          }
          if (!length(descriptor)) {
            stop("No calibration data found for entrance optics \"", entrance.optics, "\"")
          } else {
            days.past <- format(round(date - descriptor[["inst.calib"]][["end.date"]]))
            message.text <- paste("Using a calibration ",
                                  days.past,
                                  " past its validity, in a time gap",
                                  sep = "")
          }
        }
        warning(message.text, call. = FALSE)
      }
    }

    descriptor
  }
