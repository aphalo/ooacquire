#' Acquire spectra interactively
#'
#' Functions providing a simple interactive front-end to the functions in the
#' package, also working as example code that can be modified for different
#' uses.
#'
#' @details These functions can be useful for commonly done measurements but
#'   they also play the role of examples that users can modify according to
#'   their needs. They are all composed in a modular way from functions that can
#'   be reshuffled and combined with other functions to define new variations
#'   better suited to users' needs and tastes. Functions
#'   \code{acq_irrad_interactive()} and \code{acq_fraction_interactive} provide
#'   support the measurement of spectral irradiance of continuous light sources,
#'   and transmittance, reflectance and absorbance using continuous light
#'   sources, respectively.
#'
#'   The default behaviour of the functions can be changed by passing different
#'   arguments through parameters, but for special use cases it could be best
#'   for users to define case-specific data acquisition functions from the same
#'   building blocks.
#'
#'   Using these functions only requires an Ocean Optics spectrometer to be
#'   connected. The connection to the spectrometer and selection of channel,
#'   when relevant, is done from within these functions.
#'
#'   The irradiance calibration will be retrieved from the spectrometer memory
#'   as a last resource if not supplied in any other way. Given that the factors
#'   are stored in a format that ignores the entrance optics, either the
#'   efefctive diffuser are in xxx should be passed to parameter \code{area} or
#'   a character string with the type of the diffuser passed to
#'   \code{diff.type}. If no irardiance calibration is available, counts per
#'   second (cps) or raw counts are the only options available.
#'
#' @seealso This function calls functions \code{\link{tune_interactive}},
#'   \code{\link{protocol_interactive}} and
#'   \code{\link{set_attributes_interactive}}. If irradiance calibration is
#'   retrieved from the instrument, functions \code{\link{get_oo_descriptor}}
#'   and \code{\link{oo_calib2irrad_mult}} are also called.
#'
#' @family interactive acquisition functions
#'
#' @param tot.time.range numeric vector Range of total times for a measurement
#'   in seconds.
#' @param target.margin numeric (0..1) when tuning integration time, how big a
#'   head space to leave.
#' @param HDR.mult numeric the integration time for each bracketed integration
#'   as a multiplier of the set or tuned integration time.
#' @param protocols named list of character vectors, or a character vector with
#'   names of at least one member of the default list of protocols.
#' @param correction.method list The method to use when applying the calibration
#' @param descriptors list A list of instrument descriptors containing
#'   calibration data.
#' @param stray.light.method character Used only when the correction method is
#'   created on-the-fly.
#' @param seq.settings named list with numeric members \code{"step.delay"} and
#'   \code{"num.steps"}.
#' @param area numeric Passed to \code{o_calib2irrad_mult()}.
#' @param diff.type character Passed to \code{o_calib2irrad_mult()}.
#' @param qty.out character One of "Tfr" (spectral transmittance as a fraction
#'   of one), "irrad" (spectral irardiance), "cps" (counts per second), or "raw"
#'   (raw sensor counts).
#' @param save.pdfs,save.summaries,save.collections logical Whether to save
#'   plots to PDFs files or not, and collection summaries to csv files or not,
#'   enable collections user interface or not..
#' @param interface.mode character One of "auto", "simple", "manual", "series",
#'   "auto-attr", "simple-attr", "manual-attr", and "series-attr".
#' @param folder.name,session.name,user.name character Default name of the
#'   folder used for output, and session and user names.
#'
#' @export
#'
#' @note The integration time is set automatically so that the peak number of
#'   counts is close to 1 - \code{target.margin} times the maximum of the range
#'   of the instrument detector. The minimum \code{tot.time} is achieved by
#'   increasing the number of scans. The default protocols are usually suitable,
#'   if new protocols are passed, each character vector must contain strings
#'   "light", "filter" and "dark", or "sample", "reference", and "dark",
#'   depending on the function. Plots are produced with functions from package
#'   'ggspectra' and respect the default annotations set with function
#'   \code{set_annotations_default()}, default wavebands set with function
#'   \code{set_w.band_default()}, and irradiance quantities set with
#'   \code{photon_as_default()}, and \code{energy_as_default()}.
#'
#' @note The different interface modes are suitable for different types of
#'   measurements. The field separator in the written CSV file is according to
#'   the current locale. If needed use \code{readr::set_locale()} to change it
#'   before calling this function.
#'
#' @note Calibration data needs in most cases to be imported into R and
#'   parameters entered for the special correction algorithms into a correction
#'   method descriptor. The corrections are skipped if the needed information is
#'   missing. If no spectral irradiance calibration is available and attempt is
#'   made to retrieve it from the spectrometer, but given the format used by
#'   Ocean Optics/Ocean Insight, in this case the effective \code{area} of the
#'   cosine diffuser used (or the model name if from Ocean Optics) should be
#'   supplied by the user.
#'
#' @return These functions return the acquired spectra through "side effects" as
#'   each spectrum is saved, both as raw counts data and optionally as spectral
#'   irradiance or counts-per-second  spectral data in an \code{.rda} file as
#'   objects of the classes defined in package 'photobiology'. Optionally, the
#'   plot for each spectrum is saved as a \code{.pdf} file, as well as summaries
#'   to a CSV file and joint plots to a \code{.pdf} file for each collection
#'   created. The value returned by the function is that from closing the
#'   connection to the spectrometer.
#'
#' @examples
#' # please, see also the example scripts installed with the package
#'
#' \dontrun{
#' # requires an Ocean Insight (former Ocean Optics) spectrometer to be
#' # connected via USB
#'
#' acq_irrad_interactive()
#' acq_irrad_interactive(qty.out = "cps")
#'
#' }
#'
acq_irrad_interactive <-
  function(tot.time.range = c(5, 15),
           target.margin = 0.1,
           HDR.mult = c(short = 1, long = 10),
           protocols = NULL,
           correction.method = NA,
           descriptors = NA,
           stray.light.method = "none",
           seq.settings = NULL,
           area = NULL,
           diff.type = NULL,
           qty.out = "irrad",
           save.pdfs = TRUE,
           save.summaries = TRUE,
           save.collections = TRUE,
           interface.mode = "auto",
           folder.name = paste("acq", qty.out,
                               lubridate::today(),
                               sep = "-"),
           user.name = Sys.info()[["user"]],
           session.name = paste(user.name,
                                strftime(lubridate::now(),
                                         "%Y.%b.%d_%H.%M.%S"),
                                sep = "_")) {

    old.value <- options(warn = 1)
    on.exit(options(old.value), add = TRUE, after = TRUE)

    if (getOption("ooacquire.offline", FALSE)) {
      warning("ooacquire off-line: Aborting...")
      return()
    }

    # validate interface mode
    interface.mode <- tolower(interface.mode)
    if (!gsub("-attr$", "", interface.mode) %in%
        c("auto", "simple", "series")) {
      stop("Invalid argument for 'interface.mode', aborting.", call. = FALSE)
    }

    # validate qty.out
    qty.out <- tolower(qty.out)
    stopifnot(qty.out %in% c("irrad", "cps", "raw"))

    # define measurement protocols
    default.protocols <- list(l = "light",
                              ld = c("light", "dark"),
                              lf = c("light", "filter"),
                              lfd = c("light", "filter", "dark"))
    if (length(protocols) == 0) {
      protocols <- default.protocols
    } else if (inherits(protocols, "character")) {
      protocols <- default.protocols[protocols]
      if (length(protocols) == 0) {
        stop("No protocol selected.", call. = FALSE)
      }
    }

    # connect to spectrometer
    w <- start_session()
    on.exit(end_session(w))

    instruments <- list_srs_interactive(w = w)
    sr.index <- choose_sr_interactive(instruments = instruments)
    if (sr.index < 0L) {
      print("Aborting...")
      end_session(w = w)
      message("Bye!")
      return(NULL)
    }
    ch.index <- choose_ch_interactive(instruments = instruments,
                                      sr.index = sr.index)

    serial_no <- as.character(instruments[sr.index + 1L, 3])

    message("Using channel ", ch.index,
            " from spectrometer with serial number: ", serial_no)

    if (anyNA(c(descriptors[[1]], correction.method[[1]]))) {
      descriptor <-
        switch(serial_no,
               MAYP11278 = which_descriptor(descriptors = ooacquire::MAYP11278_descriptors),
               MAYP112785 = which_descriptor(descriptors = ooacquire::MAYP112785_descriptors),
               MAYP114590 = which_descriptor(descriptors = ooacquire::MAYP114590_descriptors),
               FLMS04133 = which_descriptor(descriptors = ooacquire::FLMS04133_descriptors),
               FLMS00673 = which_descriptor(descriptors = ooacquire::FLMS00673_descriptors),
               FLMS00440 = which_descriptor(descriptors = ooacquire::FLMS00440_descriptors),
               FLMS00416 = which_descriptor(descriptors = ooacquire::FLMS00416_descriptors),
               {
                 warning("No instrument descriptor found, retrieving from the spectrometer",
                         call. = FALSE)
                 get_oo_descriptor(w, sr.index = sr.index, ch.index = ch.index)
                 # this can introduce NA irrad.mult, which are checked further below
               }
        )

      correction.method <-
        switch(serial_no,
               MAYP11278 = ooacquire::MAYP11278_ylianttila.mthd,
               MAYP112785 = ooacquire::MAYP112785_ylianttila.mthd,
               MAYP114590 = ooacquire::MAYP114590_simple.mthd,
               FLMS04133 = ooacquire::FLMS04133_none.mthd,
               FLMS00673 = ooacquire::FLMS00673_none.mthd,
               FLMS00440 = ooacquire::FLMS00440_none.mthd,
               FLMS00416 = ooacquire::FLMS00416_none.mthd,
               {
                 warning("No spectrometer-specific method found, using a generic one",
                         call. = FALSE)
                 new_correction_method(descriptor,
                                       stray.light.method = "none")
               }
        )

      default.protocol <-
        switch(serial_no,
               MAYP11278 = "lfd",
               MAYP112785 = "lfd",
               MAYP114590 = "ld",
               FLMS04133 = "ld",
               FLMS00673 = "ld",
               FLMS00440 = "ld",
               FLMS00416 = "ld",
                           "ld"
        )

    } else {
      descriptor <- which_descriptor(descriptors = descriptors)
      stopifnot(exists("spectrometer.name", descriptor))
      default.protocol <- "ld"
    }

    # needed only for descriptors retrieved from data
    descriptor[["w"]] <- w
    descriptor[["sr.index"]] <- sr.index
    descriptor[["ch.index"]] <- ch.index

    if (length(descriptor) < 10 || length(correction.method) < 5) {
      stop("No spectrometer data found")
    }

    # We still check serial numbers, really needed only for user supplied descriptors
    descriptor.inst <- get_oo_descriptor(w)
    stopifnot(descriptor[["spectrometer.sn"]] == descriptor.inst[["spectrometer.sn"]])

    # We check that wavelength calibration is available
    stopifnot(length(descriptor[["wavelengths"]]) == descriptor[["num.pixs"]])
    # We check for valid calibration multipliers
    if (length(descriptor[["inst.calib"]][["irrad.mult"]]) != descriptor[["num.pixs"]] ||
      anyNA(descriptor[["inst.calib"]][["irrad.mult"]])) {
      if (qty.out == "irrad") {
        warning("Bad calibration data, returning counts-per-second.",
                call. = FALSE)
        qty.out = "cps"
      }
    }

    # We get metadata from user, offering defaults
    session.name <- make.names(session.name) # validate argument passed in call
    session.prompt <- paste("Session's name (-/<string>): ", session.name)
    utils::flush.console()
    user.session.name <- readline(session.prompt)
    if (! user.session.name == "") {
      session.name <- make.names(user.session.name)
      if (user.session.name == "") {
        session.name <- make.names(lubridate::now())
      }
      if (session.name != user.session.name) {
        message("Using sanitised/generated name: '", session.name, "'.", sep = "")
      }
    }

    user.name <- make.names(user.name) # validate argument passed in call
    user.name.prompt <- paste("Operator's name (-/<string>): ", user.name)
    utils::flush.console()
    user.user.name <- readline(user.name.prompt)
    if (! user.user.name == "") {
      user.name <- make.names(user.user.name)
    }
    session.label <- paste("Operator: ", user.name,
                           "\nSession: ", session.name,
                           ", instrument s.n.: ", descriptor[["spectrometer.sn"]],
                           sep = "")

    user.attrs <- list(what.measured = "",
                       comment.text = "")

    folder.name <- set_folder_interactive(folder.name)

    oldwd <- setwd(folder.name)
    on.exit(setwd(oldwd), add = TRUE)
    on.exit(message("Folder reset to: ", getwd(), "\nBye!"), add = TRUE)

    protocol <- protocol_interactive(protocols = protocols,
                                     default = default.protocol)

    start.int.time <- 0.01 # seconds

    # initial protocol
    settings <- acq_settings(descriptor = descriptor,
                             integ.time = start.int.time,
                             target.margin = target.margin,
                             tot.time.range = tot.time.range,
                             HDR.mult = HDR.mult)

    if (is.null(seq.settings)) {
      seq.settings <- list(step.delay = 0, num.steps = 1L)
    }

    # initialize lists to collect names from current session
    irrad.names <- character()
    raw.names <- character()
    file.names <- character()

    repeat { # with same settings
      repeat{
        utils::flush.console()
        user.obj.name <- readline("Give a name to the spectrum: ")
        obj.name <- make.names(user.obj.name)
        if (obj.name != user.obj.name) {
          utils::flush.console()
          answ <- readline(paste("Use sanitised name:", obj.name, " (y-/n) :"))
          if (answ == "n") {
            obj.name <- ""
          }
        }
        if (length(obj.name) > 0 && obj.name != "" &&
            !exists(obj.name)) break()
        print("A valid and unique name is required. Please try again...")
      }

      if (grepl("-attr", interface.mode)) {
        user.attrs <- set_attributes_interactive(user.attrs)
      }

      settings <- tune_interactive(descriptor = descriptor,
                                   acq.settings = settings,
                                   start.int.time = start.int.time,
                                   interface.mode = interface.mode)

      if (grepl("series", interface.mode)) {
        seq.settings <- set_seq_interactive(seq.settings)
      }

      raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                 acq.settings = settings,
                                 seq.settings = seq.settings,
                                 protocol = protocol,
                                 user.label = obj.name)

      if (length(raw.mspct) == 0) {
        next()
      }
      # we make and keep names only if an spectrum was acquired
      raw.name <- paste(obj.name, "raw_mspct", sep = ".")
      raw.names <- c(raw.names, raw.name)
      file.name <- paste(obj.name, "spct.Rda", sep = ".")
      file.names <- c(file.names, file.name)

      if (qty.out != "raw") {
        irrad.spct <- s_irrad_corrected(x = raw.mspct,
                                        correction.method = correction.method,
                                        return.cps = qty.out == "cps")

        if (length(user.attrs$what.measured) > 0) {
          photobiology::setWhatMeasured(irrad.spct, user.attrs$what.measured)
        } else {
          photobiology::setWhatMeasured(irrad.spct, obj.name)
        }

        if (length(user.attrs$comment.text) > 0) {
          comment(irrad.spct) <- paste(comment(irrad.spct), user.attrs$comment.text, sep = "\n")
        }

        irrad.name <- paste(obj.name, "spct", sep = ".")
        irrad.names <- c(irrad.names, irrad.name)

        assign(raw.name, raw.mspct)
        assign(irrad.name, irrad.spct)

        save(list = c(raw.name, irrad.name), file = file.name)

        repeat {
          fig <- ggplot2::autoplot(irrad.spct, annotations = c("-", "title*")) +
            ggplot2::labs(title = obj.name,
                          subtitle = paste(photobiology::getWhenMeasured(irrad.spct), " UTC, ",
                                           session.label, sep = ""),
                          caption = paste("ooacquire",
                                          utils::packageVersion("ooacquire"))) +
            ggplot2::theme_bw()
          print(fig)

          if(qty.out == "cps") {
            plot.prompt <- "Plot: wavebands/discard/SAVE+NEXT (w/d/s-): "
            valid.answers <-  c("w", "d", "s")
          } else {
            plot.prompt <- "Plot: photons/energy/wavebands/discard/SAVE+NEXT (p/e/w/d/s-): "
            valid.answers <- c("p", "e", "w", "d", "s")
          }
          repeat {
            utils::flush.console()
            answer <- readline(plot.prompt)[1]
            answer <- ifelse(answer == "", "s", answer)
            if (answer %in% valid.answers) {
              break()
            } else {
              print("Answer not recognized, please try again...")
            }
          }
          switch(answer,
                 p = {options(photobiology.radiation.unit = "photon"); next()},
                 e = {options(photobiology.radiation.unit = "energy"); next()},
                 w = {
                   repeat {
                     utils::flush.console()
                     answer1 <-
                       tolower(
                         readline("Wavebands: UV+PAR/plants/visible/total/DEFAULT (u/p/v/t/d-): ")
                       )[1]
                     answer1 <- ifelse(answer1 == "", "d", answer1)
                     if (answer1 %in% c("u", "p", "v", "t", "d")) {
                       break()
                     } else {
                       print("Answer not recognized. Please try again...")
                     }
                   }
                 switch(answer1,
                        u = options(photobiology.plot.bands =
                                      c(photobiologyWavebands::UV_bands(),
                                        list(photobiologyWavebands::PAR()))),
                        p = options(photobiology.plot.bands =
                                      photobiologyWavebands::Plant_bands()),
                        v = options(photobiology.plot.bands =
                                      photobiologyWavebands::VIS_bands()),
                        t = options(photobiology.plot.bands =
                                      list(photobiology::new_waveband(
                                        photobiology::wl_min(irrad.spct),
                                        photobiology::wl_max(irrad.spct),
                                        wb.name = "Total"))),
                        options(photobiology.plot.bands = NULL))
                 next()},
                 d = break()
          )
          if (save.pdfs) {
            pdf.name <- paste(obj.name, "spct.pdf", sep = ".")
            grDevices::pdf(file = pdf.name, width = 8, height = 6)
            print(fig)
            grDevices::dev.off()
          }
          break()
        }

      } else {
        assign(raw.name, raw.mspct)
        save(list = c(raw.name), file = file.name)
      }

      repeat {
        utils::flush.console()
        if (save.collections) {
          answer2 <- readline("change protocol/collect+continue/collect+quit/abort/NEXT (p/c/q/a/n-): ")[1]
        } else {
          answer2 <- readline("change protocol/continue/quit/abort/NEXT (p/c/q/a/n-): ")[1]
        }
        answer2 <- ifelse(answer2 == "", "n", answer2)
        if (answer2 %in% c("n", "p", "c", "q", "a", "z")) {
          break()
        } else {
          print("Answer not recognized. Please try again...")
        }
      }
      if (answer2 == "") {
        next()
      } else if (answer2 == "p") {
        protocol <- protocol_interactive(protocols)
      } else if (answer2 %in% c("c", "q")) {
        if (save.collections) {
          message("Source spectra to collect: ",
                  paste(irrad.names, collapse = ", "))
          message("Raw objects to collect: ",
                  paste(raw.names, collapse = ", "), sep = " ")
          user.collection.name <- readline("Name of the collection?: ")
          collection.name <- make.names(paste("collection ",
                                              user.collection.name, sep = ""))
          if (user.collection.name == "") {
            collection.name <- make.names(paste("collection ",
                                                lubridate::now(), sep = ""))
          }
          if (collection.name != user.collection.name) {
            message("Using sanitised/generated name: '",
                    collection.name, "'.", sep = "")
          }
          utils::flush.console()
          collection.title <- readline("Title for plot?:")
          raw.collection.name <- paste(collection.name, "raw", "lst", sep = ".")
          collection.file.name <- paste(collection.name, "Rda", sep = ".")

          if (qty.out != "raw") {
            collection.mspct <-
              switch(qty.out,
                     irrad = photobiology::source_mspct(mget(irrad.names)),
                     cps =   photobiology::cps_mspct(mget(irrad.names)))

            # plot collection and summaries
            collection.fig <- ggplot2::autoplot(collection.mspct) +
              ggplot2::labs(title = collection.title,
                            subtitle = session.label,
                            caption = paste("ooacquire",
                                            utils::packageVersion("ooacquire"))) +
              ggplot2::theme(legend.position = "bottom")
            print(collection.fig)

            if (save.pdfs) {
              collection.pdf.name <- paste(collection.name, "pdf", sep = ".")
              grDevices::pdf(file = collection.pdf.name, onefile = TRUE,
                             width = 11, height = 7, paper = "a4r")
              print(collection.fig)
              grDevices::dev.off()
            }

            if (save.summaries && qty.out == "irrad") {
              summary.tb <- spct_summary(mspct = collection.mspct)
              if (!is.null(summary.tb) && is.data.frame(summary.tb)) {
                readr::write_delim(summary.tb,
                                   file =  paste(collection.name, "csv", sep = "."),
                                   delim = readr::locale()$grouping_mark)
                rm(summary.tb) # clean up
              } else {
                warning("Computation of collection summaries failed!")
              }
            }

            irrad.collection.name <- paste(collection.name, "irrad", "mspct", sep = ".")
            assign(irrad.collection.name, collection.mspct)
            assign(raw.collection.name, mget(raw.names))
            save(list = c(irrad.collection.name, raw.collection.name),
                 file = collection.file.name)
            # Clean up
            rm(collection.fig, collection.title, collection.pdf.name)
          } else {
            assign(raw.collection.name, mget(raw.names))
            save(list = raw.collection.name, file = collection.file.name)
          }
          message("collection saved to file '",
                  collection.file.name, "'.", sep = "")

          file.names <- c(file.names, collection.file.name)

          # clean up by removing the spectra that have been added to the
          # collection, and clearing the stored names afterwards
          rm(list = c(irrad.names))
          rm(list = c(raw.names))
          irrad.names <- character()
          raw.names <- character()
        }
        if (answer2 %in% c("q", "a")) {
          break()
        } else {
          repeat {
            answer3 <- readline("change protocol/NEXT (p/n-): ")[1]
            answer3 <- ifelse(answer3 == "", "n", answer3)
            if (answer3 %in% c("n", "p")) {
              break()
            } else {
              print("Answer not recognized, please try again...")
            }
          }
          if (answer3 == "p") {
            protocol <- protocol_interactive(protocols)
          }
        }
      }
    }
    save(file.names,
         file = paste("files4session-",
                      make.names(session.name),
                      ".Rda", sep = ""))

    message("Data files created during session:\n",
            paste(file.names, collapse = ",\n"), ".", sep = "")

    message("Ending...")
    end_session(w)
  }

#' @rdname acq_irrad_interactive
#'
#' @param ref.value numeric or filter_spct/reflector_spct object.
#' @param type character Type of transmittance or reflectance measured.
#'
#' @note The calculations for reflectance and transmittance are very similar,
#'   so we provide a single function capable of handling both. For
#'   transmittance the reference is usually direct exposure to radiation but
#'   for reflectance a white reference patch is normally used. In some cases
#'   one may want to use a grey reference. We provide an argument that allows
#'   the user to supply a constant or a spectrum describing the properties of
#'   the reference. It is also important to distinguish between total and
#'   internal transmittance, and between total and specular reflectance.
#'   In both cases which of these is measured depends on the measuring protocol
#'   (condition used as reference, use of an integrating sphere versus use of a
#'   probe with a narrow angle of aperture, etc.) and consequently the correct
#'   value should be entered to ensure that data are correctly tagged and
#'   later computations valid.
#'
#' @export
#'
acq_fraction_interactive <-
  function(tot.time.range = c(5, 15),
           target.margin = 0.2,
           HDR.mult = c(short = 1, long = 10),
           protocols = NULL,
           correction.method = NA,
           descriptors = NA,
           ref.value = 1,
           qty.out = "Tfr",
           type = "total",
           stray.light.method = "simple",
           save.pdfs = TRUE) {

    old.value <- options(warn = 1)
    on.exit(options(old.value), add = TRUE, after = TRUE)

    if (getOption("ooacquire.offline", TRUE)) {
      warning("ooacquire off-line: data acquisition not possible")
      message("Aborting...")
      return(FALSE)
    }

    dyn.range <- 1e3

    stopifnot(qty.out %in% c("Tfr", "Rfr", "raw"))

    # define measurement protocols
    if (length(protocols) == 0L) {
      protocols <- list(rsd = c("reference", "sample", "dark"),
                        rs = c("reference", "sample"))
    }

    w <- start_session()

    instruments <- list_srs_interactive(w = w)
    sr.index <- choose_sr_interactive(instruments = instruments)
    if (sr.index < 0L) {
      cat("Aborting...\n")
      end_session(w = w)
      message("Bye!")
    }
    ch.index <- choose_ch_interactive(instruments = instruments,
                                      sr.index = sr.index)

    serial_no <- as.character(instruments[sr.index + 1L, 3])

    message("Using channel ", ch.index + 1L,
            " from spectrometer with serial number: ", serial_no)

    if (anyNA(c(descriptors[[1]], correction.method[[1]]))) {
      descriptor <-
        switch(serial_no,
               MAYP11278 = which_descriptor(descriptors = ooacquire::MAYP11278_descriptors),
               MAYP112785 = which_descriptor(descriptors = ooacquire::MAYP112785_descriptors),
               JAZA3098 =
               { if (ch.index == 0L) {
                 ooacquire::JAZA3098_ch1_descriptors[[1]]
               } else {
                 ooacquire::JAZA3098_ch2_descriptors[[1]]
               }
               },
               get_oo_descriptor(w, sr.index = sr.index, ch.index = ch.index)
        )

      correction.method <-
        switch(serial_no,
               MAYP11278 = ooacquire::MAYP11278_ylianttila.mthd,
               MAYP112785 = ooacquire::MAYP112785_ylianttila.mthd,
               JAZA3098 =
               { if (ch.index == 0L) {
                 ooacquire::JAZA3098_ch1_none.mthd
               } else {
                 ooacquire::JAZA3098_ch2_none.mthd
               }
               },
               new_correction_method(descriptor,
                                     stray.light.method = stray.light.method)
        )

    } else {
      descriptor <- which_descriptor(descriptors = descriptors)
      stopifnot(exists("spectrometer.name", descriptor))
    }

    # needed only for descriptors retrieved from data
    descriptor[["w"]] <- w
    descriptor[["sr.index"]] <- sr.index
    descriptor[["ch.index"]] <- ch.index

    if (anyNA(c(descriptor[[1]], correction.method[[1]]))) {
      stop("No callibration data found")
    }

    # We still check serial numbers, really needed only for user supplied descriptors
    descriptor.inst <- get_oo_descriptor(w)
    stopifnot(descriptor[["spectrometer.sn"]] == descriptor.inst[["spectrometer.sn"]])


    # Before continuing we check that wavelength calibration is available
    stopifnot(length(descriptor[["wavelengths"]]) == descriptor[["num.pixs"]])

    # We get metadata from user

    utils::flush.console()
    user.session.name <- readline("Session's name: ")
    session.name <- make.names(user.session.name)
    if (user.session.name == "") {
      session.name <- make.names(lubridate::now())
    }
    if (session.name != user.session.name) {
      message("Using sanitised/generated name: '", session.name, "'.", sep = "")
    }

    utils::flush.console()
    session.label <- paste("Operator: ", readline("Operator's name: "),
                           "\nSession: ", session.name,
                           ", instrument s.n.: ", descriptor[["spectrometer.sn"]],
                           sep = "")

    user.attrs <- list(what.measured = "",
                       comment.text = "")

    folder.name <- set_folder_interactive()

    oldwd <- setwd(folder.name)
    on.exit(setwd(oldwd), add = TRUE)
    on.exit(message("Folder reset to: ", getwd(), "\nBye!"), add = TRUE)
    message("Files will be saved to '", folder.name, "'", sep="")

    protocol <- protocol_interactive(protocols = protocols)

    start.int.time <- 1 # seconds

    settings <- acq_settings(descriptor = descriptor,
                             integ.time = start.int.time,
                             target.margin = target.margin,
                             tot.time.range = tot.time.range,
                             HDR.mult = HDR.mult)

    # save current value as starting value for next iteration

    repeat {
      repeat{
        obj.name <- make.names(readline("Give a name to the spectrum: "))
        if (length(obj.name) > 0 && !exists(obj.name)) break()
        cat("A valid and unique name is required, please try again...\n")
      }

      settings <- tune_interactive(descriptor = descriptor, acq.settings = settings)

      raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                 acq.settings = settings,
                                 protocol = protocol,
                                 user.label = obj.name)

      if (length(raw.mspct) == 0) {
        next()
      }

      raw.name <- paste(obj.name, "raw_spct", sep = ".")
      assign(raw.name, raw.mspct)

      file.name <- paste(obj.name, "Rda", sep = ".")

      if (qty.out == "raw") {
        fig <- ggplot2::autoplot(raw.mspct[["sample"]], annotations = c("-", "title*")) +
          ggplot2::labs(title = raw.name,
                        subtitle = paste(photobiology::getWhenMeasured(raw.mspct[["sample"]]), " UTC, ",
                                         session.label, sep = ""),
                        caption = paste("ooacquire", utils::packageVersion("ooacquire"))) +
          ggplot2::theme_bw()
        print(fig)
        save(list = raw.name, file = file.name)
      } else {
        filter.spct <- s_fraction_corrected(raw.mspct,
                                            type = type,
                                            correction.method = correction.method,
                                            qty.out = qty.out,
                                            dyn.range = dyn.range,
                                            ref.value = ref.value)

        if (length(user.attrs$what.measured) > 0) {
          photobiology::setWhatMeasured(filter.spct, user.attrs$what.measured)
        } else {
          photobiology::setWhatMeasured(filter.spct, obj.name)
        }

        if (length(user.attrs$comment.text) > 0) {
          comment(filter.spct) <- paste(comment(filter.spct), user.attrs$comment.text, sep = "\n")
        }

        filter.name <- paste(obj.name, "spct", sep = ".")
        assign(filter.name, filter.spct)
        save(list = c(raw.name, filter.name), file = file.name)

        repeat {
          fig <- ggplot2::autoplot(filter.spct, annotations = c("-", "title*")) +
            ggplot2::labs(title = filter.name,
                          subtitle = paste(photobiology::getWhenMeasured(filter.spct), " UTC, ",
                                           session.label, sep = ""),
                          caption = paste("ooacquire", utils::packageVersion("ooacquire"))) +
            ggplot2::theme_bw()
          print(fig)
          utils::flush.console()
          answer <- readline("Change wavebands/discard/save and continue (/w/d/-): ")
          switch(substr(answer, 1, 1),
                 # p = {options(photobiology.radiation.unit = "photon"); next()},
                 # e = {options(photobiology.radiation.unit = "energy"); next()},
                 w = {answer1 <- readline("Set plot wavebands to: UV+PAR, plants, visible, total, default (u/p/v/t/-)")
                 switch(substr(answer1, 1, 1),
                        u = options(photobiology.plot.bands =
                                      c(photobiologyWavebands::UV_bands(),
                                        list(photobiologyWavebands::PAR()))),
                        p = options(photobiology.plot.bands =
                                      photobiologyWavebands::Plant_bands()),
                        v = options(photobiology.plot.bands =
                                      photobiologyWavebands::VIS_bands()),
                        t = options(photobiology.plot.bands =
                                      list(photobiology::new_waveband(
                                        photobiology::wl_min(filter.spct),
                                        photobiology::wl_max(filter.spct),
                                        wb.name = "Total"))),
                        options(photobiology.plot.bands = NULL))
                 next()},
                 d = break()
          )
          break()
        }
      }
      if (save.pdfs) {
        pdf.name <- paste(filter.name, "pdf", sep = ".")
        grDevices::pdf(file = pdf.name, width = 8, height = 6)
        print(fig)
        grDevices::dev.off()
      }

      utils::flush.console()
      user.input <- readline("Next, change protocol, quit (-/p/q): ")

      if (user.input == "") {
        next()
      } else if (substr(user.input, 1, 1) == "p") {
        protocol <- protocol_interactive(protocols)
      } else if (substr(user.input, 1, 1) == "q") {
        break()
      }
    }
    cat("Ending...\n")

    # clean up is done using 'on.exit()'
  }


# simple fraction interactive ---------------------------------------------

#' @rdname acq_irrad_interactive
#'
#' @export
#'
acq_rfr_tfr_interactive <-
  function(tot.time.range = c(10, 15),
           target.margin = 0.1,
           HDR.mult = c(1, 10),
           descriptors = NA,
           ref.value = 1,
           save.pdfs = TRUE,
           qty.out = "Tfr") {

    old.value <- options(warn = 1)
    on.exit(options(old.value), add = TRUE, after = TRUE)

    # define measurement protocols
    protocols <- list(rsd = c("reference", "sample", "dark"),
                      rs = c("reference", "sample"))

    w <- start_session()
    on.exit(end_session(w))

    instruments <- list_srs_interactive(w = w)
    sr.index <- choose_sr_interactive(instruments = instruments)
    if (sr.index < 0L) {
      cat("Aborting...\n")
      end_session(w = w)
      message("Bye!")
    }

    # needs interactive swapping
    rfr.ch.index <- 0L
    tfr.ch.index <- 1L

    serial_no <- as.character(instruments[sr.index + 1L, 3])

    message("Channels: ", rfr.ch.index, "for Rfr, and ", tfr.ch.index,
            " for Tfr; ",
            "spectrometer with s.n.: ", serial_no)

    rfr.descriptor <- ooacquire::JAZA3098_ch1_descriptors[[1]]
    tfr.descriptor <- ooacquire::JAZA3098_ch2_descriptors[[1]]

    # needed only for descriptors retrieved from data
    rfr.descriptor[["w"]] <- w
    rfr.descriptor[["sr.index"]] <- sr.index
    rfr.descriptor[["ch.index"]] <- rfr.ch.index

    tfr.descriptor[["w"]] <- w
    tfr.descriptor[["sr.index"]] <- sr.index
    tfr.descriptor[["ch.index"]] <- tfr.ch.index

    # We still check serial numbers, really needed only for user supplied descriptors
    descriptor.inst <- get_oo_descriptor(w)
    stopifnot(rfr.descriptor[["spectrometer.sn"]] == descriptor.inst[["spectrometer.sn"]])

    # Before continuing we check that wavelength calibration is available
    stopifnot(length(rfr.descriptor[["wavelengths"]]) == rfr.descriptor[["num.pixs"]])
    stopifnot(length(tfr.descriptor[["wavelengths"]]) == tfr.descriptor[["num.pixs"]])

    utils::flush.console()
    session.label <- paste("operator: ", readline("Operator's name: "),
                           ", instrument s.n.: ", rfr.descriptor[["spectrometer.sn"]],
                           sep = "")

    utils::flush.console()
    folder.name <- readline("Enter folder name (use '/' instead of '\'): ")
    if (length(folder.name == 0)) {
      folder.name <- "."
    }
    # need to add folder.name sanitation
    if (!file.exists(folder.name)) {
      message("Folder does not exist, creating it...")
      dir.create(folder.name)
    }
    oldwd <- setwd(folder.name)
    on.exit(setwd(oldwd))
    on.exit(message("Folder reset to: ", getwd(), "\nBye!"), add = TRUE)
    message("Files will be saved to '", folder.name, "'", sep="")

    protocol <- protocol_interactive(protocols = protocols)

    start.int.time <- 0.5 # seconds

    rfr.settings <-
      acq_settings(rfr.descriptor,
                   start.int.time,
                   target.margin = target.margin,
                   tot.time.range = tot.time.range,
                   HDR.mult = HDR.mult)

    tfr.settings <-
      acq_settings(tfr.descriptor,
                   start.int.time,
                   target.margin = target.margin,
                   tot.time.range = tot.time.range,
                   HDR.mult = HDR.mult)

    # save current value as starting value for next iteration

    repeat {
      repeat{
        utils::flush.console()
        obj.name <- readline("Give a name to the spectrum: ")
        if (length(obj.name) > 0 && !exists(obj.name)) break()
        cat("A valid and unique name is required, please try again...\n")
      }
      rfr.raw.name <- paste(obj.name, "rfr_raw_spct", sep = ".")
      tfr.raw.name <- paste(obj.name, "tfr_raw_spct", sep = ".")
      rfr.name <- paste(obj.name, "rfr_spct", sep = ".")
      tfr.name <- paste(obj.name, "tfr_spct", sep = ".")
      spct.name <- paste(obj.name, "spct", sep = ".")
      file.name <- paste(obj.name, "Rda", sep = ".")
      pdf.name <- paste(obj.name, "pdf", sep = ".")

      cat("REFLECTANCE:\n")
      rfr.settings <- tune_interactive(descriptor = rfr.descriptor,
                                       acq.settings = rfr.settings)

      rfr.raw.mspct <- acq_raw_mspct(descriptor = rfr.descriptor,
                                     acq.settings = rfr.settings,
                                     protocol = protocol,
                                     user.label = spct.name)

      if (length(rfr.raw.mspct) == 0) {
        next()
      }

      assign(rfr.raw.name, rfr.raw.mspct)

      cat("TRANSMITANCE:\n")
      tfr.settings <- tune_interactive(descriptor = tfr.descriptor,
                                       acq.settings = tfr.settings)

      tfr.raw.mspct <- acq_raw_mspct(descriptor = tfr.descriptor,
                                     acq.settings = tfr.settings,
                                     protocol = protocol,
                                     user.label = spct.name)

      if (length(tfr.raw.mspct) == 0) {
        next()
      }

      assign(tfr.raw.name, tfr.raw.mspct)

      # processing

      if (qty.out == "raw") {
        save(list = c(rfr.raw.name, tfr.raw.name),
             file = file.name)
      } else {
        # reflectance
        rfr.raw.mspct %>%
          photobiology::msmsply(trim_counts) %>%
          photobiology::msmsply(linearize_counts) %>%
          raw2cps() %>%
          photobiology::msmsply(merge_cps) -> rfr.cps.mspct

        photobiology::cps2Rfr(rfr.cps.mspct$sample,
                              rfr.cps.mspct$reference,
                              rfr.cps.mspct$dark) -> rfr.spct

        assign(rfr.name, rfr.spct)

        # transmitance
        tfr.raw.mspct %>%
          photobiology::msmsply(trim_counts) %>%
          photobiology::msmsply(linearize_counts) %>%
          raw2cps() %>%
          photobiology::msmsply(merge_cps) -> tfr.cps.mspct

        photobiology::cps2Tfr(tfr.cps.mspct$sample,
                              tfr.cps.mspct$reference,
                              tfr.cps.mspct$dark) -> tfr.spct

        assign(tfr.name, tfr.spct)

        object.spct <-
          photobiology::object_spct(w.length = rfr.spct[["w.length"]],
                                    Rfr = rfr.spct[["Rfr"]],
                                    Tfr = tfr.spct[["Tfr"]],
                                    Rfr.type = "total",
                                    Tfr.type = "total",
                                    comment = obj.name)

        object.spct <-
          photobiology::copy_attributes(rfr.spct, object.spct,
                                        c("what_measured", "when_measured", "instr_desc"))

        object.spct <- photobiology::clip_wl(object.spct)

        assign(spct.name, object.spct)

        save(list = c(rfr.raw.name, tfr.raw.name, spct.name, rfr.name, tfr.name),
             file = file.name)

        repeat {
          fig1 <- ggplot2::autoplot(rfr.spct, range = c(280, 850), annotations = c("-", "title*")) +
            ggplot2::labs(title = spct.name,
                          subtitle = paste(photobiology::getWhenMeasured(rfr.spct), " UTC, ",
                                           session.label, sep = ""),
                          caption = paste("ooacquire", utils::packageVersion("ooacquire"))) +
            ggplot2::theme_bw()
          fig2 <- ggplot2::autoplot(tfr.spct, range = c(280, 850), annotations = c("-", "title*")) +
            ggplot2::labs(title = spct.name,
                          subtitle = paste(photobiology::getWhenMeasured(tfr.spct), " UTC, ",
                                           session.label, sep = ""),
                          caption = paste("ooacquire", utils::packageVersion("ooacquire"))) +
            ggplot2::theme_bw()

          print(ggspectra::multiplot(fig1, fig2))

          utils::flush.console()
          answer <- readline("Change wavebands/discard/save and continue (/w/d/-): ")
          switch(answer,
                 # p = {options(photobiology.radiation.unit = "photon"); next()},
                 # e = {options(photobiology.radiation.unit = "energy"); next()},
                 w = {answer1 <- readline("Set plot wavebands: UV+PAR, plants, visible, total, default (u/p/v/t/-)")
                 switch(answer1,
                        u = options(photobiology.plot.bands =
                                      c(photobiologyWavebands::UV_bands(),
                                        list(photobiologyWavebands::PAR()))),
                        p = options(photobiology.plot.bands =
                                      photobiologyWavebands::Plant_bands()),
                        v = options(photobiology.plot.bands =
                                      photobiologyWavebands::VIS_bands()),
                        t = options(photobiology.plot.bands =
                                      list(photobiology::new_waveband(
                                        photobiology::wl_min(object.spct),
                                        photobiology::wl_max(object.spct),
                                        wb.name = "Total"))),
                        options(photobiology.plot.bands = NULL))
                 next()},
                 d = break()
          )
          break()
        }
      }
      if (save.pdfs) {
        grDevices::pdf(file = pdf.name, width = 8, height = 6, onefile = TRUE)
        print(fig1)
        print(fig2)
        grDevices::dev.off()
      }

      utils::flush.console()
      user.input <- readline("NEXT/change protocol/quit (n-/p/q): ")

      if (user.input[1] == "") {
        next()
      } else if (user.input[1] == "p") {
        protocol <- protocol_interactive(protocols)
      } else if (user.input[1] == "q") {
        break()
      }
    }
    cat("Ending...\n")

    # clean up is done using 'on.exit()'
  }

spct_summary <- function(mspct,
                         unit.out = getOption("photobiology.radiation.unit",
                                              default = "energy"),
                         scale.factor = ifelse(unit.out == "photon",
                                               1e6, 1),
                         type = "plant",
                         digits = 3L) {

  # handle also single spectra
  if (is.generic_spct(mspct)) {
    mspct <- generic_mspct(list(mspct), class = class(mspct)[1])
  }

  ratio <- switch(unit.out,
                  photon = photobiology::q_ratio,
                  quantum = photobiology::q_ratio,
                  energy = photobiology::e_ratio)

  if (type %in% c("plant", "PAR")) {
    plant.wb <- switch(type,
                       PAR = c(photobiologyWavebands::UV_bands("CIE"),
                               list(photobiologyWavebands::PAR())),
                       plant = c(photobiologyWavebands::Plant_bands()))
    irrad.tb <-
      photobiology::irrad(mspct,
                          unit.out = unit.out,
                          scale.factor = scale.factor,
                          w.band = plant.wb)
    uv_ratios.tb <-
      ratio(mspct,
            w.band.num = photobiologyWavebands::UV_bands(),
            w.band.denom = photobiologyWavebands::PAR())
    vis_ratios.tb <-
      ratio(mspct,
            w.band.num = list(blue = photobiologyWavebands::Blue("Sellaro"),
                              red = photobiologyWavebands::Red("Smith10")),
            w.band.denom = list(green = photobiologyWavebands::Green("Sellaro"),
                                "far-red" = photobiologyWavebands::Far_red("Smith10")),
            attr2tb = c("when.measured"))
    summary.tb <- dplyr::full_join(irrad.tb, uv_ratios.tb)
    summary.tb <- dplyr::full_join(summary.tb, vis_ratios.tb)
  } else if (type == "VIS") {
    summary.tb <-
      photobiology::irrad(mspct,
                          unit.out = unit.out,
                          w.band = photobiologyWavebands::VIS_bands(),
                          attr2tb = c("when.measured"))
  } else { # total
    summary.tb <-
      photobiology::irrad(mspct,
                          unit.out = unit.out,
                          w.band = NULL,
                          attr2tb = c("when.measured"))
  }

  # summary.tb <- photobiology::add_attr2tb(tb = summary.tb,
  #                                         mspct = mspct)

  selector <- unname(sapply(summary.tb, is.numeric))
  summary.tb[ , selector] <- signif(summary.tb[ , selector], digits = digits)

  summary.tb
}
