#' Acquire spectral irradiance or fluence
#'
#' Interactive front-end allowing acquisition of spectral irradiance and
#' spectral fluence using Ocean Optics spectrometers. Output of spectral data in
#' R data files stored in objects suitable for use with packages 'photobiology'
#' and 'ggspectra' as well as plots as PDF files and summaries as comma
#' separated files and R objects.
#'
#' @details  Function \code{acq_irrad_interactive()} supports measurement of
#'   spectral irradiance from continuous light sources and spectral fluence
#'   from discontinuous ones. For spectral irradiance it assumes that the
#'   duration of the measurement event is the relevant time base for expression
#'   of the flux of radiation. For spectral fluence the flux of radiation is
#'   expressed per pulse of illumination.
#'
#'   This function can be used to acquire spectra using different protocols for
#'   acquisition and stray light and dark corrections. The protocols are
#'   described in the vignettes and in the help for the low-level functions
#'   called by this function, also from this same package.
#'
#'   Using this function only requires an Ocean Optics spectrometer to be
#'   connected to the computer where R is running and the OmniDriver runtime
#'   from Ocean Insight installed. The connection to the spectrometer and
#'   selection of channel, when relevant, is done from within these functions.
#'
#'   The irradiance calibration will be retrieved from the spectrometer memory
#'   as a last resource if not supplied in any other way. Given that the factors
#'   are stored by Ocean Optics in a format that ignores the entrance optics,
#'   either the effective cosine diffuser area in xxx should be passed to
#'   parameter \code{area} or a character string with the type of the diffuser
#'   passed to \code{diff.type}. If no irradiance calibration is available,
#'   counts per second (cps) or raw counts are the only options available.
#'
#'   Three main protocols and two variations are available by default. They
#'   differ in the additional measurements done to correct for stray light and
#'   dark noise and in the sequence in which they are acquired. The default
#'   protocols are usually suitable, if new protocols are passed, each character
#'   vector must contain strings "light", "filter" and "dark".
#'
#'   In the case of spectral irradiance, the default is to set the integration time
#'   automatically so that the number of counts at the highest peak is close to
#'   1 - \code{target.margin} times the maximum of the range of the instrument
#'   detector (retrieved from the calibration or the instrument memory). The
#'   minimum \code{tot.time} is obtained by increasing the number of scans. The
#'   maximum integration time supported by the spectrometer is not exceeded.
#'
#'   In the case of spectral fluence the default is for the integration time to
#'   be set manually and for a message to be displayed asking for the light
#'   pulse to be manually triggered. It is possible to override the default
#'   function by one that triggers the light source automatically.
#'
#'   Plots are produced with functions from package 'ggspectra' and respect the
#'   default annotations set with function \code{set_annotations_default()},
#'   default wavebands set with function \code{set_w.band_default()}, and
#'   irradiance quantities set with \code{photon_as_default()}, and
#'   \code{energy_as_default()}.
#'
#'   The different interface modes available are suitable for different types of
#'   measurements.
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
#' @param entrance.optics character, name or geometry of diffuser, needed only
#'   if there is more than one for the same instrument.
#' @param stray.light.method character Used only when the correction method is
#'   created on-the-fly.
#' @param seq.settings named list with numeric members \code{start.boundary},
#'   \code{initial.delay}, \code{"step.delay"} and \code{"num.steps"}.
#' @param area numeric Passed to \code{o_calib2irrad_mult()}.
#' @param diff.type character Passed to \code{o_calib2irrad_mult()}.
#' @param qty.out character One of "irrad" (spectral irradiance), "fluence"
#'   (spectral fluence), "cps" (counts per second), or "raw"
#'   (raw sensor counts).
#' @param summary.type character One of "plant", "PAR" or "VIS".
#' @param save.pdfs,save.summaries,save.collections logical Whether to save
#'   plots to PDFs files or not, and collection summaries to csv files or not,
#'   enable collections user interface or not..
#' @param interface.mode character One of "auto", "simple", "manual", "full",
#'   "series", "auto-attr", "simple-attr", "manual-attr", "full-atr", and
#'   "series-attr".
#' @param num.exposures integer Number or light pulses (flashes) per scan. Set
#'   to \code{-1L} to indicate that the light source is continuous.
#' @param f.trigger.pulses function Function to be called to trigger light
#'   pulse(s). Should accept as its only argument the number of pulses, and
#'   return \code{TRUE} on success and \code{FALSE} on failure.
#' @param folder.name,session.name,user.name character Default name of the
#'   folder used for output, and session and user names.
#'
#' @export
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
#'   The function is composed in a modular way from functions that can be
#'   reshuffled and combined with other functions to define new variations
#'   possibly better suited to users' needs and tastes.
#'
#' @return These functions return the acquired spectra through "side effects" as
#'   each spectrum is saved, both as raw counts data and optionally as spectral
#'   irradiance or counts-per-second  spectral data in an \code{.rda} file as
#'   objects of the classes defined in package 'photobiology'. Optionally, the
#'   plot for each spectrum is saved as a \code{.pdf} file. At any time, the
#'   current group of spectra can be saved as a collection. When a collection
#'   is created, spectral data are for several spectra are saved together.
#'   Summaries are saved to a CSV file and joint plots to a \code{.pdf} file.
#'   The value returned by the function is that from closing the
#'   connection to the spectrometer.
#'
#' @examples
#' # please, see also the example scripts installed with the package
#'
#' \dontrun{
#' # requires an Ocean Optics spectrometer to be connected via USB
#'
#' acq_irrad_interactive()
#' acq_irrad_interactive(qty.out = "cps")
#'
#' }
#'
acq_irrad_interactive <-
  function(tot.time.range = c(5, 15),
           target.margin = 0.1,
           HDR.mult = if (qty.out == "fluence")
                        c(short = 1) else c(short = 1, long = 10),
           protocols = NULL,
           correction.method = NA,
           descriptors = NA,
           entrance.optics = NULL,
           stray.light.method = "none",
           seq.settings = NULL,
           area = NULL,
           diff.type = NULL,
           qty.out = "irrad",
           summary.type = "plant",
           save.pdfs = TRUE,
           save.summaries = TRUE,
           save.collections = interface.mode != "simple",
           interface.mode = ifelse(qty.out == "fluence", "manual", "auto"),
           num.exposures = ifelse(qty.out == "fluence", 1L, -1L),
           f.trigger.pulses = f.trigger.message,
           folder.name = paste("acq", qty.out,
                               lubridate::today(tzone = "UTC"),
                               sep = "-"),
           user.name = Sys.info()[["user"]],
           session.name = paste(user.name,
                                strftime(lubridate::now(tzone = "UTC"),
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
        c("auto", "simple", "manual", "full", "series")) {
      stop("Invalid argument for 'interface.mode', aborting.", call. = FALSE)
    }

    # validate qty.out
    qty.out <- tolower(qty.out)
    stopifnot(qty.out %in% c("irrad", "fluence", "cps", "raw"))

    # define measurement protocols
    default.protocols <- list(l = "light",
                              ld = c("light", "dark"),
                              lf = c("light", "filter"),
                              lfd = c("light", "filter", "dark"),
                              dl = rev(c("light", "dark")),
                              dfl = rev(c("light", "filter", "dark"))
    )
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
    on.exit(end_session(w)) # ensure session is always closed!

    instruments <- list_srs_interactive(w = w)
    sr.index <- choose_sr_interactive(instruments = instruments)
    if (sr.index < 0L) {
      print("Aborting...")
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
               MAYP11278 = which_descriptor(descriptors = ooacquire::MAYP11278_descriptors,
                                            entrance.optics = entrance.optics),
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
               MAYP114590 = "lfd",
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

    if (interface.mode != "series") {
      acq.overhead <- NA_real_ # safeguard as it should not be used
    } else if (grepl("^MAY", serial_no)) {
      acq.overhead <- 1e-3 # 1 ms
    } else if (grepl("^FLM", serial_no)) {
      acq.overhead <- 1e-3 # 1 ms
    } else if (grepl("^USB", serial_no)) {
      acq.overhead <- 0.1 # 100 ms slow
    } else {
      acq.overhead <- 50e-3 # 50 ms, just in case
    }

    # We still check serial numbers, really needed only for user supplied descriptors
    descriptor.inst <- get_oo_descriptor(w)
    stopifnot(descriptor[["spectrometer.sn"]] == descriptor.inst[["spectrometer.sn"]])

    # We check that wavelength calibration is available
    stopifnot(length(descriptor[["wavelengths"]]) == descriptor[["num.pixs"]])
    # We check for valid calibration multipliers
    if (length(descriptor[["inst.calib"]][["irrad.mult"]]) != descriptor[["num.pixs"]] ||
        anyNA(descriptor[["inst.calib"]][["irrad.mult"]])) {
      if (qty.out %in% c("irrad", "fluence")) {
        warning("Bad calibration data, returning counts-per-second.",
                call. = FALSE)
        qty.out = "cps"
      }
    }

    # We get metadata from user, offering defaults
    session.name <- make.names(session.name) # validate argument passed in call
    session.prompt <- paste("Session's name (<string>/\"", session.name, "\"-): ", sep = "")
    utils::flush.console()
    user.session.name <- readline(session.prompt)
    if (! user.session.name == "") {
      session.name <- make.names(user.session.name)
      if (user.session.name == "") {
        session.name <- make.names(format(lubridate::now(tzone = "UTC")))
      }
      if (session.name != user.session.name) {
        message("Using sanitised/generated name: '", session.name, "'.", sep = "")
      }
    }

    user.name <- make.names(user.name) # validate argument passed in call
    user.name.prompt <- paste("Operator's name (<string>/\"", user.name, "\"-): ", sep = "")
    utils::flush.console()
    user.user.name <- readline(user.name.prompt)
    if (! user.user.name == "") {
      user.name <- make.names(user.user.name)
    }
    message("Using \"", user.name, "\" as operator's name", sep = "")
    session.label <- paste("Operator: ", user.name,
                           "\nSession: ", session.name,
                           ", instrument s.n.: ", descriptor[["spectrometer.sn"]],
                           sep = "")

    user.attrs <-
      list(what.measured = "",
           comment.text = "",
           how.measured = paste("Acquired with ", descriptor[["spectrometer.name"]],
                                " (", descriptor[["spectrometer.sn"]],
                                "), R (", paste(R.version[["major"]], R.version[["minor"]], sep = "."),
                                "), 'ooacquire' (", utils::packageVersion("ooacquire"),
                                ") in mode \"", interface.mode,
                                "\",\n 'rOmniDriver' (", utils::packageVersion("rOmniDriver"),
                                ") and OmniDriver (", rOmniDriver::get_api_version(w), ").",
                                sep = ""))

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
                             HDR.mult = HDR.mult,
                             num.exposures = num.exposures)

    if (is.null(seq.settings)) {
      seq.settings <- list(start.boundary = "second",
                           initial.delay = 0.1,
                           step.delay = 0,
                           num.steps = 1L)
    } else if (!setequal(names(seq.settings),
                         c("start.boundary", "initial.delay", "step.delay", "num.steps"))) {
      warning("Missing or wrong member names in 'seq.settings': ignoring!")
      seq.settings <- list(start.boundary = "second",
                           initial.delay = 0.1,
                           step.delay = 0,
                           num.steps = 1L)
    }

    # initialize lists to collect names from current session
    irrad.names <- character()
    raw.names <- character()
    file.names <- character()

    reuse.old.refs <- FALSE # none yet available

    repeat { # main loop for UI
      repeat{
        user.obj.name <- readline("Give a name to the spectrum: ")
        obj.name <- make.names(user.obj.name)
        if (obj.name != user.obj.name) {
          utils::flush.console()
          answ <- readline(paste("Use sanitised name:", obj.name, " (y-/n) :"))
          if (answ == "n") {
            obj.name <- ""
          }
        }
        if (length(obj.name) > 0 && obj.name != "") {
          # we make names
          irrad.name <- paste(obj.name, "spct", sep = ".")
          raw.name <- paste(obj.name, "raw_mspct", sep = ".")
          file.name <- paste(obj.name, "spct.Rda", sep = ".")
          if ((irrad.name %in% irrad.names) || file.exists(file.name)) {
            if (readline(paste("Overwrite existing: '", irrad.name, ". (y/n-) :"))[1] == "y") {
              irrad.names <- setdiff(irrad.names, irrad.name)
              raw.names <- setdiff(raw.names, raw.name)
              break()
            }
          } else {
            break()
          }
        }
        print("A valid and unique name is required. Please try again...")
      }

      if (grepl("-attr", interface.mode)) {
        user.attrs <- set_attributes_interactive(user.attrs)
      }

      if (!reuse.old.refs) {
        # using previous dark and filter spectra is possible only with
        # same settings
        settings <- tune_interactive(descriptor = descriptor,
                                     acq.settings = settings,
                                     start.int.time = start.int.time,
                                     interface.mode = interface.mode)
      }

      if (grepl("series", interface.mode)) {

        estimated.measurement.duration <-
          sum(settings$integ.time * settings$num.scans * 1e-6) +
          acq.overhead * length(settings$HDR.mult) + # number of HDR acquisitions
          sum(0.66 * settings$integ.time * 1e-6) # estimate of worse case overhead due to free-running

        message("Estimated duration of one measurement with overhead: ", signif(estimated.measurement.duration, 3), " s.")

        seq.settings <-
          set_seq_interactive(seq.settings = seq.settings,
                              measurement.duration = estimated.measurement.duration,
                              minimum.step.delay = ifelse(length(settings$HDR.mult) == 1L,
                                                          0,
                                                          estimated.measurement.duration))
      }

      if (reuse.old.refs) {
        raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                   acq.settings = settings,
                                   seq.settings = seq.settings,
                                   protocol = "light",
                                   f.trigger.pulses = f.trigger.pulses,
                                   user.label = obj.name)
      } else {
        raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                   acq.settings = settings,
                                   seq.settings = seq.settings,
                                   protocol = protocol,
                                   f.trigger.pulses = f.trigger.pulses,
                                   user.label = obj.name)
      }

      if (length(raw.mspct) == 0) {
        # failed data acquisition
        next()
      }

      if (reuse.old.refs) {
        # we add old refs to new light data
        raw.mspct <- c(old.refs.mpsct, raw.mspct)
      } else {
        # we save old references for possible reuse
        refs.selector <- grep("dark|filter", protocol, value = TRUE)
        if (length(refs.selector)) {
          old.refs.mpsct <- raw.mspct[refs.selector]
        } else {
          old.refs.mpsct <- raw_mspct()
        }
      }

      if (qty.out != "raw") {
        # for series measurements we can have multiple "light" raw spectra
        spct.names <-
          list(light = grep("^light", names(raw.mspct), value = TRUE),
               filter = "filter",
               dark = "dark")

        irrad.spct <- s_irrad_corrected(x = raw.mspct,
                                        spct.names = spct.names,
                                        correction.method = correction.method,
                                        return.cps = qty.out == "cps")

        photobiology::setHowMeasured(irrad.spct, user.attrs$how.measured)

        if (user.attrs$what.measured == "") {
          photobiology::setWhatMeasured(irrad.spct, obj.name)
        } else {
          photobiology::setWhatMeasured(irrad.spct, user.attrs$what.measured)
        }

        if (user.attrs$comment.text != "") {
          comment(irrad.spct) <-
            paste(comment(irrad.spct), user.attrs$comment.text, sep = "\n")
        }

        repeat {
          fig <- ggplot2::autoplot(irrad.spct,
                                   annotations = c("-", "colour.guide"),
                                   geom = ifelse(getMultipleWl(irrad.spct) == 1,
                                                 "spct", "line")) +
            ggplot2::labs(title = paste(what_measured(irrad.spct)[[1L]],
                                        " (n = ", getMultipleWl(irrad.spct), ")",
                                        sep = ""),
                          subtitle = when_measured(irrad.spct)[[1L]],
                          caption = how_measured(irrad.spct)[[1L]]) +
            ggplot2::theme(legend.position = "bottom") +
            ggplot2::theme_bw()
          print(fig)

          if(qty.out == "cps") {
            plot.prompt <- "Plot: wavebands/discard/SAVE+NEXT (w/d/s-): "
            valid.answers <-  c("w", "d", "s")
          } else {
            plot.prompt <- "photons/energy/wavebands/discard/SAVE+NEXT (p/e/w/d/s-): "
            valid.answers <- c("p", "e", "w", "d", "s")
          }
          repeat {
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
                         readline("Bands: UV+PhR/UV+PAR/plants/VIS/TOT/DEFAULT (u/a/p/v/t/d-): ")
                       )[1]
                     answer1 <- ifelse(answer1 == "", "d", answer1)
                     if (answer1 %in% c("u", "a", "p", "v", "t", "d")) {
                       break()
                     } else {
                       print("Answer not recognized. Please try again...")
                     }
                   }
                   switch(answer1,
                          u = options(photobiology.plot.bands =
                                        c(photobiologyWavebands::UV_bands(),
                                          list(photobiologyWavebands::PhR()))),
                          a = options(photobiology.plot.bands =
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

          # moved from above so that saving is skipped for discarded spectra
          # one could use a temporary file for maximum safety...
          raw.names <- c(raw.names, raw.name)
          file.names <- c(file.names, file.name)
          irrad.names <- c(irrad.names, irrad.name)

          assign(raw.name, raw.mspct)
          assign(irrad.name, irrad.spct)

          save(list = c(raw.name, irrad.name), file = file.name)

          if (save.pdfs) {
            pdf.name <- paste(obj.name, "spct.pdf", sep = ".")
            grDevices::pdf(file = pdf.name, width = 8, height = 6)
            print(fig)
            grDevices::dev.off()
          }
          break()
        }

      } else {
        # moved from above so that saving is skipped for discarded spectra
        raw.names <- c(raw.names, raw.name)
        file.names <- c(file.names, file.name)

        assign(raw.name, raw.mspct)
        save(list = raw.name, file = file.name)
      }

      if (save.collections || save.summaries) {
        repeat {
          answer.collect <-
            readline("collect and/or summarize? yes/NO (y/n-): ")[1]
          if (answer.collect %in% c("n", "")) {
            collect.and.save <- FALSE
            break()
          } else if (answer.collect == "y") {
            collect.and.save <- TRUE
            break()
          } else {
            print("Answer not recognized. Please try again...")
          }
        }

        if (collect.and.save) {
          message("Corrected ", qty.out, " spectra to collect: ",
                  paste(irrad.names, collapse = ", "))
          message("Raw objects to collect: ",
                  paste(raw.names, collapse = ", "), sep = " ")
          user.collection.name <- readline("Name of the collection?: ")
          collection.name <- make.names(paste("collection ",
                                              user.collection.name, sep = ""))
          if (user.collection.name == "") {
            collection.name <- make.names(paste("collection ",
                                                lubridate::now(tzone = "UTC"), sep = ""))
          }
          if (collection.name != user.collection.name) {
            message("Using sanitised/generated name: '",
                    collection.name, "'.", sep = "")
          }
          collection.title <- readline("Title for plot?: ")
          if (collection.title == "") {
            collection.title <- collection.name
          }
          collection.file.name <- paste(collection.name, "Rda", sep = ".")
          collection.objects <- character()

          if (qty.out != "raw") {
            collection.mspct <-
              switch(qty.out,
                     irrad = photobiology::source_mspct(mget(irrad.names)),
                     cps =   photobiology::cps_mspct(mget(irrad.names)))

            # plot collection and summaries
            if (length(collection.mspct) > 200) {
              plot.data = "median"
            } else {
              plot.data = "as.is"
            }
            collection.fig <-
              ggplot2::autoplot(collection.mspct,
                                annotations =
                                  c("-", "peaks", "colour.guide", "summaries"),
                                plot.data = plot.data) +
              ggplot2::labs(title = paste(collection.title, " (n = ",
                                          length(collection.mspct), ")",
                                          sep = ""),
                            subtitle = session.label,
                            caption = how_measured(collection.mspct[[1L]])) +
              ggplot2::theme(legend.position = "bottom")
            print(collection.fig)
            rm(collection.title)

            if (save.pdfs) {
              collection.pdf.name <- paste(collection.name, "pdf", sep = ".")
              grDevices::pdf(file = collection.pdf.name, onefile = TRUE,
                             width = 11, height = 7, paper = "a4r")
              print(collection.fig)
              grDevices::dev.off()
              rm(collection.pdf.name)
            }
            rm(collection.fig)

            if (save.summaries) {
              contents.collection.name <-
                paste(collection.name, "contents.tb", sep = ".")
              assign(contents.collection.name, summary(collection.mspct))
              collection.objects <- c(collection.objects, contents.collection.name)
              if (qty.out %in% c("irrad", "fluence")) {
                last.summary.type <- summary.type
                repeat{
                  valid.answers <- c("plant", "PAR", "VIS")
                  summary.type <- readline(paste("Change summary type from \"",
                                                 last.summary.type, "\"? (", "): ",
                                                 paste(valid.answers, collapse = "/", sep = ""),
                                                 ": ", sep = ""))[1]
                  if (summary.type == "") {
                    summary.type <- last.summary.type
                  }
                  if (summary.type %in% valid.answers) {
                    break()
                  } else {
                    print("Answer not recognized. Please try again...")
                  }
                }
                summary.tb <-
                  irrad_summary_table(mspct = collection.mspct)
                if (!is.null(summary.tb) && is.data.frame(summary.tb)) {
                  readr::write_delim(summary.tb,
                                     file =  paste(collection.name, "csv", sep = "."),
                                     delim = readr::locale()$grouping_mark)
                  summary.collection.name <- paste(collection.name, "summary.tb", sep = ".")
                  assign(summary.collection.name, summary.tb)
                  collection.objects <- c(collection.objects, summary.collection.name)
                } else {
                  message("Computation of summaries failed!")
                }
              }
            }

            if (save.collections) {
              irrad.collection.name <- paste(collection.name, qty.out, "mspct", sep = ".")
              assign(irrad.collection.name, collection.mspct)
              collection.objects <- c(collection.objects, irrad.collection.name)

              raw.collection.name <- paste(collection.name, "raw", "lst", sep = ".")
              assign(raw.collection.name, mget(raw.names))
              collection.objects <- c(collection.objects, raw.collection.name)
              repeat {
                save(list = collection.objects, file = collection.file.name, precheck = TRUE)
                if (file.exists(collection.file.name)) {
                  message("Collection objects saved to file '",
                          collection.file.name, "'.", sep = "")
                  # save file name to report at end of sessions
                  file.names <- c(file.names, collection.file.name)
                  # remove saved objects and the list with their names
                  rm(list = collection.objects)
                  rm(collection.objects)
                  # clean up by removing the spectra that have been added to the
                  # saved collection and reset the list of names for next collection
                  rm(list = c(irrad.names))
                  rm(list = c(raw.names))
                  irrad.names <- character()
                  raw.names <- character()
                  break()
                } else {
                  message("Saving of the collection to file failed!")
                }
              }
            }
          }
        }
      }

      repeat {
        valid.answers <- c("q", "r", "n")
        answer2 <-
          readline("quit/repeat/NEXT (q/r/n-): ")[1]
        answer2 <- ifelse(answer2 == "", "n", answer2)
        if (answer2 %in% valid.answers) {
          break()
        } else {
          print("Answer not recognized. Please try again...")
        }
      }

      if (answer2 == "r") {
        reuse.old.refs <- TRUE
        answer2 <- "n"
      } else {
        reuse.old.refs <- FALSE
      }

      if (answer2 == "q") {
        break() # out of UI main loop
      } else if (!reuse.old.refs) {
         repeat {
          answer3 <- readline("Change protocol? yes/NO (y/n-): ")[1]
          answer3 <- ifelse(answer3 == "", "n", answer3)
          if (answer3 %in% c("n", "y")) {
            break()
          } else {
            print("Answer not recognized, please try again...")
          }
        }
        if (answer3 == "y") {
          protocol <- protocol_interactive(protocols)
        }
      }

    } # end of main UI loop

    save(file.names,
         file = paste("files4session-",
                      make.names(session.name),
                      ".Rda", sep = ""))

    message("Data files saved during session:\n",
            "to ", getwd(), "\n",
            paste(file.names, collapse = ",\n"), ".", sep = "")

    message("Ending data acquisition...")

  }

#' Summarize spectral irradiance or fluence
#'
#' Compute irradiance or fluence by waveband and energy or photon ratios
#' between wavebands of interest to plants' and human visual responses to light.
#'
#' @param mspct A source_mspct, or a source_spct object containing spectral
#'    irradiance for one or more sources.
#' @param unit.out character One of "photon" or "energy".
#' @param scale.factor numeric A multiplicative factor used to rescale data.
#' @param attr2tb character Vector with one or more of "when.measured",
#'    "what.measured", "where.measured", "how.measured" and "comment".
#' @param summary.type character One of "plant", "PAR" or "VIS".
#' @param digits integer The number of significant digits in the output.
#'
#' @details This function packages different functions from pacakge 'photobiology'
#'    and returns a typical set of summaries for different purposes.
#'
#' @return A tibble with one row per spectrum and one column per
#'    summary quantity and attribute and a column with the names of the spectra.
#'
#' @export
#'
#' @seealso See the documentation for functions
#'   \code{\link[photobiology]{irrad}},
#'   \code{\link[photobiology]{q_ratio}}, \code{\link[photobiology]{e_ratio}},
#'   \code{\link[photobiology]{add_attr2tb}},
#'   \code{\link[photobiologyInOut]{spct_CRI}},
#'   \code{\link[photobiologyInOut]{spct_CCT}}
#'   and \code{\link[base]{signif}} which are called to build
#'   the summary table.
#'
#' @examples
#'
#' irrad_summary_table(sun.spct)
#' irrad_summary_table(sun.spct, attr2tb = c("what.measured", "where.measured"))
#' irrad_summary_table(sun.spct, summary.type = "plant", unit.out = "photon")
#' irrad_summary_table(sun.spct, summary.type = "PAR", unit.out = "photon")
#' irrad_summary_table(sun.spct, summary.type = "VIS", unit.out = "energy")
#'
irrad_summary_table <-
  function(mspct,
           unit.out = getOption("photobiology.radiation.unit",
                                default = "energy"),
           scale.factor = ifelse(unit.out == "photon",
                                 1e6, 1),
           attr2tb = "when.measured",
           summary.type = "plant",
           digits = 3L) {

  # handle also single spectra
  if (is.generic_spct(mspct)) {
    mspct <- generic_mspct(list(mspct), class = class(mspct)[1])
  }
  if (any(unname(sapply(mspct, getMultipleWl)) > 1)) {
    mspct <- subset2mspct(mspct)
  }

  ratio <- switch(unit.out,
                  photon = photobiology::q_ratio,
                  quantum = photobiology::q_ratio,
                  energy = photobiology::e_ratio)

  if (summary.type %in% c("plant", "PAR")) {
    plant.wb <- switch(summary.type,
                       PAR = c(photobiologyWavebands::UV_bands("CIE"),
                               list(photobiologyWavebands::PAR())),
                       plant = c(photobiologyWavebands::Plant_bands(),
                                 list(photobiologyWavebands::PAR())))
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
                              red = photobiologyWavebands::Red("Smith20")),
            w.band.denom = list(green = photobiologyWavebands::Green("Sellaro"),
                                "far-red" = photobiologyWavebands::Far_red("Smith20")),
            attr2tb = attr2tb)
    summary.tb <- dplyr::full_join(irrad.tb, uv_ratios.tb, by = "spct.idx")
    summary.tb <- dplyr::full_join(summary.tb, vis_ratios.tb, by = "spct.idx")
  } else if (summary.type == "VIS") {
    summary.tb <-
      photobiology::irrad(mspct,
                          unit.out = unit.out,
                          scale.factor = scale.factor,
                          w.band = photobiologyWavebands::VIS_bands(),
                          attr2tb = attr2tb)
    summary.tb[["CRI"]] <- sapply(mspct,
                                  photobiologyInOut::spct_CRI,
                                  tol = 0.0054,
                                  named = FALSE)
    summary.tb[["CCT"]] <- sapply(mspct,
                                  photobiologyInOut::spct_CCT,
                                  strict = TRUE,
                                  named = FALSE)
  } else { # total
    summary.tb <-
      photobiology::irrad(mspct,
                          unit.out = unit.out,
                          scale.factor = scale.factor,
                          w.band = NULL,
                          attr2tb = attr2tb)
  }

  selector <- unname(sapply(summary.tb, is.numeric))
  summary.tb[ , selector] <- signif(summary.tb[ , selector], digits = digits)

  summary.tb
}

