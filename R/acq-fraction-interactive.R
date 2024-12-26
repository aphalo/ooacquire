#' Acquire spectral fraction
#'
#' Interactive front-end allowing acquisition of spectral fractions using Ocean
#' Optics spectrometers. Output of spectral data in R data files stored in
#' objects suitable for use with packages 'photobiology' and 'ggspectra' as well
#' as plots as PDF files and summaries as comma separated files.
#'
#' @details This function can be used to acquire spectral reflectance,
#'   spectral transmittance and/or spectral absorptance using
#'   different protocols for acquisition and stray light and dark corrections.
#'   Depending on the optical setup, solid or liquid samples can be measured.
#'   The kinetics of changes in optical properties can be captured as a time
#'   series of spectra, using `interface.mode = "series"`.
#'
#'   The protocols are described in the vignettes and in the help for the
#'   lower level functions called, also from this same package.
#'
#'   Using this function only requires an Ocean Optics spectrometer to be
#'   connected to the computer where R is running and the OmniDriver runtime
#'   from Ocean Insight installed. The connection to the spectrometer and
#'   selection of channel, when relevant, is done from within these functions.
#'   A stable and continuous source of light is also needed as well as black,
#'   white and possibly grey reflectance patches.
#'
#'   The calculations for reflectance and transmittance are very similar,
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
#'   A wavelength calibration is needed, but being the measurements relative,
#'   no calibration of pixel responsiveness is required. A known linearization
#'   function is also needed.
#'
#'   Some protocols are available by default. They differ in the
#'   additional measurements done to correct for stray light and dark noise. The
#'   default protocols are usually suitable, if new protocols are passed, each
#'   character vector must contain strings "light", "filter" and "dark".
#'
#'   By default the integration time is set automatically so that the number of
#'   counts at the highest peak is close to 1 - \code{target.margin} times the
#'   maximum of the range of the instrument detector (retrieved from the
#'   calibration or the instrument memory). The minimum \code{tot.time} is
#'   obtained by increasing the number of scans. The maximum integration time
#'   supported by the spectrometer is not exceeded.
#'
#'   Plots are produced with functions from package 'ggspectra' and respect the
#'   default annotations set with function \code{set_annotations_default()},
#'   and default wavebands set with function \code{set_w.band_default()}.
#'
#'   The different interface modes available are suitable for different types of
#'   measurements.
#'
#' @seealso This function calls functions \code{\link{tune_interactive}},
#'   \code{\link{protocol_interactive}} and
#'   \code{\link{set_attributes_interactive}}.
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
#' @param type character Type of transmittance or reflectance measured.
#' @param stray.light.method character Used only when the correction method is
#'   created on-the-fly.
#' @param seq.settings named list with numeric members \code{start.boundary},
#'   \code{initial.delay}, \code{"step.delay"} and \code{"num.steps"}.
#' @param light.source character One of "continuous", "pulsed".
#' @param ref.value numeric or filter_spct/reflector_spct object.
#' @param qty.out character One of "Tfr" (spectral transmittance as a fraction
#'   of one), "cps" (counts per second), or "raw"
#'   (raw sensor counts).
#' @param plot.lines.max integer Maximum number of spectra to plot as individual
#'   lines. Random sampling is used if number of spectra exceeds
#'   \code{plot.lines.max}.
#' @param summary.type character One of "plant", "PAR" or "VIS".
#' @param save.pdfs,save.summaries,save.collections logical Whether to save
#'   plots to PDFs files or not, and collection summaries to csv files or not,
#'   enable collections user interface or not.
#' @param async.saves logical A flag enabling or disabling the use of concurrent
#'   processes to save data to files. Package 'mirai' must be installed before
#'   enabling this feature.
#' @param show.figs logical Default for flag enabling display plots of acquired
#'   spectra.
#' @param interface.mode character One of "auto", "simple", "manual", "full",
#'   "series", "auto-attr", "simple-attr", "manual-attr", "full-atr", and
#'   "series-attr".
#' @param num.exposures integer Number or light pulses (flashes) per scan. Set
#'   to \code{-1L} to indicate that the light source is continuous.
#' @param f.trigger.on,f.trigger.off,f.trigger.init function Functions to be
#'   called immediately before and immediately after a measurement, and any
#'   initialization code needed before a repeat. See \code{\link{acq_raw_spct}}
#'   for details.
#' @param triggers.enabled character vector Names of protocol steps during which
#'   trigger functions should be called.
#' @param folder.name,session.name,user.name character Default name of the
#'   folder used for output, and session and user names.
#' @param folder.name,session.name,user.name character Default name of the
#'   folder used for output, and session and user names.
#' @param verbose logical If TRUE additional messages are emitted, including
#'   report on memory usage.
#' @param QC.enabled logical If FALSE return NA skipping QC.
#'
#' @export
#'
#' @note Calibration data needs in most cases to be imported into R and
#'   parameters entered for the special correction algorithms into a correction
#'   method descriptor. The corrections are skipped if the needed information is
#'   missing. If no wavelength calibration is available and attempt is
#'   made to retrieve it from the spectrometer.
#'
#'   The function is composed in a modular way from functions that can be
#'   reshuffled and combined with other functions to define new variations
#'   possibly better suited to users' needs and tastes. Even easier is to
#'   simply change the default arguments in a wrapper function or in a script.
#'
#' @return These functions return the acquired spectra through "side effects" as
#'   each spectrum is saved, both as raw counts data and optionally as spectral
#'   transmittance or counts-per-second data in an \code{.rda} file as
#'   objects of the classes defined in package 'photobiology'. Optionally, the
#'   plot for each spectrum is saved as a \code{.pdf} file. At any time, the
#'   current group of spectra can be saved as a collection. When a collection is
#'   created, spectral data for several spectra are saved together.
#'   Summaries are saved to a CSV file and joint plots to a \code{.pdf} file.
#'   The value returned by the function is that from closing the connection to
#'   the spectrometer.
#'
#' @examples
#' # please, see also the example scripts installed with the package
#'
#' \dontrun{
#' # requires an Ocean Insight (former Ocean Optics) spectrometer to be
#' # connected via USB
#'
#' acq_fraction_interactive()
#'
#' }
#'
#'
acq_fraction_interactive <-
  function(tot.time.range = c(5, 15),
           target.margin = 0.1,
           HDR.mult = if (light.source == "pulsed")
             c(short = 1) else c(short = 1, long = 10),
           protocols = NULL,
           correction.method = NA,
           descriptors = NA,
           stray.light.method = "simple",
           seq.settings = NULL,
           light.source = "continuous",
           ref.value = 1,
           qty.out = "Tfr",
           type = "total",
           plot.lines.max = 11,
           summary.type = "VIS",
           save.pdfs = TRUE,
           save.summaries = !interface.mode %in% c("series", "series-attr"),
           save.collections = !interface.mode %in% c("simple", "series", "series-attr"),
           async.saves = FALSE,
           show.figs = TRUE,
           interface.mode = ifelse(light.source == "pulsed", "manual", "auto"),
           num.exposures = ifelse(light.source == "pulsed", 1L, -1L),
           f.trigger.init = NA,
           f.trigger.on = f.trigger.message,
           f.trigger.off = NA,
           triggers.enabled = c("sample", "reference"),
           folder.name = paste("acq", qty.out,
                               lubridate::today(tzone = "UTC"),
                               sep = "-"),
           user.name = Sys.info()[["user"]],
           session.name = paste(user.name,
                                strftime(lubridate::now(tzone = "UTC"),
                                         "%Y.%b.%d_%H.%M.%S"),
                                sep = "_"),
           verbose = getOption("photobiology.verbose", default = FALSE),
           QC.enabled = TRUE) {

    ## Is the driver available?
    if (getOption("ooacquire.offline", FALSE)) {
      warning("ooacquire off-line: Aborting...")
      return()
    }

    ## Asynchronous file saving
    if (is.null(async.saves)) {
      # in the future NULL could be a dynamic default dependent of file size
      async.saves <- FALSE
    }

    if (async.saves && !requireNamespace("mirai", quietly = TRUE)) {
      message("Ignoring 'async.saves = TRUE' as package 'mirai' is not installed")
      async.saves <- FALSE
    }

    if (async.saves) {
      cat("Will save files asynchronously\n(not blocking data acquisitions)\n")
    } else {
      cat("Will save files synchronously\n(blocking data acquisition until files are saved)\n")
    }

    # initialize mirai
    rda.mirai <- NA
    pdf.mirai <- NA

    ## set R options
    if (is.null(getOption("digits.secs"))) {
      old.options <- options(warn = 1, "digits.secs" = 3)
    } else {
      old.options <- options(warn = 1)
    }
    on.exit(options(old.options), add = TRUE, after = TRUE)

    dyn.range <- 1e3

    ## Validate arguments
    # validate interface mode
    interface.mode <- tolower(interface.mode)
    if (!gsub("-attr$", "", interface.mode) %in%
        c("auto", "simple", "manual", "full", "series")) {
      stop("Invalid argument for 'interface.mode', aborting.", call. = FALSE)
    }

    # validate qty.out
    stopifnot(qty.out %in% c("Tfr", "Rfr", "raw"))

    # initialize repeats counter

    pending.repeats <- 1L
    series.start <- TRUE

    # define measurement protocols
    default.protocols <- list(rsd = c("reference", "sample", "dark"),
                              rs = c("reference", "sample"))

    if (length(protocols) == 0) {
      protocols <- default.protocols
    } else if (inherits(protocols, "character")) {
      protocols <- default.protocols[protocols]
      if (length(protocols) == 0) {
        stop("Aborting! Requested protocol is not available.", call. = FALSE)
      }
    }

    ## connect to spectrometer
    w <- start_session()
    on.exit(end_session(w),
            add = TRUE) # ensure session is always closed!

    # Transfer focus to console (e.g., from editor pane)
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      rstudioapi::executeCommand('activateConsole')
    }

    # If multiple spectrometers are connected, ask which one to use
    instruments <- list_srs_interactive(w = w)
    sr.index <- choose_sr_interactive(instruments = instruments)
    # -1L returned only if user aborts the selection
    if (sr.index < 0L) {
      message("Aborting at user's request!  Bye!")
      return(NULL)
    }
    # If the spectrometer has > 1 channel, ask which one to use
    ch.index <- choose_ch_interactive(instruments = instruments,
                                      sr.index = sr.index)

    # retrieve serial number of spectrometer in use
    serial_no <- as.character(instruments[sr.index + 1L, 3])

    cat("Using channel ", ch.index,
        " from spectrometer with serial number: ", serial_no, "\n")

    if (anyNA(c(descriptors[[1]], correction.method[[1]]))) {
      descriptor <-
        switch(serial_no,
               MAYP11278 = which_descriptor(descriptors = ooacquire::MAYP11278_descriptors,
                                            entrance.optics = "cosine"),
               MAYP112785 = which_descriptor(descriptors = ooacquire::MAYP112785_descriptors),
               MAYP114590 = which_descriptor(descriptors = ooacquire::MAYP114590_descriptors),
               FLMS04133 = which_descriptor(descriptors = ooacquire::FLMS04133_descriptors),
               FLMS00673 = which_descriptor(descriptors = ooacquire::FLMS00673_descriptors),
               FLMS00440 = which_descriptor(descriptors = ooacquire::FLMS00440_descriptors),
               FLMS00416 = which_descriptor(descriptors = ooacquire::FLMS00416_descriptors),
               JAZA3098 =
                 {
                   if (ch.index == 0L) {
                     ooacquire::JAZA3098_ch1_descriptors[[1]]
                   } else {
                     ooacquire::JAZA3098_ch2_descriptors[[1]]
                   }
                 },
               { # default for no match to serial_no
                 cat("No instrument descriptor found, retrieving it from the spectrometer\n")
                 get_oo_descriptor(w, sr.index = sr.index, ch.index = ch.index)
               }
        )

      correction.method <-
        switch(serial_no,
               MAYP11278 = ooacquire::MAYP11278_simple.mthd,
               MAYP112785 = ooacquire::MAYP112785_simple.mthd,
               MAYP114590 = ooacquire::MAYP114590_simple.mthd,
               FLMS04133 = ooacquire::FLMS04133_none.mthd,
               FLMS00673 = ooacquire::FLMS00673_none.mthd,
               FLMS00440 = ooacquire::FLMS00440_none.mthd,
               FLMS00416 = ooacquire::FLMS00416_none.mthd,
               JAZA3098 =
               {
                 if (ch.index == 0L) {
                   ooacquire::JAZA3098_ch1_none.mthd
                 } else {
                   ooacquire::JAZA3098_ch2_none.mthd
                 }
               },
               new_correction_method(descriptor,
                                     stray.light.method = stray.light.method)
        )

    } else {
      # we need a wavelength calibration, usually available in descriptor
      descriptor <- which_descriptor(descriptors = descriptors)
      stopifnot(exists("spectrometer.name", descriptor))
    }

    # default protocols are available for most spectrometers
    available.protocols <- names(protocols)
    default.protocol <- ifelse("rsd" %in% available.protocols, "rsd", available.protocols[1])

    # jwrapper and spectrometer indexes have to be set to current ones if
    # descriptor was not acquired from the spectrometer in the current session
    descriptor[["w"]] <- w
    descriptor[["sr.index"]] <- sr.index
    descriptor[["ch.index"]] <- ch.index

    if (length(descriptor) < 10 || length(correction.method) < 5) {
      stop("No spectrometer data found")
    }

    # acquisition overheads, important for time series
    if (!grepl("^series", interface.mode)) {
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

    # check serial numbers, really needed only for user supplied descriptors
    descriptor.inst <- get_oo_descriptor(w)
    stopifnot(descriptor[["spectrometer.sn"]] == descriptor.inst[["spectrometer.sn"]])

    # check that wavelength calibration is available
    stopifnot(length(descriptor[["wavelengths"]]) == descriptor[["num.pixs"]])

    ## Session settings
    # session and user IDs
    session.name <- set_session_name_interactive(session.name)
    user.name <- set_user_name_interactive(user.name)
    session.label <- paste("Operator: ", user.name,
                           "\nSession: ", session.name,
                           ", instrument s.n.: ", descriptor[["spectrometer.sn"]],
                           sep = "")

    # set working directory for current session
    folder.name <- set_folder_interactive(folder.name)
    oldwd <- setwd(folder.name)
    on.exit(setwd(oldwd), add = TRUE)
    on.exit(message("Folder reset to: ", getwd(), "\nBye!"), add = TRUE)

    # set default for metadata attributes
    user.attrs <-
      list(what.measured = "",
           comment.text = "",
           how.measured = paste("Acquired with ", descriptor[["spectrometer.name"]],
                                " (", descriptor[["spectrometer.sn"]],
                                "), with a ", descriptor[["entrance.optics"]][["geometry"]], " diffuser",
                                ")\nR (", paste(R.version[["major"]], R.version[["minor"]], sep = "."),
                                "), 'ooacquire' (", utils::packageVersion("ooacquire"),
                                ") in mode \"", interface.mode,
                                "\", 'rOmniDriver' (", utils::packageVersion("rOmniDriver"),
                                ") and OmniDriver (", rOmniDriver::get_api_version(w), ").",
                                sep = ""))

    # ask user to choose protocol only if needed
    if (length(protocols) > 1) {
      protocol <- protocol_interactive(protocols = protocols,
                                       default = default.protocol)
    } else {
      protocol <- protocols[[default.protocol]]
    }

    # set default data acquisition settings based of call arguments
    start.int.time <- 1 # seconds
    num.scans <- min(max(tot.time.range) %/% start.int.time, 1L)
    settings <- acq_settings(descriptor = descriptor,
                             integ.time = start.int.time,
                             num.scans = num.scans,
                             target.margin = target.margin,
                             tot.time.range = tot.time.range,
                             HDR.mult = HDR.mult,
                             num.exposures = num.exposures)

    # set default sequential settings for time series
    if (is.null(seq.settings)) {
      seq.settings <- list(start.boundary = "none",
                           initial.delay = 0,
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

    # initialize counters used for sequential naming and repeats
    total.repeats <- 1L
    pending.repeats <- 0L
    series.start <- TRUE
    file.counter <- 0L
    seq.name.digits <- 3L
    acq.pausing <- TRUE

    # initialize lists to collect names from current session
    filter.names <- character()
    raw.names <- character()
    file.names <- character()

    # initialize interface logic flags
    reuse.old.refs <- FALSE # none yet available
    reuse.seq.settings <- FALSE
    reset.count <- TRUE
    get.obj.name <- TRUE
    get.seq.settings <- grepl("^series", interface.mode)
    sequential.naming <- FALSE
    sequential.naming.required <- FALSE
    clear.display <- FALSE

    # initialize default object name
    base.obj.name <- "ooacq_#"

    # save current value as starting value for next iteration

    repeat { # main loop for UI

      # forcing memory garbage collection could help avoid random delays
      # during time-series acquisition
      gc.data <- gc(full = TRUE)
      if (verbose) {
        print(gc.data)
      }

      if (clear.display) {
        # clear plot viewer panel of RStudio
        print(ggplot2::ggplot() +
                ggplot2::ggtitle("Display of plots disabled") +
                ggplot2::theme_minimal())
        clear.display <- FALSE
      }

      repeat{ # obtain a valid object name from user

        if (get.obj.name) {
          current.sequential.naming <- sequential.naming
          current.base.obj.name <- base.obj.name
          if (sequential.naming && file.counter > 0) {
            obj.name.prompt <-
              paste("Give a name to the spectrum (",
                    current.base.obj.name, "#): ", sep = "")
          } else {
            obj.name.prompt <-
              "Give a name to the spectrum: "
          }

          repeat {
            readline(obj.name.prompt) |> trimws() -> user.obj.name
            if (user.obj.name == "" && current.sequential.naming) {
              # we reuse base.obj.name and keep counting
              reset.count <- FALSE
              sequential.naming <- TRUE
              user.obj.name <- base.obj.name
            } else if (grepl("#$|%$", user.obj.name)) {
              # we start the count using a new base.name
              reset.count <- grepl("#$", user.obj.name)
              sequential.naming <- TRUE
              user.obj.name <- sub("#$|%$", "", user.obj.name)
            } else {
              # new name with no hash
              sequential.naming <- sequential.naming.required
              reset.count <- TRUE
            }

            base.obj.name <- make.names(user.obj.name)
            if (base.obj.name != user.obj.name) {
              answ <- readline(paste("Use sanitised (base) name '", base.obj.name, "'? (y-/n): "))
              if (answ == "n") {
                base.obj.name <- current.base.obj.name
                next()
              }
            }
            break()
          }
        }

        # sequential object and file naming

        if (sequential.naming) {
          if (reset.count) {
            file.counter <- 1
            reset.count <- FALSE
          } else {
            file.counter <- file.counter + 1
          }
        } else {
          file.counter <- 0
        }

        if (sequential.naming) {
          obj.name <- paste(base.obj.name, formatC(file.counter, width = 3, flag = "0"), sep = "")
        } else {
          obj.name <- base.obj.name
        }

        # generate object names from base name

        filter.name <- paste(obj.name, "spct", sep = ".")
        raw.name <- paste(obj.name, "raw_mspct", sep = ".")
        file.name <- paste(obj.name, "spct.Rda", sep = ".")

        # although base name is known valid, the name may be already in use
        if ((filter.name %in% filter.names) || file.exists(file.name)) {
          if (sequential.naming && reuse.seq.settings) {
            # likely running unattended
            cat("Overwriting existing: '", filter.name, "'.", sep = "")
            filter.names <- setdiff(filter.names, filter.name)
            raw.names <- setdiff(raw.names, raw.name)
            break()
          } else {
            # operator likely present
            if (readline(paste("Overwrite existing '", filter.name, "? (y/n-): "))[1] == "y") {
              filter.names <- setdiff(filter.names, filter.name)
              raw.names <- setdiff(raw.names, raw.name)
              break()
            }
          }
        } else {
          break()
        }
        print("A valid and unique name is required. Please try again...")

      }  # obtain a valid object name from user

      # user input of metadata for attributes "comment" and "what.measured"
      if (grepl("-attr", interface.mode)) {
        user.attrs <- set_attributes_interactive(user.attrs)
      }

      # adjust acquisition settings
      if (!reuse.old.refs) {
        # using previous dark and filter spectra is possible only with
        # same settings
        settings <- tune_interactive(descriptor = descriptor,
                                     acq.settings = settings,
                                     start.int.time = start.int.time,
                                     interface.mode = interface.mode)
        # with new settings we start with one repeat
        pending.repeats <- 1
        total.repeats <- 1
        get.seq.settings <- grepl("series", interface.mode)
      }

      # time series settings
      if (get.seq.settings) {

        # Estimate time needed for measuring one spectrum
        if (length(settings$HDR.mult) > 1) { # acq settings once per integ time value
          estimated.measurement.duration <-
            sum(settings$integ.time * settings$num.scans * 1e-6) +
            acq.overhead * length(settings$HDR.mult) + # number of HDR acquisitions
            sum(0.66 * settings$integ.time * 1e-6) # worse case overhead due to restart
        } else if (length(settings$HDR.mult) == 1) { # no need to change acq settings
          if (settings$num.scans > 1) {
            estimated.measurement.duration <-
              settings$integ.time * settings$num.scans * 1e-6 + acq.overhead
          } else if (settings$num.scans == 1) { # buffered high speed acquisition
            estimated.measurement.duration <-
              settings$integ.time * 1e-6 # no overhead
          } else {
            estimated.measurement.duration <- NA_real_
          }
        }

        stopifnot("Estimated measurement duration is not finite" =
                    is.finite(estimated.measurement.duration),
                  "Estimated measurement duration <= 0" =
                    estimated.measurement.duration > 0)

        cat("Duration of each repeated measurement: ",
            signif(estimated.measurement.duration, 3), " s.\n")

        seq.settings <-
          set_seq_interactive(seq.settings = seq.settings,
                              measurement.duration = estimated.measurement.duration,
                              minimum.step.delay = ifelse(length(settings$HDR.mult) == 1L,
                                                          0,
                                                          estimated.measurement.duration),
                              time.division = ifelse(length(settings$HDR.mult) == 1L,
                                                     settings$integ.time * 1e-6, # -> seconds
                                                     0))
        get.seq.settings <- FALSE

      }

      if (pending.repeats >= 1) {
        cat("\nRepeat ", total.repeats - pending.repeats + 1,
            " of ", total.repeats, ".\n", sep = "")
      }

      # call trigger- or measurement initialization function
      if (is.function(f.trigger.init)) {
        f.trigger.init()
      }

      # acquire raw-counts spectra
      if (reuse.old.refs) { # acquire only sample spectra
        if (acq.pausing) {
          raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                     acq.settings = settings,
                                     seq.settings = seq.settings,
                                     protocol = "sample",
                                     pause.fun = NULL,
                                     f.trigger.on = f.trigger.on,
                                     f.trigger.off = f.trigger.off,
                                     triggers.enabled = intersect(protocol, triggers.enabled),
                                     user.label = obj.name)
          if (pending.repeats > 1) {
            answer.abort <- readline(prompt = "Skip pending repeats? yes/NO (y/n-): ")
            if (answer.abort %in% c("y", "z")) {
              pending.repeats <- 1
            }
          }
        } else {
          if (pending.repeats == total.repeats) {
            readline(paste("Acquire SAMPLE reading(s): g = GO (g-):"))[1]
          }
          raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                     acq.settings = settings,
                                     seq.settings = seq.settings,
                                     protocol = "sample",
                                     pause.fun = function(...) {TRUE},
                                     f.trigger.on = f.trigger.on,
                                     f.trigger.off = f.trigger.off,
                                     triggers.enabled = intersect(protocol, triggers.enabled),
                                     user.label = obj.name)
        }
      } else { # acquire all spectra needed for protocol
        raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                   acq.settings = settings,
                                   seq.settings = seq.settings,
                                   protocol = protocol,
                                   pause.fun = NULL, # default
                                   f.trigger.on = f.trigger.on,
                                   f.trigger.off = f.trigger.off,
                                   triggers.enabled = intersect(protocol, triggers.enabled),
                                   user.label = obj.name)
      }

      if (length(raw.mspct) == 0) {
        message("Failure: no data acquired!")
        if (reuse.seq.settings) {
          # avoid endless looping when unattended
          break()
        } else {
          next()
        }
      }

      # combine spectra if needed
      if (reuse.old.refs) {
        cat("Retrieving ", paste(names(old.refs.mpsct), collapse = " and "),
            " spectrum/a ... ", sep = "")
        # we add old refs to new light data
        raw.mspct <- c(old.refs.mpsct, raw.mspct)
        cat("ready!\n")
      } else {
        # we save old references for possible reuse
        refs.selector <- grep("dark|reference", protocol, value = TRUE)
        if (length(refs.selector)) {
          cat("Cacheing ", paste(refs.selector,
                                 collapse = " and "),
              " spectrum/a ... ", sep = "")
          old.refs.mpsct <- raw.mspct[refs.selector]
          cat("ready!\n")
        } else {
          old.refs.mpsct <- raw_mspct() # empty object
        }
      }

      # convert into physical units, display and save to file
      if (qty.out != "raw") {
        # for series measurements we can have multiple "sample" raw spectra
        spct.names <-
          list(sample = grep("^sample", names(raw.mspct), value = TRUE),
               reference = "reference",
               dark = "dark")

        if (length(raw.mspct) > 10L) {
          cat("Computing ", qty.out, " ... ", sep = "")
        }

        filter.spct <- s_fraction_corrected(x = raw.mspct,
                                            spct.names = spct.names,
                                            type = type,
                                            reference.value = 1,
                                            correction.method = correction.method,
                                            dyn.range = dyn.range,
                                            qty.out = qty.out,
                                            ref.value = ref.value,
                                            verbose = verbose)

        cat('Adding metadata ... ')
        photobiology::setHowMeasured(filter.spct, user.attrs$how.measured)

        if (user.attrs$what.measured == "") {
          photobiology::setWhatMeasured(filter.spct, obj.name)
        } else {
          photobiology::setWhatMeasured(filter.spct, user.attrs$what.measured)
        }

        if (user.attrs$comment.text != "") {
          comment(filter.spct) <-
            paste(comment(filter.spct), user.attrs$comment.text, sep = "\n")
        }

        if (length(raw.mspct) > 10L) {
          cat("ready.\n")
        }

        # prepare plot invariants
        if (plot.lines.max < getMultipleWl(filter.spct)) {
          title.text <- paste(what_measured(filter.spct)[[1L]],
                              " (n = ", plot.lines.max,
                              "/", getMultipleWl(filter.spct),
                              ")",
                              sep = "")
          plot.spct <- pull_sample(filter.spct, size = plot.lines.max)
        } else {
          title.text <- paste(what_measured(filter.spct)[[1L]],
                              " (n = ", getMultipleWl(filter.spct), ")",
                              sep = "")
          plot.spct <- filter.spct
        }

        # display plot, allowing user to tweak it
        repeat {
          if (length(plot.spct) > 10L) {
            cat("Building plot ... ")
          }
          fig <- ggplot2::autoplot(plot.spct,
                                   annotations = c("-", "colour.guide"),
                                   geom = ifelse(getMultipleWl(plot.spct) == 1,
                                                 "spct", "line")) +
            ggplot2::labs(title = title.text,
                          subtitle = when_measured(plot.spct)[[1L]],
                          caption = how_measured(plot.spct)[[1L]]) +
            ggplot2::theme(legend.position = "bottom") +
            ggplot2::theme_bw()
          if (length(raw.mspct) > 10L) {
            cat("ready.\n")
          }

          if (show.figs) {
            print(fig)
          } else {
            if (clear.display) {
              # clear plot viewer panel of RStudio
              print(ggplot2::ggplot() +
                      ggplot2::ggtitle("Display of plots disabled") +
                      ggplot2::theme_minimal())
              clear.display <- FALSE
            }
          }

          # user interaction and display of plot only if at end of measurement
          # to avoid delays
          if (!reuse.seq.settings || pending.repeats == 1) {
            plot.prompt <- "fig/w.bands/discard+go/SAVE+GO (f/w/d/s-): "
            valid.answers <-  c("f","w", "d", "s", "g")
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
                   f = {clear.display <- show.figs; show.figs <- !show.figs; next()},
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
                                            photobiology::wl_min(filter.spct),
                                            photobiology::wl_max(filter.spct),
                                            wb.name = "Total"))),
                            options(photobiology.plot.bands = NULL))
                     next()},
                   d = break() # exit loop early, discarding acquired data
            )
          }

          # update lists of objects and files created during current session
          # since start or most recent saving of a collection.
          raw.names <- c(raw.names, raw.name)
          file.names <- c(file.names, file.name)
          filter.names <- c(filter.names, filter.name)

          # "rename" temporary objects
          assign(raw.name, raw.mspct)
          assign(filter.name, filter.spct)
          obj.names <- c(raw.name, filter.name)

          # save objects to files on disk
          if (async.saves && !mirai::unresolved(rda.mirai)) {
            # non-blocking
            cat("Saving files asynchronously ...\n")
            rda.mirai <-
              mirai::mirai({
                assign(obj.names[1], raw.mspct)
                assign(obj.names[2], filter.spct)
                save(list = obj.names, file = file.name)
                return(file.exists(file.name))
              },
              obj.names = obj.names,
              file.name = file.name,
              raw.mspct = raw.mspct,
              filter.spct = filter.spct,
              .timeout = 60000 # 60 s
              )
          } else {
            # fall back to main process
            cat("Saving files ... ")
            save(list = obj.names, file = file.name)
            cat("ready\n")
          }

          # save plots to files on disk
          if (save.pdfs) {
            pdf.name <- paste(obj.name, "spct.pdf", sep = ".")
            if (async.saves && !mirai::unresolved(pdf.mirai)) {
              pdf.mirai <- mirai::mirai({
                grDevices::pdf(file = pdf.name, width = 8, height = 6)
                print(fig)
                grDevices::dev.off()
                return(file.exists(pdf.name))
              },
              pdf.name = pdf.name,
              fig = fig,
              .timeout = 60000 # 60 s
              )
            } else {
              grDevices::pdf(file = pdf.name, width = 8, height = 6)
              print(fig)
              grDevices::dev.off()
            }
          }
          break()
        }

      } else { # qty.out == "raw"

        # currently no plotting!
        if (!reuse.seq.settings || pending.repeats == 1) {
          raw.prompt <- "discard/SAVE+NEXT (d/s-): "
          valid.answers <-  c("d", "s")
          repeat {
            answer <- readline(raw.prompt)[1]
            answer <- ifelse(answer == "", "s", answer)
            if (answer %in% valid.answers) {
              break()
            } else {
              print("Answer not recognized, please try again...")
            }
          }

          # save raw-counts data to file on disk
          if (answer == "s") {
            raw.names <- c(raw.names, raw.name)
            file.names <- c(file.names, file.name)

            assign(raw.name, raw.mspct)

            if (async.saves && !mirai::unresolved(rda.mirai)) {
              cat("Saving files asynchronously\n")
              rda.mirai <- mirai::mirai({
                assign(raw.name, raw.mspct)
                save(list = raw.name, file = file.name)
                return(file.exists(file.name))
              },
              .args = list(raw.name, file.name, raw.mspct),
              .timeout = 60000) # 1 min
            } else {
              cat("Saving files ... ")
              save(list = raw.name, file = file.name)
              cat("ready\n")
            }
          }
        }
      }

      # make collection from all spectra acquired since start of session or last
      # saving of a collection
      if (pending.repeats == 0 && (save.collections || save.summaries)) {

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

        # Construct collection object and create plot of collection
        if (collect.and.save) {
          message("Quantity \"", qty.out, "\" spectra to collect: ",
                  paste(filter.names, collapse = ", "))
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

          # name of the Rda file used to save the collection
          collection.file.name <- paste(collection.name, "Rda", sep = ".")
          # collection.objects used to collect all R objects to be saved to the Rda file
          collection.objects <- character()

          if (qty.out != "raw") {
            # construct collection of computed spectra, needed for summaries
            # even when not saved to file on disk!
            collection.mspct <-
              switch(qty.out,
                     fluence = photobiology::source_mspct(mget(filter.names)),
                     filter = photobiology::source_mspct(mget(filter.names)),
                     cps =   photobiology::cps_mspct(mget(filter.names)))

            # plot collection and summaries
            if (plot.lines.max < getMultipleWl(filter.spct)) {
              collection.title <- paste(collection.title,
                                        " (sample of ", plot.lines.max, ")",
                                        sep = "")
            } else {
              collection.title <- paste(collection.title,
                                        " (n = ", getMultipleWl(filter.spct), ")",
                                        sep = "")
            }

            # create plot
            # if too many spectra to plot, draw a random sample of the maximum size
            collection.fig <-
              ggplot2::autoplot(pull_sample(collection.mspct, plot.lines.max),
                                annotations =
                                  c("-", "peaks", "colour.guide", "summaries")) +
              ggplot2::labs(title = collection.title,
                            subtitle = session.label,
                            caption = how_measured(collection.mspct[[1L]])) +
              ggplot2::theme(legend.position = "bottom")
            print(collection.fig)
            rm(collection.title)

            # save plot to file on disk
            if (save.pdfs) {
              collection.pdf.name <- paste(collection.name, "pdf", sep = ".")
              grDevices::pdf(file = collection.pdf.name, onefile = TRUE,
                             width = 11, height = 7, paper = "a4r")
              print(collection.fig)
              grDevices::dev.off()
              rm(collection.pdf.name)
            }
            rm(collection.fig)

            # create table of summaries from the spectra in the collection
            if (save.summaries) {
              contents.collection.name <-
                paste(collection.name, "contents.tb", sep = ".")
              assign(contents.collection.name, summary(collection.mspct))
              collection.objects <- c(collection.objects, contents.collection.name)

              if (qty.out %in% "Tfr") {
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
                  Tfr_summary_table(mspct = collection.mspct)

                # save summary table to file on disk
                if (!is.null(summary.tb) && is.data.frame(summary.tb)) {
                  readr::write_delim(summary.tb,
                                     file =  paste(collection.name, "csv", sep = "."),
                                     delim = readr::locale()$grouping_mark)
                  summary.collection.name <- paste(collection.name, "summary.tb", sep = ".")
                  # "rename" data frame with summaries
                  assign(summary.collection.name, summary.tb)
                  collection.objects <- c(collection.objects, summary.collection.name)
                } else {
                  message("Computation of summaries failed!")
                }
              }
            }

            # create collection of raw-counts spectra and save all collections
            if (save.collections) {
              # "rename" temporary objects
              filter.collection.name <- paste(collection.name, qty.out, "mspct", sep = ".")
              assign(filter.collection.name, collection.mspct)
              collection.objects <- c(collection.objects, filter.collection.name)

              raw.collection.name <- paste(collection.name, "raw", "lst", sep = ".")
              assign(raw.collection.name, mget(raw.names))
              collection.objects <- c(collection.objects, raw.collection.name)

              # save collections to files on disk
              retrying <- FALSE
              repeat {
                save(list = collection.objects, file = collection.file.name, precheck = TRUE)
                if (file.exists(collection.file.name)) {
                  message("Collection objects saved to file '",
                          collection.file.name, "'.", sep = "")
                  # save file name to report at end of session
                  file.names <- c(file.names, collection.file.name)
                  # remove saved objects and the list with their names
                  rm(list = collection.objects)
                  rm(collection.objects)
                  # reset the lists of names for next collection
                  filter.names <- character()
                  raw.names <- character()
                  message("Object lists reset")
                  break()
                } else {
                  if (retrying) {
                    message("Saving of the collection to file failed again! (Aborting)")
                    break()
                  }
                  message("Saving of the collection to file failed! (Trying again)")
                  retrying <- TRUE
                }
              }
            }
          }
        }
      }

      # whole-measurement repeats remaining to be done
      pending.repeats <- pending.repeats - 1L

      if (pending.repeats >= 1) {
        get.obj.name <- FALSE
        acq.pausing <- acq.pausing.always
      } else {
        get.obj.name <- TRUE
        acq.pausing <- TRUE

        get.seq.settings <- grepl("^series", interface.mode)

        loop.valid.answers <- c("q", "r", "m", "n")
        loop.prompt <- "quit/repeat/NEW-MEASUREMENT (q/r/m-): "
        repeat {
          answer2 <- readline(loop.prompt)[1]
          answer2 <- ifelse(answer2 == "", "m", answer2)
          if (answer2 %in% loop.valid.answers) {
            break()
          } else {
            print("Answer not recognized. Please try again...")
          }
        }

        if (answer2 == "r") {
          repeat {
            prompt <- paste("Number of repeats (integer >= 1 or \"\" = ",
                            total.repeats, "): ")
            answer4 <- readline(prompt)
            if (answer4 == "") {
              pending.repeats <- total.repeats
              break()
            } else {
              pending.repeats <- try(as.integer(answer4))
              if (!is.na(pending.repeats) && pending.repeats >= 1L) {
                total.repeats <- pending.repeats
                break()
              } else {
                cat("Value entered is not a number >= 1!\n")
              }
            }
          }
          repeat {
            answer3 <- readline("Repeats: no figs./WITH FIGS./pausing (n/f-/p): ")
            if (answer3 == "") {
              answer3 <- "f"
            }
            if (answer3 %in% c("n", "f", "p")) {
              break()
            } else {
              cat("Answer not recognized, please try again...")
            }
          }
          acq.pausing.always <- answer3 == "p"
          clear.display <- show.figs && answer3 == "n"
          show.figs <- answer3 %in% c("p", "f")
          if (acq.pausing.always) {
            message("Pausing between repeats")
          } else {
            message("Not pausing between repeats")
          }
          reuse.old.refs <- TRUE
          reuse.seq.settings <- pending.repeats > 1L
          sequential.naming.required <- pending.repeats > 1L
          answer2 <- "m"
        } else {
          acq.pausing.always <- FALSE
          reuse.old.refs <- FALSE
          reuse.seq.settings <- FALSE
          sequential.naming.required <- FALSE
          pending.repeats <- 1
        }

        if (answer2 == "q") {
          break() # out of UI main loop
        } else if (!reuse.old.refs) {
          repeat {
            answer5 <- readline("Change protocol? yes/NO (y/n-): ")[1]
            answer5 <- ifelse(answer5 == "", "n", answer5)
            if (answer5 %in% c("n", "y")) {
              break()
            } else {
              print("Answer not recognized, please try again...")
            }
          }
          if (answer5 == "y") {
            protocol <- protocol_interactive(protocols)
          }
        }

      }

    } # end of main UI loop

    # Wait for all files to be saved (needed? but anyway reassuring)
    if (async.saves && (mirai::unresolved(rda.mirai) || mirai::unresolved(pdf.mirai))) {
      cat("Saving files ")
      while (mirai::unresolved(rda.mirai) || mirai::unresolved(pdf.mirai)) {
        cat(".")
        Sys.sleep(0.25)
      }
      cat("\n")
    }

    # report on asynchronous file saving
    # (if and only if persistent daemons are created!)
    # if (async.saves && mirai::status()$connections) {
    #   print(mirai::status()$daemons)
    # }

    # save list of all file saved during session
    save(file.names,
         file = paste("files4session-",
                      make.names(session.name),
                      ".Rda", sep = ""))

    cat("Files saved during this session to folder:\n",
        getwd(), "\n",
        paste(file.names, collapse = ",\n"), ".\n", sep = "")

    cat("Ending data acquisition session ...\n")

    # connection to spectrometer is closed using on.exit() to ensure
    # disconnection even when end of session is forced by error or by user

  }

#' Summarize spectral transmittance
#'
#' Compute transmittance by waveband of interest to plants' and human visual
#' responses to light.
#'
#' @param mspct A filter_mspct, or a filter_spct object containing spectral
#'    transmittance for one or more sources.
#' @param quantity character Passed to \code{\link[photobiology]{transmittance}}.
#' @param attr2tb character Vector with one or more of "when.measured",
#'    "what.measured", "where.measured", "how.measured" and "comment".
#' @param summary.type character One of "plant", "PAR" or "VIS".
#' @param digits integer The number of significant digits in the output.
#'
#' @details This function calls different functions from package 'photobiology'
#'    and returns a typical set of summaries.
#'
#' @return A tibble with one row per spectrum and one column per
#'    summary quantity and attribute and a column with the names of the spectra.
#'
#' @export
#'
#' @seealso See the documentation for functions
#'   \code{\link[photobiology]{transmittance}},
#'   \code{\link[photobiology]{add_attr2tb}}
#'   and \code{\link[base]{signif}} which are called to build
#'   the summary table.
#'
#' @examples
#'
#' Tfr_summary_table(yellow_gel.spct)
#' Tfr_summary_table(yellow_gel.spct, attr2tb = c("what.measured", "where.measured"))
#' Tfr_summary_table(yellow_gel.spct, summary.type = "plant")
#' Tfr_summary_table(yellow_gel.spct, summary.type = "PAR")
#' Tfr_summary_table(yellow_gel.spct, summary.type = "VIS")
#'
Tfr_summary_table <-
  function(mspct,
           quantity = "average",
           attr2tb = "when.measured",
           summary.type = "VIS",
           digits = 3L) {

    # handle also single spectra
    if (is.generic_spct(mspct)) {
      mspct <- generic_mspct(list(mspct), class = class(mspct)[1])
    }
    if (any(unname(sapply(mspct, getMultipleWl)) > 1)) {
      mspct <- subset2mspct(mspct)
    }

    if (summary.type %in% c("plant", "PAR")) {
      plant.wb <- switch(summary.type,
                         PAR = c(photobiologyWavebands::UV_bands("CIE"),
                                 list(photobiologyWavebands::PAR())),
                         plant = c(photobiologyWavebands::Plant_bands(),
                                   list(photobiologyWavebands::PAR())))
      summary.tb <-
        photobiology::transmittance(mspct,
                                    quantity = quantity,
                                    w.band = plant.wb,
                                    attr2tb = attr2tb)
    } else if (summary.type == "VIS") {
      summary.tb <-
        photobiology::transmittance(mspct,
                                    quantity  = quantity,
                                    w.band = photobiologyWavebands::VIS_bands(),
                                    attr2tb = attr2tb)
    } else { # total
      summary.tb <-
        photobiology::transmittance(mspct,
                                    quantity = quantity,
                                    w.band = NULL,
                                    attr2tb = attr2tb)
    }

    selector <- unname(sapply(summary.tb, is.numeric))
    summary.tb[ , selector] <- signif(summary.tb[ , selector], digits = digits)

    summary.tb
  }

