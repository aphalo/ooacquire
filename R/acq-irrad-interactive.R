#' Acquire spectral irradiance or spectral fluence
#'
#' Interactive front-end allowing acquisition of spectral irradiance and
#' spectral fluence using Ocean Optics spectrometers. Output of spectral data in
#' R data files stored in objects suitable for use with packages 'photobiology'
#' and 'ggspectra' as well as plots as PDF files and summaries as comma
#' separated files and R objects.
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
#' @param folder.name,session.name,user.name character Default name of the
#'   folder used for output, and session and user names.
#' @param verbose logical If TRUE additional messages are emitted, including
#'   report on memory usage.
#' @param QC.enabled logical If FALSE return NA skipping QC.
#'
#' @details  Function \code{acq_irrad_interactive()} supports measurement of
#'   spectral irradiance from continuous light sources and spectral fluence
#'   from discontinuous ones. For spectral irradiance it assumes that the
#'   duration of the measurement event is the relevant time base for expression
#'   of the flux of radiation. For spectral fluence the flux of radiation is
#'   expressed per pulse of illumination. The acquisition of both individual
#'   spectra and time series of spectra are supported.
#'
#'   A tutorial guiding on the use of this function, illustrated with diagrams,
#'   is available at \url{https://www.r4photobiology.info/pages/acq-irrad-tutorial.html}.
#'   A summary is provided below.
#'
#'   Different arguments passed to \code{interface.mode} modify which aspects
#'   of the user interface are available through menues, without altering
#'   the ability to control the behaviour through arguments passed to formal
#'   parameters when calling the function (see section Interface Modes for
#'   details).
#'
#'   This function can acquire spectra using different protocols for
#'   acquisition and stray light and dark corrections. The protocols are
#'   described in the vignettes and in the help for the low-level functions
#'   called by this function, also from this same package.
#'
#'   Opening the connection to the spectrometer and selection of the channel,
#'   when relevant, is done from within this function.
#'
#'   The irradiance calibration is retrieved from the spectrometer memory
#'   as a last resource if not supplied in any other way. Given that the factors
#'   are stored by Ocean Optics in a format that ignores the entrance optics,
#'   either the effective cosine diffuser area in xxx should be passed to
#'   parameter \code{area} or a character string with the type of the diffuser
#'   passed to \code{diff.type}. If no irradiance calibration is available,
#'   counts per second (cps) or raw counts are the only options available for
#'   the returned spectral data.
#'
#'   Three main protocols and two variations are available by default. They
#'   differ in the additional measurements done to correct for stray light and
#'   dark noise and in the sequence in which they are acquired. In most
#'   situations, at least one of the default protocols is suitable.
#'
#'   In the case of spectral irradiance, the default is to set the integration
#'   time automatically so that the number of counts at the highest peak is
#'   close to 1 - \code{target.margin} times the maximum raw-counts of the
#'   instrument detector (retrieved from the calibration or the instrument
#'   memory). The minimum \code{tot.time} is obtained by increasing the number
#'   of scans. The maximum integration time supported by the spectrometer cannot
#'   be exceeded but multiple scans can be averaged.
#'
#'   In the case of spectral fluence the default is for the integration time to
#'   be set manually and for a message to be displayed asking for the light
#'   pulse to be manually triggered. It is possible to override the default
#'   function by one that triggers the light source automatically when suitable
#'   hardware is available.
#'
#'   Repeated measurements are converted into physical units immediately after
#'   acquisition and saved to file on disk. Each repeated measurement can be
#'   either a single spectrum or a time series of spectra. To avoid long delays
#'   caused by saving large files, \code{async.saves} can be enabled.
#'
#'   Time series of spectra are acquired as fast as possible, converted into
#'   physical units after the acquisition of all individual raw-counts spectra
#'   and saved as a single \code{cps_spct} or \code{source_spct} in long form.
#'
#'   Time series of light measurements using single "dark" and "filter"
#'   measurements are scheduled by setting four members of the named list
#'   passed as argument to \code{seq.settings}, or interactively through the
#'   user interface.
#'
#'   The \code{initial.delay} is numeric and gives a minimum delay in
#'   seconds before the start of measurements with a default of 0s.
#'
#'   The \code{step.delay} is numeric and gives the length of time between
#'   successive "light" measurements. The value entered by the user is adjusted
#'   based on the estimated duration of individual spectrum acquisition. In most
#'   cases a vector of length one is used as time step lengths in seconds. Any vector
#'   shorter than the number of steps will be extended with \code{rep_len()},
#'   and the values interpreted as the time increment in seconds between the
#'   start of successive measurements. If the length is the same as "num.steps",
#'   and the values are monotonically increasing, they are interpreted as time
#'   offsets from the start of the sequence. Member
#'
#'   The \code{start.boundary} must be set to a character string, wither "none",
#'   or a number of seconds, minutes or hours indicated by a number followed by
#'   S, M, or H (capital letters) the round value of the current time at which
#'   the measurement event will start. For example,\code{1H} indicates that
#'   measurements should be scheduled to start exactly at the hour, and
#'   \code{5M} at the next time the minutes in the current time are divisible by
#'   5.
#'
#'   The \code{num.steps} must be an integer between 1 and an 100000
#'   indicating the number of time points at which spectra should be acquired
#'   for the time series. The maximum value depends on the available memory and
#'   in many computers 5000 spectra is a more realistic limit than 100000.
#'   Applying corrections and applying the calibration are computation intensive,
#'   consequently for long series is wise to set `qty.out = "raw"` to speed up
#'   the measurement session.
#'
#'   Plots are produced with functions from package 'ggspectra' and respect the
#'   defaults for plot annotations set in R options. The options can be easily
#'   set with functions \code{set_annotations_default()},
#'   \code{set_w.band_default()}, \code{photon_as_default()}, and
#'   \code{energy_as_default()}. The ggplots are not saved as 'gg' objects as
#'   they contain redundant copies of the spectral data. They can be easily
#'   recreated using function \code{autoplot()} after attaching package
#'   'ggspectra'.
#'
#'   The screen display of plots can be disabled, as in some cases the delay
#'   introduced by rendering can be a nuisance. Alternatively, the value of
#'   \code{plot.lines.max} can be changes from its default.
#'
#' @section Interface Modes:
#'   Mode \strong{simple} displays a simplified user interface, supporting the
#'   acquisition of individual irradiance spectra. Integration time is adjusted
#'   automatically.
#'
#'   Mode \strong{auto} displays a user interface supporting the acquisition of
#'   individual irradiance spectra and creation of collections of spectra.
#'   Integration time is adjusted automatically.
#'
#'   Mode \strong{manual} displays a user interface supporting the acquisition
#'   of individual fluence or irradiance spectra and creation of collections of
#'   spectra. Integration time can adjusted automatically but also set manually.
#'
#'   Mode \strong{series} displays a user interface supporting the acquisition
#'   of individual spectra and time series of irradiance spectra. Integration
#'   time can adjusted automatically but also set manually.
#'
#'   All these modes with \strong{-attr} appended, enable a menu and dialogues
#'   that make it possible to set the values stored in attributes \code{comment}
#'   and \code{what.measure} interactively.
#'
#'   All modes support repeated measurements with unchanged acquisition settings
#'   reusing the reference spectra ('dark' and 'filter') from the most recent
#'   previous measurement.
#'
#' @section Object names: Object names entered interactively are sanitized if
#'   necessary. Sequentially numbered object names are enabled by appending "#"
#'   to the desired base name. As long as no new name is entered, the sequence
#'   continues. If a new name is entered, numbering restarts at 001 or stops
#'   depending on whether the new name ends in "#" or not. In the case of
#'   repeated measurements, sequential numbering is enforced to ensure unique
#'   names.
#'
#' @section Irradiance calibration:
#'   Calibration data needs in most cases to be imported into R and
#'   parameters entered for the special correction algorithms into a correction
#'   method descriptor. The corrections are skipped if the needed information is
#'   missing. If no spectral irradiance calibration is available and attempt is
#'   made to retrieve it from the spectrometer, but given the format used by
#'   Ocean Optics/Ocean Insight, in this case the effective \code{area} of the
#'   cosine diffuser used (or the model name if from Ocean Optics) should be
#'   supplied by the user.
#'
#' @section Quality control of dark spectra:
#'   Disabling the quality control with \code{QC.enabled = FALSE} is necessary
#'   when the "dark" reference is a measurement of ambient light instead of true
#'   darkness; i.e., when the irradiance of one light source is measured as the
#'   difference between background illumination and background illumination
#'   plus the target light source.
#'
#' @return This function returns the acquired spectra through "side effects" as
#'   each spectrum is saved, both as raw counts data and optionally as spectral
#'   irradiance, spectral fluence or counts-per-second spectral data in an
#'   \code{.rda} (R data) file as objects of the classes defined in package
#'   'photobiology'. Optionally, the plot for each spectrum or a time series of
#'   spectra is saved as a \code{.pdf} file. At any time, the current group of
#'   spectra can be saved as a collection. When a collection is created, all
#'   recently measured spectra are saved together, decreasing the number of
#'   files and keeping related spectra in the same files. Summaries of the
#'   spectra in a collection are additionally saved to a CSV file and a plot of
#'   the collected spectra saved to a \code{.pdf} file.
#'
#'   The value returned by the function is that from closing the connection to
#'   the spectrometer.
#'
#' @note The function is composed in a modular way from functions defined in
#'   this some package, R or imported packages. The code of the function can be
#'   reshuffled combining the functions used here with other functions to create
#'   new variations, possibly better suited to users' needs and tastes.
#'
#'   A "light-weight" approach to tweaking the user interface is to implement
#'   new modes by simply changing which of the logical flags that control the
#'   display of menus are enabled or not. And even easier approach is to create
#'   a simple script that passes suitable arguments to the different formal
#'   parameters.
#'
#' @seealso This function calls functions \code{\link{tune_interactive}},
#'   \code{\link{protocol_interactive}}, \code{\link{set_seq_interactive}} and
#'   \code{\link{set_attributes_interactive}}. If irradiance calibration is
#'   retrieved from the instrument, functions \code{\link{get_oo_descriptor}}
#'   and \code{\link{oo_calib2irrad_mult}} are also called.
#'
#' @family interactive acquisition functions
#'
#' @export
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
  function(tot.time.range = if (qty.out == "fluence") 5 else c(5, 15),
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
           plot.lines.max = 11,
           summary.type = "plant",
           save.pdfs = TRUE,
           save.summaries = !interface.mode %in% c("series", "series-attr"),
           save.collections = !interface.mode %in% c("simple", "series", "series-attr"),
           async.saves = FALSE,
           show.figs = TRUE,
           interface.mode = ifelse(qty.out == "fluence", "manual", "auto"),
           num.exposures = ifelse(qty.out == "fluence", 1L, -1L),
           f.trigger.init = NULL,
           f.trigger.on = f.trigger.message,
           f.trigger.off = NULL,
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

    if (getOption("ooacquire.offline", FALSE)) {
      warning("ooacquire off-line: Aborting...")
      return()
    }

    ## Multiple processes for asynchronous file saving
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

    # set R options
    if (is.null(getOption("digits.secs"))) {
      old.options <- options(warn = 1,
                             "digits.secs" = 3,
                             "ooacquire.qc.enabled" = QC.enabled)
    } else {
      old.options <- options(warn = 1,
                             "ooacquire.qc.enabled" = QC.enabled)
    }
    on.exit(options(old.options), add = TRUE, after = TRUE)

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
        stop("Aborting! Requested protocol is not available.", call. = FALSE)
      }
    }

    # connect to spectrometer
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
        " from spectrometer with serial number: ", serial_no, "\n", sep = "")

    # if multiple entrance optics are available, each has a different calibration
    # we validate the argument passed in the case of on spectrometer
    if (serial_no == "MAYP11278") {
      if (is.null(entrance.optics) || grepl("^cos", entrance.optics) ) {
        entrance.optics <- "cosine"
      } else if (entrance.optics == "dome" || grepl("^hemis", entrance.optics)) {
        entrance.optics <- "hemispherical"
      } else {
        warning("Aborting! 'entrance.optics' must be \"cosine\" or \"hemispherical\"", call. = FALSE)
        return(NULL)
      }
      cat("Entrance optics \"", entrance.optics, "\" selected\n", sep = "")
      answer.entrance <- readline("Is this correct? YES/no (y-/n)")
      if (!answer.entrance %in% c("", "y")) {
        warning("Aborting! 'entrance.optics' selection not validated by user!", call. = FALSE)
        return(NULL)
      }
    }

    # spectrometer-specific correction method parameters
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

      # default protocols depend of implemented correction methods
      available.protocols <- names(protocols)
      default.protocol <-
        switch(serial_no,
               MAYP11278 = ifelse("lfd" %in% available.protocols, "lfd", available.protocols[1]),
               MAYP112785 = ifelse("lfd" %in% available.protocols, "lfd", available.protocols[1]),
               MAYP114590 = ifelse("lfd" %in% available.protocols, "lfd", available.protocols[1]),
               FLMS04133 = ifelse("ld" %in% available.protocols, "ld", available.protocols[1]),
               FLMS00673 = ifelse("ld" %in% available.protocols, "ld", available.protocols[1]),
               FLMS00440 = ifelse("ld" %in% available.protocols, "ld", available.protocols[1]),
               FLMS00416 = ifelse("ld" %in% available.protocols, "ld", available.protocols[1]),
               ifelse("ld" %in% available.protocols, "ld", available.protocols[1])
        )

    } else {
      descriptor <- which_descriptor(descriptors = descriptors)
      stopifnot(exists("spectrometer.name", descriptor))
      default.protocol <- ifelse("ld" %in% available.protocols, "ld", available.protocols[1])
    }

    # jwrapper and spectrometer indexes have to be set to current ones if
    # descriptor was not acquired from the spectrometer in the current session
    descriptor[["w"]] <- w
    descriptor[["sr.index"]] <- sr.index
    descriptor[["ch.index"]] <- ch.index

    if (length(descriptor) < 10 || length(correction.method) < 5) {
      stop("No spectrometer data found")
    }

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

    # check for valid calibration multipliers
    if (length(descriptor[["inst.calib"]][["irrad.mult"]]) != descriptor[["num.pixs"]] ||
        anyNA(descriptor[["inst.calib"]][["irrad.mult"]])) {
      if (qty.out %in% c("irrad", "fluence")) {
        warning("Bad calibration data, returning counts-per-second.",
                call. = FALSE)
        qty.out = "cps"
      }
    }

    # session and user IDs
    session.name <- set_session_name_interactive(session.name)
    user.name <- set_user_name_interactive(user.name)
    session.label <- paste("Operator: ", user.name,
                           "\nSession: ", session.name,
                           ", instrument s.n.: ", descriptor[["spectrometer.sn"]],
                           sep = "")

    # set default for metadata attributes
    user.attrs <-
      list(what.measured = "",
           comment.text = "",
           how.measured = paste("Acquired with ", descriptor[["spectrometer.name"]],
                                " (", descriptor[["spectrometer.sn"]],
                                "), with a ", descriptor[["entrance.optics"]][["geometry"]], " diffuser",
                                "\nR (", paste(R.version[["major"]], R.version[["minor"]], sep = "."),
                                "), 'ooacquire' (", utils::packageVersion("ooacquire"),
                                ") in mode \"", interface.mode,
                                "\", 'rOmniDriver' (", utils::packageVersion("rOmniDriver"),
                                ") and OmniDriver (", rOmniDriver::get_api_version(w), ").",
                                sep = ""))

    # set working directory for current session
    folder.name <- set_folder_interactive(folder.name)
    oldwd <- setwd(folder.name)
    on.exit(setwd(oldwd), add = TRUE)
    on.exit(message("Folder reset to: ", getwd(), "\nBye!"), add = TRUE)

    # ask user to choose protocol only if needed
    if (length(protocols) > 1) {
      protocol <- protocol_interactive(protocols = protocols,
                                       default = default.protocol)
    } else {
      protocol <- protocols[[default.protocol]]
    }

    # set default data acquisition settings based of call arguments
    start.int.time <- 0.01 # seconds
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
    irrad.names <- character()
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
          # increase width of seq numbers if needed
          seq.name.digits <- max(seq.name.digits, ceiling(log10(file.counter + 1)))
          obj.name <- paste(base.obj.name,
                            formatC(file.counter, width = seq.name.digits, flag = "0"),
                            sep = "")
        } else {
          obj.name <- base.obj.name
        }

        # generate object names from base name

        irrad.name <- paste(obj.name, "spct", sep = ".")
        raw.name <- paste(obj.name, "raw_mspct", sep = ".")
        file.name <- paste(obj.name, "spct.Rda", sep = ".")

        # although base name is known valid, the name may be already in use
        if ((irrad.name %in% irrad.names) || file.exists(file.name)) {
          if (sequential.naming && reuse.seq.settings) {
            # likely running unattended
            cat("Overwriting existing: '", irrad.name, "'.", sep = "")
            irrad.names <- setdiff(irrad.names, irrad.name)
            raw.names <- setdiff(raw.names, raw.name)
            break()
          } else {
            # operator likely present
            if (readline(paste("Overwrite existing '", irrad.name, "? (y/n-): "))[1] == "y") {
              irrad.names <- setdiff(irrad.names, irrad.name)
              raw.names <- setdiff(raw.names, raw.name)
              break()
            }
          }
        } else {
          break()
        }
        print("A valid and unique name is required. Please try again...")

      } # obtain a valid object name from user

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
            sum(2 * settings$integ.time * 1e-6) # worse case overhead due to restart
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
            signif(estimated.measurement.duration, 3), " s.\n", sep = "")

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

      if (!is.null(f.trigger.init)) {
        f.trigger.init()
      }
      # acquire raw-counts spectra
      if (reuse.old.refs) { # acquire only light spectra
        if (acq.pausing) {
          raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                     acq.settings = settings,
                                     seq.settings = seq.settings,
                                     protocol = "light",
                                     pause.fun = NULL,
                                     f.trigger.on = f.trigger.on,
                                     f.trigger.off = f.trigger.off,
                                     user.label = obj.name)
          if (pending.repeats > 1) {
            answer.abort <- readline(prompt = "Skip pending repeats? yes/NO (y/n-): ")
            if (answer.abort %in% c("y", "z")) {
              pending.repeats <- 1
            }
          }
        } else {
          if (pending.repeats == total.repeats) {
            readline(paste("Acquire LIGHT reading(s): g = GO (g-):"))[1]
          }
          raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                     acq.settings = settings,
                                     seq.settings = seq.settings,
                                     protocol = "light",
                                     pause.fun = function(...) {TRUE},
                                     f.trigger.on = f.trigger.on,
                                     f.trigger.off = f.trigger.off,
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
        refs.selector <- grep("dark|filter", protocol, value = TRUE)
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
        # for series measurements we can have multiple "light" raw spectra
        spct.names <-
          list(light = grep("^light", names(raw.mspct), value = TRUE),
               filter = "filter",
               dark = "dark")

        if (length(raw.mspct) > 10L) {
          cat("Computing ", qty.out, " ... ", sep = "")
        }

        irrad.spct <-
          s_irrad_corrected(x = raw.mspct,
                            spct.names = spct.names,
                            correction.method = correction.method,
                            hdr.tolerance = getOption("ooacquire.hdr.tolerance",
                                                      default = 0.05),
                            return.cps = qty.out == "cps")

        cat('Adding metadata ... ')
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

        if (length(raw.mspct) > 10L) {
          cat("ready.\n")
        }

        # prepare plot invariants
        if (plot.lines.max < getMultipleWl(irrad.spct)) {
          title.text <- paste(what_measured(irrad.spct)[[1L]],
                              " (n = ", plot.lines.max,
                              "/", getMultipleWl(irrad.spct),
                              ")",
                              sep = "")
          plot.spct <- pull_sample(irrad.spct, size = plot.lines.max)
        } else {
          title.text <- paste(what_measured(irrad.spct)[[1L]],
                              " (n = ", getMultipleWl(irrad.spct), ")",
                              sep = "")
          plot.spct <- irrad.spct
        }

        # display plot, allowing user to tweak it
        repeat {
          if (length(raw.mspct) > 10L) {
            cat("Building plot ... ")
          }
          fig <- ggplot2::autoplot(plot.spct,
                                   annotations = c("-", "colour.guide"),
                                   geom = ifelse(getMultipleWl(irrad.spct) == 1,
                                                 "spct", "line")) +
            ggplot2::labs(title = title.text,
                          subtitle = when_measured(irrad.spct)[[1L]],
                          caption = how_measured(irrad.spct)[[1L]]) +
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
            if(qty.out == "cps") {
              plot.prompt <- "fig/w.bands/discard+go/SAVE+GO (f/w/d/s-): "
              valid.answers <-  c("f","w", "d", "s", "g")
            } else {
              plot.prompt <- "fig/photons/energy/w.bands/discard+go/SAVE+GO (f/p/e/w/d/s-): "
              valid.answers <- c("f","p", "e", "w", "d", "s", "g")
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
                   f = {clear.display <- show.figs; show.figs <- !show.figs; next()},
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
                   d = break() # exit loop early, discarding acquired data
            )
          }

          # update lists of objects and files created during current session
          # since start or most recent saving of a collection.
          raw.names <- c(raw.names, raw.name)
          file.names <- c(file.names, file.name)
          irrad.names <- c(irrad.names, irrad.name)

          # "rename" temporary objects
          assign(raw.name, raw.mspct)
          assign(irrad.name, irrad.spct)
          obj.names <- c(raw.name, irrad.name)

          # save objects to files on disk
          if (async.saves && !mirai::unresolved(rda.mirai)) {
            # non-blocking
            cat("Saving files asynchronously ...\n")
            rda.mirai <-
              mirai::mirai({
                assign(obj.names[1], raw.mspct)
                assign(obj.names[2], irrad.spct)
                save(list = obj.names, file = file.name)
                return(file.exists(file.name))
              },
              obj.names = obj.names,
              file.name = file.name,
              raw.mspct = raw.mspct,
              irrad.spct = irrad.spct,
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

          # name of the Rda file used to save the collection
          collection.file.name <- paste(collection.name, "Rda", sep = ".")
          # collection.objects used to collect all R objects to be saved to the Rda file
          collection.objects <- character()

          if (qty.out != "raw") {
            # construct collection of computed spectra, needed for summaries
            # even when not saved to file on disk!
            collection.mspct <-
              switch(qty.out,
                     fluence = photobiology::source_mspct(mget(irrad.names)),
                     irrad = photobiology::source_mspct(mget(irrad.names)),
                     cps =   photobiology::cps_mspct(mget(irrad.names)))

            # plot collection and summaries
            if (plot.lines.max < getMultipleWl(irrad.spct)) {
              collection.title <- paste(collection.title,
                                        " (sample of ", plot.lines.max, ")",
                                        sep = "")
            } else {
              collection.title <- paste(collection.title,
                                        " (n = ", getMultipleWl(irrad.spct), ")",
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
              irrad.collection.name <- paste(collection.name, qty.out, "mspct", sep = ".")
              assign(irrad.collection.name, collection.mspct)
              collection.objects <- c(collection.objects, irrad.collection.name)

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
                  irrad.names <- character()
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
          acq.pausing <- acq.pausing.always
          clear.display <- show.figs && answer3 == "n"
          show.figs <- answer3 %in% c("p", "f")
          if (acq.pausing.always) {
            cat("Pausing between repeats\n")
          } else {
            cat("Not pausing between repeats\n")
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

    # Wait for all files to be saved (needed? but anyway a  reassuring)
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
#' # temporary kludge until fixed in photobiologyInOut
#' # irrad_summary_table(sun.spct, summary.type = "VIS", unit.out = "energy")
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
