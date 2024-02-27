#' Interactively adjust the integration time settings
#'
#' Adjust integration time settings, allowing the user to repeat the tuning,
#' and to change some of the parameters used for tuning such as total
#' compound integration time and integration time bracketing.
#'
#' @param descriptor list Descriptor of the instrument, including wrapper to
#'   Java object used to access the instrument.
#' @param acq.settings list containing starting values for instrument settings.
#' @param start.int.time numeric Integration time to use as starting guess when
#'   tuning the settings.
#' @param interface.mode character One of "simple", "auto", or "manual".
#'
#' @details
#' This function implements three different user interfaces: 1) "simple" is an
#' interface suitable for the most usual measurements using automatic tuning
#' of integration time and hides some of the
#' less frequently used options, 2) "auto" gives access to all available
#' options offering maximum flexibility when using automatic tuning of
#' integration time, and 3) "manual" supports use of fixed integration times
#' directly entered by the user.
#'
#' Tuning of the integration time takes into account the range of times
#' supported by the connected instrument, read from the instrument descriptor.
#' The algorithm also makes use of the linearisation function when extrapolating
#' to guess the integration time needed. Initial (default) values are read from
#' \code{acq.settings} while \code{start.int.time} provides a default starting
#' value for integration time for tuning when the user chooses not to use the
#' value stored in \code{acq.settings}.
#'
#' @family interactive acquisition utility functions
#'
#' @export
#'
tune_interactive <- function(descriptor,
                             acq.settings,
                             start.int.time = 0.1,
                             interface.mode = "auto") {
  if (!interface.mode %in% c("simple", "auto", "manual", "full")) {
    interface.mode <- "auto" # gets used for "series"
  }
  # configure interface for active mode
  prompt.text1 <-
    switch(interface.mode,
           simple = "RETUNE/range/HDR/undo/help/go (t-/r/h/u/?/g): ",
           auto = "fixed/RETUNE/tune/sat/range/HDR/undo/help/go (f/t-/T/s/r/h/u/?/g): ",
           manual = "FIXED/range/HDR/undo/help/go (f-/r/h/u/?/m): ",
           full = "FIXED/retune/tune/sat/range/HDR/undo/help/go (f-/t/T/s/r/h/u/?/g): "
    )
  prompt.text2 <-
    switch(interface.mode,
           simple = "retune/range/HDR/undo/help/GO (t/r/h/u/?/g-): ",
           auto = "fixed/retune/tune/sat/range/HDR/undo/help/GO (f/t/T/s/r/h/u/?/g-): ",
           manual = "fixed/range/HDR/undo/help/GO (f/r/h/u/?/g-): ",
           full = "fixed/retune/tune/sat/range/HDR/undo/help/GO (f/t/T/s/r/h/u/?/g-): "
    )
  valid.input <-
    switch(interface.mode,
           simple = c("t", "r", "h", "u", "?", "m", "g", ""),
           auto = c("f", "t", "T", "s", "r", "h", "u", "?", "m", "g", ""),
           manual = c("f", "r", "h", "u", "?", "m", "g", ""),
           full = c("f", "t", "T", "s", "r", "h", "u", "?", "m", "g", "")
    )
  default.input <-
    switch(interface.mode,
           simple = c("t", "m"),
           auto = c("t", "m"),
           manual = c("f", "m"),
           full = c("f", "m")
    )
  # set help
  all.help <- c(f = "f = fixed. User-suplied \"base\" integration time in seconds.",
                t = "t = retune. Adjust integration time starting from last value.",
                T = "T = tune. Adjust integration time starting from default value.",
                s = "s = saturation margin. Tuned integration time is maximum * (1 - margin).",
                r = "r = range. Total measurement time in seconds, as a single value or a range.",
                h = "h = HDR multipliers. High dynamic range or bracketing, as multipliers for target integration time.",
                u = "u = undo. Restore settings from last measurement.",
                H = "? = help. Show this help text.",
                m = "g = go. Go ahead to next step, without setting/tuning integration time.",
                default = "- = default. Action selected by pressing \"Enter\" key.")
  help.text <- paste(all.help[setdiff(valid.input, "")], collapse = "\n")
  # common code to all modes
  old.settings <- acq.settings # allow starting over
  tuned <- FALSE
  repeat{
    acq.settings.string <-
      sprintf("Acq: %s = %.3gS, saturation = %.2f, tot. range = %.3g-%.3gS, HDR mult. = %s\n",
              ifelse(tuned, "integ(OK)", "integ(??)"),
              acq.settings[["integ.time"]][1] * 1e-6,
              acq.settings[["target.margin"]],
              min(acq.settings[["tot.time.range"]]) * 1e-6,
              max(acq.settings[["tot.time.range"]]) * 1e-6,
              paste(as.character(acq.settings[["HDR.mult"]]), collapse = ","))
    cat(acq.settings.string)
    if (tuned) {
      prompt.text <- prompt.text2
    } else {
      prompt.text <- prompt.text1
    }
    repeat {
      answ <- readline(prompt.text)[1]
      answ <- substr(answ, 1, 1)
      if (answ %in% valid.input) {
        break()
      }
      cat("Unrecognized input: '", answ, "'. Please, try again...\n", sep = "")
    }
    if (answ == "") {
      answ <- ifelse(!tuned, default.input[1], default.input[2])
    }
    if (answ %in% c("m", "g")) {
      break()                       ## <- exit point for loop
    }

    if (answ == "?") {
      cat(help.text)
    } else if (answ %in% c("t", "T")) {
      answ.t <- readline("Auto-adjust integration time? ADJUST/current/abort (a-/c/z): ")
      if (answ.t %in% c("z", "c")) {
        if (answ.t == "c") {
          tuned <- TRUE
        }
        next()
      } else if (answ.t %in% c("a", "")) {
      if (answ == "T") {
        acq.settings[["integ.time"]] <- start.int.time * 1e6
      }
      acq.settings <- tune_acq_settings(descriptor = descriptor,
                                        acq.settings = acq.settings)
      tuned <- TRUE
      }
    } else if (answ == "f") {
      user.integ.time <- read_seconds("Integration time(s) (seconds): ", n.max = 4)
      user.integ.time <- user.integ.time * 1e6 # seconds -> microseconds
      if (length(user.integ.time) >= 1) {
        num.integ.times <- length(unique(user.integ.time))
        if (num.integ.times > 1) {
          message("Resetting HDR multipliers to 1")
          acq.settings[["HDR.mult"]] <- rep(1, num.integ.times)
          user.integ.time <- unique(sort(user.integ.time))
          acq.settings[["num.exposures"]] <-
            rep_len(acq.settings[["num.exposures"]], num.integ.times)
        }

        acq.settings <- set_integ_time(acq.settings = acq.settings,
                                       integ.time = user.integ.time)
        tuned <- TRUE
      } else {
        cat("Value not changed!\n")
      }
    } else if (answ == "s") {
      margin <- readline("Saturation safety margin [0..1]: ")
      margin <- try(as.numeric(margin))[1]
      if (!is.na(margin) && margin >= 0 && margin < 1) {
        acq.settings[["target.margin"]] <- margin
        tuned <- FALSE
      } else {
        cat("Request ignored: value not in 0..1 range\n")
      }
    } else if (answ == "r") {
      tot.time.range <-
        read_seconds("Total time range (seconds), 1 or 2 numbers: ", n.max = 2)
      tot.time.range <- round(tot.time.range * 1e6) # integer microseconds
      if (length(tot.time.range) >= 1) {
        tot.time.range <- range(tot.time.range)
        # ensure that the the range includes a finite positive value
        if (tot.time.range[1] >= 0 && tot.time.range[2] > 0) {
          acq.settings[["tot.time.range"]] <- tot.time.range
          tuned <- FALSE
        } else {
          cat("Request ignored: entered value < 0!\n")
        }
      } else {
        cat("Value not changed!\n")
      }
    }  else if (answ == "h") {
      old.hdr.mult.len <- length(acq.settings[["HDR.mult"]])
      HDR.mult <- read_numbers("HDR multipliers, 1 to 4 numbers: ", n.max = 4)
      HDR.mult <- sort(HDR.mult)
      if (length(HDR.mult) >= 1 &&
          HDR.mult[1] >= 0  && HDR.mult[length(HDR.mult)] < 1000) {
        acq.settings[["HDR.mult"]] <- HDR.mult
        tuned <- FALSE
        if (length(HDR.mult) != old.hdr.mult.len) {
          # ensure 'num.exposures' matches 'HDR.mult' in length
          num.exposures <- acq.settings[["num.exposures"]]
          if (length(num.exposures) < length(HDR.mult)) {
            acq.settings[["num.exposures"]] <- rep(acq.settings[["num.exposures"]][1], length(HDR.mult))
            if (!all(num.exposures == -1L)) {
              warning("Resetting 'num.exposures'")
            }
          } else if (length(num.exposures) > length(HDR.mult)) {
            acq.settings[["num.exposures"]] <- num.exposures[1:length(HDR.mult)]
          }
        }
      } else {
        cat("Bad multipliers ignored. Value not changed! Please, try again...\n")
      }
    } else if (answ == "u") {
      cat("Restoring previous settings!\n")
      tuned <- FALSE
      acq.settings <- old.settings
    }
  }
  acq.settings
}

#' Interactively select a measurement protocol
#'
#' Choose a protocol by name from a list of protocols, allowing the user to
#' correct the selection if needed. Protocols are not hard-wired, but instead
#' defined by the list passed as argument to \code{protocols}.
#'
#' @details
#' A protocol is defined as a named list of character strings, and consist in
#' multiple acquisition of spectra contributing to the same logical measurement.
#' The name of the list member is the name of the protocol, while the members
#' of each character vector correspond to spectra to be acquired, after some
#' change in the measurement conditions. For example the list
#' \code{list(rsd = c("reference", "sample", "dark"), rs = c("reference", "sample"))}
#' defines two protocols.
#'
#' @family interactive acquisition utility functions
#'
#' @param protocols named list Measuring protocol defifinitions and names.
#' @param default character Name of the default protocol.
#'
#' @return The member vector corresponding to the protocol selected by
#' the user.
#'
#' @export
#'
protocol_interactive <- function(protocols,
                                 default = names(protocols)[[1]]) {
  stopifnot(length(protocols) > 0L)
  if (length(protocols) == 1) {
    protocol <- protocols[[1]]
    cat("Will use protocol '",
        paste(protocol, collapse = " -> "),
        "' (no choice).\n", sep = "")
  } else {
    if (! default %in% names(protocols)) {
      # if default is invalid fall back to first member of list
      default <- names(protocols)[[1]]
    }
    protocol.names <- names(protocols)
    protocol.names <- ifelse(protocol.names == default,
                             paste(protocol.names, "-", sep = ""),
                             protocol.names)
    prompt <- paste("Protocols; light, filter, dark (",
                    paste(protocol.names, collapse = "/"),
                    "): ", sep = "")
    repeat{
      user.input <- tolower(readline(prompt = prompt))
      if (user.input == "") {
        user.input <- default
      }
      if (user.input %in% names(protocols)) {
        protocol <- protocols[[user.input[1]]]
        if (readline(paste("Will use protocol '",
                           paste(protocol, collapse = " -> "),
                           "', o.k.? (-/n): ", sep = "")) != "n") {
          break()
        }
      } else {
        cat("Protocol '", user.input, "' unknown!\n")
      }
    }
  }
  protocol
}

#' Get list of connected instruments
#'
#' Get list of spectrometers requesting user to connect one or abort if none
#' found.
#'
#' @param w handle to Omni Driver, used to test if an spectrometer is still
#'   connected.
#'
#' @family interactive acquisition utility functions
#'
#' @export
#'
list_srs_interactive <- function(w) {
  if (is.null(w)) return(list())
  while (rOmniDriver::number_srs(w) < 1L) {
    answ <- readline("Connect spectrometer to USB port. <enter> = try again, z = abort (-/z): ")
    if (answ[1] %in% c("z", "Z")) {
      break()
    }
  }
  list_instruments(w)
}

#' Interactively select an instrument
#'
#' Choice of spectrometer from a list of serial numbers, allowing the
#' user to correct the selection if needed.
#'
#' @param instruments the returm value of \code{list_instruments(w)}.
#'
#' @return A numeric index suitable for use in calls to functions defined in
#' package 'rOmniDriver' based on which package 'ooacquire' is coded.
#'
#' @family interactive acquisition utility functions
#'
#' @export
#'
choose_sr_interactive <- function(instruments) {
  # if instruments had names this would not be needed
  sn.idx <- 3

  # make sure at least one instrument is connected
  stopifnot(nrow(instruments) > 0L)

  num.inst <- nrow(instruments)
  if (num.inst >= 1) {
    cat("Found", num.inst, "spectrometer(s)")
  } else {
    cat("No spectrometers found.\n")
    return(-1L)
  }

  # select instrument
  if (num.inst > 1) {
    prompt <- paste(1:nrow(instruments), ": ", instruments[[sn.idx]],
                    " (choose by index): ", sep = "")
    repeat{
      answ <- as.integer(readline(prompt = prompt))
      if (is.na(answ[1])) {
        sr.idx <- 1L
      }
      if (sr.idx[1] %in% 1L:nrow(instruments)) {
        sr.idx <- sr.idx[1]
        break()
      } else {
        cat("A number between 1 and ", num.inst, " is required.\n", sep = "")
      }
    }
  } else { # num.inst == 1
    sr.idx <- 1L
  }
  cat("selected:\n")
  print(instruments[sr.idx, ])
  sr.idx - 1L # use Omni Driver convention for indexes
}

#' Interactively select a channel
#'
#' Choice of channel to be used if spectrometer has more than one channel.
#'
#' @param instruments the returm value of \code{list_instruments(w)}
#' @param sr.index integer The index to the spectrometer, starting from zero,
#'   following Omni Driver indexing conventions.
#' @param prompt.text character string to use as prompt.
#'
#' @return A numeric index suitable for use in calls to functions defined in
#' package 'rOmniDriver' based on which package 'ooacquire' is coded.
#'
#' @family interactive acquisition utility functions
#'
#' @export
#'
choose_ch_interactive <- function(instruments,
                                  sr.index = 0L,
                                  prompt.text = "Channels available: ") {
  stopifnot(nrow(instruments) > 0)

  num.ch.idx <- 4

  num.channels <- instruments[sr.index + 1L, num.ch.idx]
  if (num.channels > 1) {
    repeat {
      prompt <- paste(prompt.text, paste(format(1:num.channels, digits = 0L), collapse = ", "),
                      " (choose by index): ", sep = "")
      ch.idx <- as.integer(readline(prompt = prompt))
      if (ch.idx %in% 1:num.channels) {
        ch.index <- ch.idx
        break()
      } else {
        cat("A number between 1 and ", num.channels, " is required.\n", sep = "")
      }
    }
  } else {
    ch.index <- 1L
  }
  ch.index - 1L # use Omni Driver convention for indexes
}

#' Interactively set sequential measurements
#'
#' Enter settings defining a sequence of spectra to be measured as a time
#' series.
#'
#' @param seq.settings numeric Definition of time steps for a sequence of
#'   repeated measurements. Named vector with member named
#'   \code{start.boundary}, \code{"initial.delay"}, \code{"step.delay"}, and
#'   \code{"num.steps"}.
#' @param measurement.duration numeric Duration of one measurement event (s).
#' @param minimum.step.delay numeric Minimum duration of \code{"step.delay"} (s).
#' @param time.division numeric The step is forced to be a multiple of this
#'   time duration, because spectrometers normally are constantly acquiring
#'   spectra and thbey return the most recently acquired one. Should be set to
#'   the integration time plus a very small overhead (s).
#'
#' @details Function \code{seq.settings()} allows users to enter values needed
#' to define a sequence of spectral acquisitions. These are the time unit
#' boundary to synchronize to, the delay or duration of the time
#' step between successive acquisitions and the number of acquisitions in the
#' series. The \code{measurement.time} determines the minimum length for
#' \code{"step"}.
#'
#' A sequence of measurements are expected to share a single reference or
#' dark scan, and be done as a sequence. With a single time point, the initial
#' delay or time unit boundary can be used to schedule a single timed
#' measurement.
#'
#' @family interactive acquisition utility functions
#'
#' @return A named numeric vector of length two.
#'
#' @export
#'
set_seq_interactive <- function(seq.settings = list(start.boundary = "second",
                                                    initial.delay = 0,
                                                    step.delay = 0,
                                                    num.steps = 1),
                                measurement.duration = 0,
                                minimum.step.delay = measurement.duration,
                                time.division = 0) {

  # ensure that step.delay is a multiple of time.division (integration time)
  check.step.delay <- function(step, time.div) {
    if (time.division > 0) {
      num.time.divs <- (step + 1e-7) %/% time.division # protect from loss of precision
      if (num.time.divs < 1) {
        step <- 0
      } else {
        step <- time.division * num.time.divs
      }
    }
    step
  }

  # validate input
  if (!setequal(names(seq.settings),
      c("start.boundary", "initial.delay", "step.delay", "num.steps"))) {
    warning("Resetting invalid 'seq.settings' to defaults.")
    seq.settings <- list(start.boundary = "second",
                         initial.delay = 0,
                         step.delay = 0,
                         num.steps = 1)
  }

  seq.settings$step.delay <-
    check.step.delay(seq.settings$step.delay, time.division)

  # support undo
  old.seq.settings <- seq.settings

  # set help
  all.help <- c(w = "w = wait. Waiting time before start of acquisition of time series.",
                b = "b = boundary. The time boundary at which to start acquisition of time series.",
                s = "s = step duration. The time step between the start of succesive acquisitions in a time series.",
                r = "r = step repetitions. The number of spectra to measure for the current time series.",
                H = "? = help. Show this help text.",
                m = "m = MEASURE. Measure without setting/tuning integration time.",
                default = "- = default. Action selected by pressing \"Enter\" key.")
  help.text <- paste(all.help, collapse = "\n")

  repeat{
    # ensure step delay is achievable
    if (seq.settings$step.delay < measurement.duration &&
        seq.settings$step.delay != 0) {
      seq.settings$step.delay <- signif(minimum.step.delay * 1.01, 3)
      cat("'step.delay' too short! Reset to ", seq.settings$step.delay, " s.\n", sep = "")
    }
    # display current settings before prompt for user input
    seq.settings.string <-
           sprintf("Seq: wait = %.3gS, boundary = %s, step-len = %.3gS, step-reps = %i \n",
                   seq.settings[["initial.delay"]],
                   seq.settings[["start.boundary"]],
                   seq.settings[["step.delay"]],
                   seq.settings[["num.steps"]])
    cat(seq.settings.string)

    answ <- readline(prompt = "wait/boundary/step-len/step-reps/undo/help/GO (w/b/s/r/u/?/g-): ")

    if (answ %in% c("", "m", "g")) {
      break()
    }
    if (answ == "?") {
      cat(help.text)
    } else if (substr(answ, 1, 1) == "w") {
      step <- read_period("Wait before series start (period): ", n.max = 1)
      # step <- readline(sprintf("Wait = %.3gS, new: ",
      #                          seq.settings[["initial.delay"]]))
      # step <- period(step)
      if (!is.na(step) && length(step)) {
        step <- as.numeric(step) # period to seconds
        seq.settings[["initial.delay"]] <- step
      } else {
        cat("Wait value not changed!\n")
      }
    } else if (substr(answ, 1, 1) == "b") {
      time.boundary <- read_period("Time boundary for start of series (period): ",
                                   n.max = 1,
                                   pass.through = "none")
      if (length(time.boundary) && !is.na(lubridate::period(time.boundary))) {
        if (lubridate::is.period(time.boundary) && as.numeric(time.boundary) == 0) {
          time.boundary <- "none"
        }
        seq.settings[["start.boundary"]] <- time.boundary
      } else {
        cat("Start-boundary Value not changed!\n")
      }
    } else if (substr(answ, 1, 1) == "s") {
      step <- read_period("Time-step length (period): ", n.max = 1)
      if (length(step) && !is.na(step)) {
        step <- as.numeric(step) # period to seconds
        seq.settings$step.delay <-
          check.step.delay(step, time.division)
        if (seq.settings$step.delay != step) {
          cat("Time step = ", signif(seq.settings$step.delay, 3),
              "s, adjusted from ", step, "\n", sep = "")
        }
      } else {
        cat("Time step value not changed!\n")
      }
    } else if (substr(answ, 1, 1) == "r") {
      num.steps <- readline("Number of steps (1..100000): ")
      num.steps <- try(as.integer(num.steps))
      if (is.na(num.steps)) {
        cat("Expected a positive integer. Value not changed!\n")
      } else if (num.steps < 1L || num.steps > 100000L) {
        warning("Expected number within range 1..100000. Value not changed!")
      } else {
        seq.settings[["num.steps"]] <- num.steps
      }
    } else if (substr(answ, 1, 1) == "u") {
      cat("Restoring previous settings!\n")
      seq.settings <- old.seq.settings
    }
  }
  seq.settings
}

#' Interactively set user attributes
#'
#' Allow user to provide values for "user supplied" attribute values
#' \code{"comment"} and \code{"what.measured"}.
#'
#' @param user.attrs character Default values for the attributes.
#'
#' @family interactive acquisition utility functions
#'
#' @return a named list of character vectors.
#'
set_attributes_interactive <- function(user.attrs = list(what.measured = "",
                                                         comment.text = "")) {
  repeat{
    user.input <- readline(prompt = "w = what.measured, c = comment (w/c/-): ")
    if (substr(user.input, 1, 1) == "w") {
      user.attrs$what.measured <- readline("Set 'what.measured': ")
    } else if (substr(user.input, 1, 1) == "c") {
      user.attrs$comment.text <- readline("Set 'comment': ")
    } else {
      break()
    }
  }
  user.attrs
}

#' Interactively get folder to use
#'
#' Enter values for "user supplied" folder.
#'
#' @param folder.name character Default name of the folder.
#'
#' @details If the requested folder does not already exist it will be created.
#' The name of the folder is returned, but NOT set as working directory.
#'
#' @family interactive acquisition utility functions
#'
#' @export
#'
set_folder_interactive <- function(folder.name = NULL) {
  current.folder <- getwd()
  if (is.null(folder.name)) {
    folder.name <- "."
  }

  cat("Current folder: '", current.folder, "'.\n", sep = "")

  folder.name.prompt <-
    paste("Output folder (<path to folder>/./\"", folder.name, "\"-): ", sep = "")
  user.folder.name <- readline(folder.name.prompt)
  if (!user.folder.name == "") {
    folder.name <- user.folder.name
  }
  if (!file.exists(folder.name)) {
    cat("Folder does not exist, creating it...\n")
    # we ask for a different folder name until success
    while (!dir.create(folder.name)) {
      cat("Failure! Unable to create folder: ", folder.name, sep = "")
      folder.name <- readline("Output folder (<path to folder>/.-): ")
      if (folder.name == "") {
        folder.name  <- "."
      }
      if (file.exists(folder.name)) { # we assume that "."  always exists
        break()
      }
    }
  }
  cat("Saving files to: '", folder.name, "'.\n", sep = "")

  folder.name
}


#' Interactively get user name to set
#'
#' Enter values for "user supplied" name.
#'
#' @param user.name character Default name of the folder.
#'
#' @details Validate user name, and allow user to change the default.
#'
#' @family interactive acquisition utility functions
#'
#' @export
#'
set_user_name_interactive <- function(user.name = NULL) {
  # validate argument passed in call
  user.name <- make.names(user.name)
  user.name.prompt <-
    paste("Operator's name (<string>/\"", user.name, "\"-): ", sep = "")
  user.user.name <- readline(user.name.prompt)
  if (! user.user.name == "") {
    user.name <- make.names(user.user.name)
  }
  cat("Using \"", user.name, "\" as operator's name\n", sep = "")
  user.name
}

#' Interactively get session name to set
#'
#' Enter values for "session" name.
#'
#' @param session.name character Default name of the folder.
#'
#' @details Validate seesionr name, and allow user to change the default.
#'
#'
#' @family interactive acquisition utility functions
#'
#' @export
#'
# metadata: get session name and user name from user, offering defaults
set_session_name_interactive <- function(session.name = NULL) {
  session.name <- make.names(session.name) # validate argument passed in call
  session.prompt <- paste("Session's name (<string>/\"", session.name, "\"-): ", sep = "")
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
  session.name
}


#' Manual trigger pulses request
#'
#' This function is used by default. It prints a message asking the operator
#' to manually trigger the flash. A more elaborate function, using specific
#' hardware can be used to automatically trigger the light source, or to
#' enable hard triggering of the light source by the spectrometer itself.
#'
#' @note When using this function, set an integration time that gives enough
#'   time for the manual triggering of the flash to reliably fall within the
#'   integration.
#'
#' @family interactive acquisition utility functions
#'
#' @param n integer Number of pulses (flashes) to trigger per call.
#'
#' @export
#'
f.trigger.message <- function(n = 1L) {
  n <- as.integer(n)
  if (n < 1L) return(TRUE)
  if (n == 1L) {
    cat("Trigger the flash once!!")
  } else {
    cat("Trigger the flash", n, "times!!")
  }
  return(TRUE)
}

#' Format index with enough leading zeros
#'
#' @param idx integer vector Numbers to convert into character strings with
#'   enough leading zeros to accommodate \code{max.idx}.
#' @param max.idx integer vector of length one.
#'
#' @keywords internal
#'
format_idx <- function(idx, max.idx = NULL) {
  if (is.null(max.idx)) {
    max.idx <- max(idx)
  }
  formatC(idx,
          width = trunc(log10(max.idx[1L] + 0.1) + 1),
          format = "d", flag = "0")
}

#' Read numbers from the user
#'
#' A character string is read from the command prompt. The string is split at
#' white space and converted into \code{numeric}. If conversion fails NAs are
#' returned. If the \code{numeric} vector is longer than \code{n.max} it is
#' truncated.
#'
#' @param prompt character Prompt displayed at the console.
#' @param n.max integer Maximum vector length to return.
#' @param pattern character Passed to \code{gsub()}. Characters matched are
#' substituted by a space.
#' @param pass.through character Vector of strings that are returned as is.
#'
#' @keywords internal
#'
read_numbers <- function(prompt,
                         n.max = 1L,
                         pattern = "[,;]",
                         pass.through = "") {
  readline(prompt) |> trimws() -> y

  if (y == "") {
    return(numeric())
  } else if (y %in% pass.through) {
    return(y)
  }

  y |>
    gsub(pattern, " ", x = _) |>
    trimws() |>
    strsplit( "\\s+") |>
    unlist() |> print() |>
    as.numeric() -> z

  if (length(z) <= n.max) {
    z
  } else {
    z[1:n.max]
  }
}

#' Read a duration as seconds from the user
#'
#' A character string is read from the command prompt. The string is split at
#' white space and converted into \code{period}. If conversion fails NAs are
#' returned. If the \code{period} vector is longer than \code{n.max} it is
#' truncated.
#'
#' @param prompt character Prompt displayed at the console.
#' @param n.max integer Maximum vector length to return.
#' @param pattern character Passed to \code{gsub()}. Characters matched are
#' substituted by a space.
#' @param pass.through character Vector of strings that are returned as is.
#' @param minimum numeric Vector of length one.
#'
#' @return A \code{numeric} vector of lengths of time in seconds.
#'
#' @keywords internal
#'
read_seconds <- function(prompt,
                         n.max = 1L,
                         pattern = "[,;S]",
                         pass.through = "",
                         minimum = 0) {
  repeat {
    readline(prompt) |> trimws() -> y

    if (y == "") {
      return(numeric())
    } else if (y %in% pass.through) {
      return(y)
    }

    y |>
      gsub(pattern, " ", x = _) |>
      trimws() |>
      strsplit( "\\s+") |>
      unlist() -> z

    z <- suppressWarnings(as.numeric(z))

    if (length(z) & !anyNA(z) & all(z >= minimum[1])) {
      if (length(z) > n.max) {
        message("Maximum of ", n.max, " value(s) accepted; ",
                length(z) - n.max, " value(s) discarded!")
        z <- z[1:n.max]
      }
      break()
    }
    message("Please, try again. Numbers or numbers followed by S; >= ",
            minimum[1], "seconds")
  }
  z
}

#' Read period from the user
#'
#' A character string is read from the command prompt. The string is split at
#' white space and converted into \code{period}. If conversion fails NAs are
#' returned. If the \code{period} vector is longer than \code{n.max} it is
#' truncated.
#'
#' @param prompt character Prompt displayed at the console.
#' @param n.max integer Maximum vector length to return.
#' @param pattern character Passed to \code{gsub()}. Characters matched are
#' substituted by a space.
#' @param pass.through character Vector of strings that are returned as is.
#'
#' @return A \code{period} vector of lengths of time, negative times such as
#' -1S are interpreted as 1 second (sign is dropped).
#' .
#'
#' @keywords internal
#'
read_period <- function(prompt,
                        n.max = 1L,
                        pattern = "[,;]",
                        pass.through = "") {
  repeat {
    readline(prompt) |> trimws() -> y

    if (y == "") {
      return(lubridate::period())
    } else if (y %in% pass.through) {
      return(y)
    }

    y |>
      gsub(pattern, " ", x = _) |>
      trimws() |>
      strsplit( "\\s+") |>
      unlist() |>
      lubridate::period() -> z

    if (length(z) & !anyNA(z)) {
      if (length(z) > n.max) {
        z <- z[1:n.max]
      }
      break()
    }
    message("Please, try again. Numbers followed by S, M, or H.\n",
            "6S = 6 seconds, 10M12S = 10 minutes 12 seconds.")
  }
  z
}
