#' Interactively adjust the integration time settings
#'
#' Adjust integration time settings, allowing the user to repeat the tunning,
#' and to change some of the parameters used for tunning such as total
#' compound integration time and intergration time bracketing.
#'
#' @param descriptor list Descriptor of the instrument, including wrapper to
#'   Java object used to acces the instrument.
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
#' The algorithm also makes use of the linearization function when extrapolating
#' to guess the integration time needed. Initial (default) values are read from
#' \code{acq.settings} while \code{start.int.time} provides a default starting
#' value for integration time for tuning when the user choses not to use the
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
           simple = "RETUNE/range/HDR mult./undo/help/measure (t-/r/h/u/?/m): ",
           auto = "RETUNE/tune/saturation/range/HDR mult./undo/help/measure (t-/T/s/r/h/u/?/m): ",
           manual = "FIXED/range/HDR mult./undo/help/measure (f-/r/h/u/?/m): ",
           full = "FIXED/retune/tune/saturation/range/HDR mult./undo/help/measure (f-/t/T/s/r/h/u/?/m): "
    )
  prompt.text2 <-
    switch(interface.mode,
           simple = "retune/range/HDR mult./undo/help/MEASURE (t/r/h/u/?/m-): ",
           auto = "retune/tune/saturation/range/HDR mult./undo/help/MEASURE (t/T/s/r/h/u/?/m-): ",
           manual = "fixed/range/HDR mult./undo/help/MEASURE (f/r/h/u/?/m-): ",
           full = "fixed/retune/tune/saturation/range/HDR mult./undo/help/MEASURE (f/t/T/s/r/h/u/?/m-): "
    )
  valid.input <-
    switch(interface.mode,
           simple = c("t", "r", "h", "u", "?", "m", ""),
           auto = c("t", "T", "s", "r", "h", "u", "?", "m", ""),
           manual = c("f", "r", "h", "u", "?", "m", ""),
           full = c("f", "t", "T", "s", "r", "h", "u", "?", "m", "")
    )
  default.input <-
    switch(interface.mode,
           simple = c("t", "m"),
           auto = c("t", "m"),
           manual = c("f", "m"),
           full = c("f", "m")
    )
  # set help
  all.help <- c(t = "t = retune. Adjust integration time starting from last value.",
                T = "T = tune. Adjust integration time starting from default value.",
                M = "s = saturation margin. Tuned integration time is maximum * (100 - margin).",
                r = "r = range. Total measurement time in seconds, as a single value or a range.",
                h = "h = HDR mult. High dynamic range or bracketing, as multipliers for target integration time.",
                f = "f = fixed. User-suplied \"base\" integration time in seconds.",
                u = "u = undo. Restore settings from last measurement.",
                H = "? = help. Show this help text.",
                m = "m = MEASURE. Measure without setting/tuning integration time.",
                default = "- = default. Action selected by pressing \"Enter\" key.")
  help.text <- paste(all.help[setdiff(valid.input, "")], collapse = "\n")
  # common code to all modes
  old.settings <- acq.settings # allow starting over
  tuned <- FALSE
  repeat{
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
      cat("Unrecognized letter: '", answ, "'. Please, try again...\n")
    }
    if (answ == "") {
      answ <- ifelse(!tuned, default.input[1], default.input[2])
    }
    if (answ == "m") {
      break()                       ## <- exit point for loop
    }

    if (answ == "?") {
      cat(help.text)
    } else if (answ == "t") {
      if (readline("Auto-adjust integration time?, z = abort (-/z):") == "z") {
        next()
      }
      acq.settings <- tune_acq_settings(descriptor = descriptor,
                                        acq.settings = acq.settings)
      tuned <- TRUE
    } else if (answ == "T") {
      if (readline("Auto-adjust integration time?, z = abort (-/z):") == "z") {
        next()
      }
      acq.settings[["integ.time"]] <- start.int.time * 1e6
      acq.settings <- tune_acq_settings(descriptor = descriptor,
                                        acq.settings = acq.settings)
      tuned <- TRUE
    } else if (answ == "f") {
      cat("Integration time (seconds): ")
      user.integ.time <- scan(nmax = 4L) * 1e6
      acq.settings <- set_integ_time(acq.settings = acq.settings,
                                     integ.time = user.integ.time)
      tuned <- TRUE
    } else if (answ == "s") {
      margin <- readline(sprintf("Saturation margin = %.2g, new: ",
                                 acq.settings[["target.margin"]]))
      margin <- try(as.numeric(margin))
      if (!is.na(margin)) {
        acq.settings[["target.margin"]] <- margin
        tuned <- FALSE
      } else {
        cat("Value not changed!\n")
      }
    } else if (answ == "r") {
      cat("Total time range (seconds), 2 numbers: ")
      tot.time.range <- range(scan(nmax = 2)) * 1e6
      if (tot.time.range[1] >= 0) {
        acq.settings[["tot.time.range"]] <- tot.time.range
        tuned <- FALSE
      } else {
        cat("Value not changed!\n")
      }
    }  else if (answ == "h") {
      old.hdr.mult.len <- length(acq.settings[["HDR.mult"]])
      cat("HDR multipliers, 1 to 4 numbers: ")
      HDR.mult <- sort(scan(nmax = 4))
      if (HDR.mult[1] >= 0  && HDR.mult[length(HDR.mult)] < 1000) {
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

#' Get list of conencted instruments
#'
#' Get list of spectrometers requesting user to connect one or abort if none
#' fould.
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
    answ <- readline("Connect spectrometer to USB port. <enter> = try again, z = abort (-/z):")
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
#'   repeated measurements. Named vector with member names
#'   \code{"initial.delay"}, \code{"step.delay"}, and \code{"num.steps"}.
#' @param measurement.duration numeric Duration of one measurement event (s).
#'
#' @details Function \code{seq.settings()} allows users to enter values needed
#' to define a sequence of spectral acquisitions. These are the delay or time
#' step between successive acquisitions and the number of acquisitions in the
#' series. The \code{measurement.time} determines the minimum length for
#' \code{"step"}.
#'
#' A sequence of measurements are expected to share a single reference or
#' dark scan, and be done in "rapid" sequence.
#'
#' @family interactive acquisition utility functions
#'
#' @return A named numeric vector of length two.
#'
#' @export
#'
set_seq_interactive <- function(seq.settings = list(initial.delay = 0, step.delay = 0, num.steps = 1),
                                measurement.duration = 0) {
  old.seq.settings <- seq.settings

  repeat{
    if (seq.settings$step.delay < measurement.duration) {
      seq.settings$step.delay <- measurement.duration
      message("'step.delay' too short! Reset to ", seq.settings$step.delay, " s.")
    }
    prompt.string <-
           sprintf("Series: w = %.3g s wait; s = %.3g s step; n = %i times; u undo (w/s/n/u/-): ",
                   seq.settings[["initial.delay"]],
                   seq.settings[["step.delay"]],
                   seq.settings[["num.steps"]])
    answ <- readline(prompt  = prompt.string)
    if (answ == "") {
      break()
    }
    if (substr(answ, 1, 1) == "w") {
      step <- readline(sprintf("Wait = %.3g seconds, new: ",
                               seq.settings[["initial.delay"]]))
      step <- try(as.numeric(step))
      if (!is.na(step)) {
        seq.settings[["initial.delay"]] <- step
      } else {
        print("Value not changed!")
      }
    } else if (substr(answ, 1, 1) == "s") {
      step <- readline(sprintf("Step = %.3g seconds, new: ",
                               seq.settings[["step.delay"]]))
      step <- try(as.numeric(step))
      if (!is.na(step)) {
        seq.settings[["step.delay"]] <- step
      } else {
        print("Value not changed!")
      }
    } else if (substr(answ, 1, 1) == "n") {
      num.steps <- readline(sprintf("Number of steps = %i, new: ",
                                    seq.settings[["num.steps"]]))
      num.steps <- try(as.integer(num.steps))
      if (is.na(num.steps)) {
        print("Number of steps must be a positive integer. Value not changed!")
      } else if (num.steps < 1L || num.steps > 10000L) {
        warning("Number of steps must be in range 1..10000. Value not changed!")
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
      user.attrs$what.measured <- readline("Set 'what.measured' = ")
    } else if (substr(user.input, 1, 1) == "c") {
      user.attrs$comment.text <- readline("Set 'comment' = ")
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

  cat("Current folder: '", current.folder, "'.\n")

  folder.name.prompt <-
    paste("Output folder (-/./<path to folder>):", folder.name)
  user.folder.name <- readline(folder.name.prompt)
  if (!user.folder.name == "") {
    folder.name <- user.folder.name
  }
  if (!file.exists(folder.name)) {
    cat("Folder does not exist, creating it...\n")
    # we ask for a different folder name until success
    while (!dir.create(folder.name)) {
      cat("Failure! Unable to create folder: ", folder.name)
      folder.name <- readline("Output folder (-/./<path to folder>): .")
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
