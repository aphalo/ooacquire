#' Interactively adjust the integration time settings
#'
#' Adjust integration time settings, allowing the user to repeat the tunning,
#' and to change some of the parameters used for tunning.
#'
#' @param descriptor list Descriptor of the instrument, including wrapper to
#'   Java object used to acces the instrument.
#' @param acq.settings list containing starting values for instrument settings.
#' @param start.int.time numeric Integration time to use as starting guess when
#'   tuning the settings.
#' @param interface.mode character One of "simple", "auto", or "manual".
#'
#' @export
#'
tune_interactive <- function(descriptor,
                             acq.settings,
                             start.int.time = 0.1,
                             interface.mode = "auto") {
  if (!interface.mode %in% c("simple", "auto", "manual")) {
    interface.mode <- "auto"
  }
  # configure interface for active mode
  prompt.text <- switch(interface.mode,
                        simple = "t = retune, r = range, h = HDR mult., u = undo (t/r/h/u/-): ",
                        auto = "t = retune, T = tune, s = skip, m = margin, r = range, h = HDR mult., u = undo (t/s/m/r/h/u/-): ",
                        manual = "f = fixed, s = skip, r = range, h = HDR mult., u = undo (f/r/h/u/-): "
  )
  valid.input <- switch(interface.mode,
                        simple = c("t", "r", "h", "u", ""),
                        auto = c("t", "T", "s", "m", "r", "h", "u", ""),
                        manual = c("f", "s", "r", "h", "u", "")
  )
  default.input <- switch(interface.mode,
                          simple = c("t", "s"),
                          auto = c("t", "s"),
                          manual = c("f", "s")
  )
  # common code to all modes
  old.settings <- acq.settings # allow starting over
  tuned <- FALSE
  repeat{
    cat("Ready to adjust integration time?\n")
    repeat {
      answ <- readline(prompt.text)
      if (answ[1] %in% valid.input) {
        break()
      }
      cat("Unrecognized letter: ", answ[1], ". Please, try again.")
    }
    if (answ == "") {
      answ <- ifelse(!tuned, default.input[1], default.input[2])
    }
    if (answ %in% c("s", "g")) {
      break()                       ## <- exit point for loop
    }

    if (substr(answ, 1, 1) == "t") {
      acq.settings <- tune_acq_settings(descriptor = descriptor, acq.settings = acq.settings)
      tuned <- TRUE
    } else if (substr(answ, 1, 1) == "T") {
      acq.settings[["integ.time"]] <- start.int.time * 1e6
      acq.settings <- tune_acq_settings(descriptor = descriptor, acq.settings = acq.settings)
      tuned <- TRUE
    } else if (substr(answ, 1, 1) == "f") {
      cat("Integration time (seconds): ")
      user.integ.time <- scan(nmax = 4L) * 1e6
      acq.settings <- set_integ_time(acq.settings = acq.settings,
                                 integ.time = user.integ.time)
      tuned <- TRUE
    } else if (substr(answ, 1, 1) == "m") {
      margin <- readline(sprintf("Saturation margin = %.2g, new: ",
                                 acq.settings[["target.margin"]]))
      margin <- try(as.numeric(margin))
      if (!is.na(margin)) {
        acq.settings[["target.margin"]] <- margin
        tuned <- FALSE
      } else {
        print("Value not changed!")
      }
    } else if (substr(answ, 1, 1) == "r") {
      cat("Total time range (seconds), 2 numbers: ")
      tot.time.range <- range(scan(nmax = 2)) * 1e6
      if (tot.time.range[1] >= 0) {
        acq.settings[["tot.time.range"]] <- tot.time.range
        tuned <- FALSE
      } else {
        cat("Value not changed!")
      }
    }  else if (substr(answ, 1, 1) == "h") {
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
        cat("Value not changed!")
      }
    } else if (substr(answ, 1, 1) == "u") {
      cat("Restoring previous settings!")
      acq.settings <- old.settings
    }
  }
  acq.settings
}

#' Interactively select a measurement protocol
#'
#' Choose a protocol by name from a list of protocols, allowing the user to
#' correct the selection if needed.
#'
#' @param protocols list Measuring protocol defifinitions and names.
#'
#' @export
#'
protocol_interactive <- function(protocols) {
  stopifnot(length(protocols) > 0L)
  prompt <- paste("Protocols: ",
                  paste(names(protocols), collapse = ", "),
                  ": ")
  repeat{
    user.input <- readline(prompt = prompt)
    if (user.input == "") {
      user.input <- names(protocols)[[1]]
    }
    if (user.input %in% names(protocols)) {
      protocol <- protocols[[user.input[1]]]
      if (readline(paste("Will use protocol ",
                         paste(protocol, collapse = " -> "),
                         " o.k.? (-/n): ", sep = "")) != "n") {
        break()
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
#' @export
#'
list_srs_interactive <- function(w) {
  while (rOmniDriver::number_srs(w) < 1L) {
    answ <- readline("Please, connect a spectrometer to an USB port. <enter> = try again, z = abort.")
    if (answ[1] %in% c("z", "Z")) {
      break()
    }
  }
  list_instruments(w)
}

#' Interactively select an instrument
#'
#' Choice of spectrometer by name from a list of serial numbers, allowing the
#' user to correct the selection if needed.
#'
#' @param instruments the returm value of \code{list_instruments(w)}.
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
    print("Connected spectrometers")
  } else {
    cat("No spectrometers found.")
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
        print("A number between 1 and ", num.inst, " is required.")
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
        print(paste("A number between 1 and ", num.channels, " is required.", sep = ""))
      }
    }
  } else {
    ch.index <- 1L
  }
  ch.index - 1L # use Omni Driver convention for indexes
}

#' Interactively set sequential measurements
#'
#' Adjust integration time settings, allowing the user to repeat the tunning,
#' and to change some of the parameters used for tunning.
#'
#' @param seq.settings numeric Definition of time steps for a sequence of repeated
#'   measurements. Named vector with member names \code{"step"}, and
#'   \code{"steps"}.
#'
#' @export
#'
set_seq_interactive <- function(seq.settings = c(step.delay = 0, num.steps = 1L)) {
  old.seq.settings <- seq.settings
  repeat{
    cat("Ready to set sequence parameters?\n")
    answ <- readline("s = step size, n = step number, u = undo (s/n/u/-): ")
    if (answ == "") {
      break()
    }
    if (substr(answ, 1, 1) == "s") {
      step <- readline(sprintf("Step = %.g2 seconds, new: ",
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
      cat("Restoring previous settings!")
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
#' @export
#'
set_folder_interactive <- function(folder.name = ".") {
  old.folder.name <- folder.name
  folder.name <- readline("Enter folder name (use forward slashes '/' instead of '\'): ")
  message("Folder: ", folder.name)
  # needs to be replaced by a proper vailidity check
  if (folder.name == "") {
    folder.name <- old.folder.name
    message("Folder: ", folder.name)
  }
  if (!file.exists(folder.name)) {
    message("Folder does not exist, creating it...")
    dir.create(folder.name)
  } else {
    message("Using existing folder: '", folder.name, "'.")
  }
  folder.name
}

#' Manual trigger pulses request
#'
#' This function is used by default. It prints a message asking the operator
#' to manually trigger the flash. A more elaborate function, using specific
#' hardware can be used to automatically trigger the light source, or to
#' enable hard triggering of the light source by the spectrometer itself.
#'
#' @note When using this function, use an integration time that gives enough
#' time for the manual triggering of the flash reliably within the integration.
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
