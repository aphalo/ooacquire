#' Acquire spectra interactively
#'
#' Functions providing a simple interactive front-end to the functions in the
#' package. Mostly as example code that can be modified for diferent
#' uses.
#'
#' @param tot.time.range numeric vector Range of total times for a measurement
#'   in seconds.
#' @param target.margin numeric (0..1) when tuning integration time, how big a
#'   head space to leave.
#' @param HDR.mult numeric the integration time for each bracketed integration
#'   as a multiplier of the set or tuned integration time.
#' @param protocols list of character vectors.
#' @param correction.method list The method to use when applying the calibration
#' @param descriptors list A list of instrument descriptors containing
#'   calibration data.
#' @param stray.light.method character Used only when the correction method is
#'   created on-the-fly.
#' @param save.pdfs logical Whether to save PDFs to files or not.
#'
#' @export
#'
#' @note The integration time is set automatically so that the peak number of
#'   counts is close to 1 - \code{target.margin} of maximum range of the
#'   instrument detector. The minmum \code{tot.time} is achieved by increasing
#'   the number of scans. The default protocols are usually suitable, if new
#'   protocols are passed, each character vector must contain strings "light",
#'   "filter" and "dark", or "sample", "reference", and "dark", depending on
#'   the function.
#'
#' @examples
#'
#' \dontrun{
#' # requires an Ocean Optics spectrometer to be connected via USB
#' acquire_interactive()
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
           save.pdfs = TRUE) {

    # define measurement protocols
    if (length(protocols) == 0) {
      protocols <- list(l = "light",
                        ld = c("light", "dark"),
                        lf = c("light", "filter"),
                        lfd = c("light", "filter", "dark")
      )
    }

    w <- start_session()

    instruments <- list_instruments(w)

    sr.index <- choose_sr_interactive(instruments)
    ch.index <- choose_ch_interactive(instruments, sr.index)

    serial_no <- as.character(instruments[sr.index + 1L, 3])

    message("Using channel ", ch.index,
            " from spectrometer with serial number: ", serial_no)

    if (anyNA(c(descriptors[[1]], correction.method[[1]]))) {
      descriptor <-
        switch(serial_no,
               MAYP11278 = which_descriptor(descriptors = ooacquire::MAYP11278_descriptors),
               MAYP112785 = which_descriptor(descriptors = ooacquire::MAYP112785_descriptors),
               get_oo_descriptor(w, sr.index = sr.index, ch.index = ch.index)
        )

      correction.method <-
        switch(serial_no,
               MAYP11278 = ooacquire::MAYP11278_ylianttila.mthd,
               MAYP112785 = ooacquire::MAYP112785_ylianttila.mthd,
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

    # Before continuing we check that calibrations are available
    stopifnot(length(descriptor[["wavelengths"]]) == descriptor[["num.pixs"]])
    stopifnot(length(descriptor[["inst.calib"]][["irrad.mult"]]) == descriptor[["num.pixs"]])

    session.label <- paste("operator: ", readline("Operator's name: "),
                           ", instrument s.n.: ", descriptor[["spectrometer.sn"]])

    folder.name <- readline("Enter folder name (use forward slashes '/' instead of '\'): ")
    if (length(folder.name == 0)) {
      folder.name <- "."
    }
    # need to add folder.name sanitation
    if (!file.exists(folder.name)) {
      message("Folder does not exist, creating it...")
      dir.create(folder.name)
    }
    oldwd <- setwd(folder.name)
    message("Files will be saved to '", folder.name, "'", sep="")

    protocol <- protocol_interactive(protocols = protocols)

    start.int.time <- 0.1 # seconds

    # data set measured with same protocol values but adjusted integration time

    settings <- acq_settings(descriptor = descriptor,
                             integ.time = start.int.time,
                             target.margin = target.margin,
                             tot.time.range = tot.time.range,
                             HDR.mult = HDR.mult)

    seq.settings <- list(step = 0,
                         num.steps = 1L)

    # save current value as starting value for next iteration

    repeat { # with same settings
      repeat{
        obj.name <- readline("Give a name to the spectrum: ")
        if (length(obj.name) > 0 && obj.name != "") break()
        print("A valid and unique name is required, please try again...")
      }
      raw.name <- paste(obj.name, "raw_mspct", sep = ".")
      irrad.name <- paste(obj.name, "spct", sep = ".")
      file.name <- paste(irrad.name, "Rda", sep = ".")
      pdf.name <- paste(irrad.name, "pdf", sep = ".")

      settings <- tune_interactive(descriptor = descriptor, settings = settings)
      seq.settings <- set_seq_interactive(seq.settings)

      raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                 acq.settings = settings,
                                 seq.settings = seq.settings,
                                 protocol = protocol,
                                 user.label = obj.name)

      if (length(raw.mspct) == 0) {
        next()
      }

      irrad.spct <- s_irrad_corrected(x = raw.mspct, correction.method = correction.method)

      assign(raw.name, raw.mspct)
      assign(irrad.name, irrad.spct)

      save(list = c(raw.name, irrad.name), file = file.name)

      repeat {
        fig <- graphics::plot(irrad.spct) +
          ggplot2::labs(title = obj.name,
                        subtitle = paste(photobiology::getWhenMeasured(irrad.spct), " UTC, ",
                                         session.label, sep = ""),
                        caption = paste("ooacquire", utils::packageVersion("ooacquire"))) +
          ggplot2::theme_bw()
        print(fig)
        answer <- readline("Plot as photons/energy/wavebands/discard/save and continue (p/e/w/d/-): ")
        switch(answer,
               p = {options(photobiology.radiation.unit = "photon"); next()},
               e = {options(photobiology.radiation.unit = "energy"); next()},
               w = {answer1 <- readline("Waveband set to use, UV+PAR, plants, visible, total, default (u/p/v/t/-): ")
               switch(answer1,
                      u = options(photobiology.plot.bands =
                                    list(photobiologyWavebands::UVC(),
                                         photobiologyWavebands::UVB(),
                                         photobiologyWavebands::UVA(),
                                         photobiologyWavebands::PAR())),
                      p = options(photobiology.plot.bands = photobiologyWavebands::Plant_bands()),
                      v = options(photobiology.plot.bands = photobiologyWavebands::VIS_bands()),
                      t = options(photobiology.plot.bands =
                                    list(new_waveband(min(irrad.spct), max(irrad.spct), wb.name = "Total"))),
                      options(photobiology.plot.bands = NULL))
               next()},
               d = break()
        )
        if (save.pdfs) {
          grDevices::pdf(file = pdf.name, width = 8, height = 6)
          print(fig)
          grDevices::dev.off()
        }
        break()
      }

      user.input <- readline("Next, change protocol, quit (-/p/q): ")

      if (user.input[1] == "") {
        next()
      } else if (user.input[1] == "p") {
        protocol <- protocol_interactive(protocols)
      } else if (user.input[1] == "q") {
        break()
      }
    }
    print("Ending...")
    end_session(w)
    setwd(oldwd)
    message("Folder reset to: ", getwd())
    message("Bye!")
  }

#' @rdname acq_irrad_interactive
#'
#' @param ref.value numeric or filter_spct/reflector_spct object.
#' @param qty.out character One of "Tfr" (transmittance as a fraction of one)
#'   or "raw" (raw counts).
#' @param type character Type of transmittance or reflectance measured.
#'
#' @note The calculations for reflectance and transmittance are very similar,
#'   so we provide a single function capable of handling both. For
#'   transmittance the reference is usually direct exposure to radiation but
#'   for reflectance a white reference patch is normally used. In some cases
#'   one may want to use a grey reference. We provide an argument that allows
#'   the user to supply a constant or a spectrum describing the properties of
#'   the reference. It is also important to distinguish between total and
#'   internal transmittance, and which of these is measured depends on the
#'   measuring protocol.
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

    stopifnot(qty.out %in% c("Tfr", "Rfr", "raw"))

    # define measurement protocols
    if (length(protocols) == 0L) {
      protocols <- list(rsd = c("reference", "sample", "dark"),
                        rs = c("reference", "sample"))
    }

    w <- start_session()

    instruments <- list_instruments(w)
    print(instruments)

    sr.index <- choose_sr_interactive(instruments)
    ch.index <- choose_ch_interactive(instruments, sr.index)

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

    session.label <- paste("operator: ", readline("Operator's name: "),
                           ", instrument s.n.: ", descriptor[["spectrometer.sn"]],
                           sep = "")

    folder.name <- readline("Enter folder name (use forward slashes '/' instead of '\'): ")
    if (length(folder.name == 0)) {
      folder.name <- "."
    }
    # need to add folder.name sanitation
    if (!file.exists(folder.name)) {
      message("Folder does not exist, creating it...")
      dir.create(folder.name)
    }
    oldwd <- setwd(folder.name)
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
        obj.name <- readline("Give a name to the spectrum: ")
        if (length(obj.name) > 0 && !exists(obj.name)) break()
        print("A valid and unique name is required, please try again...")
      }
      raw.name <- paste(obj.name, "raw_spct", sep = ".")
      filter.name <- paste(obj.name, "spct", sep = ".")
      file.name <- paste(filter.name, "Rda", sep = ".")
      pdf.name <- paste(filter.name, "pdf", sep = ".")

      settings <- tune_interactive(descriptor = descriptor, settings = settings)

      raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                 acq.settings = settings,
                                 protocol = protocol,
                                 user.label = obj.name)

      if (length(raw.mspct) == 0) {
        next()
      }

      assign(raw.name, raw.mspct)

      if (qty.out == "raw") {
        fig <- graphics::plot(raw.mspct[["sample"]]) +
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
                                            ref.value = ref.value)
        assign(filter.name, filter.spct)
        save(list = c(raw.name, filter.name), file = file.name)

        repeat {
          fig <- graphics::plot(filter.spct) +
            ggplot2::labs(title = filter.name,
                          subtitle = paste(photobiology::getWhenMeasured(filter.spct), " UTC, ",
                                           session.label, sep = ""),
                          caption = paste("ooacquire", utils::packageVersion("ooacquire"))) +
            ggplot2::theme_bw()
          print(fig)
          answer <- readline("Change wavebands/discard/save and continue (/w/d/-): ")
          switch(answer,
                 # p = {options(photobiology.radiation.unit = "photon"); next()},
                 # e = {options(photobiology.radiation.unit = "energy"); next()},
                 w = {answer1 <- readline("Waveband set to use, UV+PAR, plants, visible, total, default (u/p/v/t/-)")
                 switch(answer1,
                        u = options(photobiology.plot.bands =
                                      list(photobiologyWavebands::UVC(),
                                           photobiologyWavebands::UVB(),
                                           photobiologyWavebands::UVA(),
                                           photobiologyWavebands::PAR())),
                        p = options(photobiology.plot.bands = photobiologyWavebands::Plant_bands()),
                        v = options(photobiology.plot.bands = photobiologyWavebands::VIS_bands()),
                        t = options(photobiology.plot.bands =
                                      list(new_waveband(min(filter.spct), max(filter.spct), wb.name = "Total"))),
                        options(photobiology.plot.bands = NULL))
                 next()},
                 d = break()
          )
          break()
        }
      }
      if (save.pdfs) {
        grDevices::pdf(file = pdf.name, width = 8, height = 6)
        print(fig)
        grDevices::dev.off()
      }

      user.input <- readline("Next, change protocol, quit (-/p/q): ")

      if (user.input[1] == "") {
        next()
      } else if (user.input[1] == "p") {
        protocol <- protocol_interactive(protocols)
      } else if (user.input[1] == "q") {
        break()
      }
    }
    print("Ending...")
    end_session(w)
    setwd(oldwd)
    message("Folder reset to: ", getwd())
    message("Bye!")
  }

#' Interactively adjust the integration time settings
#'
#' Adjust integration time settings, allowing the user to repeat the tunning,
#' and to change some of the parameters used for tunning.
#'
#' @keywords internal
#'
tune_interactive <- function(descriptor, settings) {
  old.settings <- settings
  tuned <- FALSE
  repeat{
    cat("Ready to adjust integration time?\n")
    answ <- readline("t = tune, s = skip, m = margin, r = range, h = HDR mult., u = undo (t/s/m/r/h/u/-): ")
    if (answ == "") {
      answ <- ifelse(tuned, "s", "t")
    }
    if (answ == "t") {
      settings <- tune_acq_settings(descriptor = descriptor, acq.settings = settings)
      tuned <- TRUE
    } else if (answ == "s") {
      break()
    } else if (answ == "m") {
      margin <- readline(sprintf("Saturation margin = %.2g, new: ",
                                 settings[["target.margin"]]))
      margin <- try(as.numeric(margin))
      if (!is.na(margin)) {
        settings[["target.margin"]] <- margin
        tuned <- FALSE
      } else {
        print("Value not changed!")
      }
    } else if (answ == "r") {
      cat("Total time range (seconds), 2 numbers: ")
      tot.time.range <- range(scan(nmax = 2)) * 1e6
      if (tot.time.range[1] >= 0) {
        settings[["tot.time.range"]] <- tot.time.range
        tuned <- FALSE
      } else {
        cat("Value not changed!")
      }
    }  else if (answ == "h") {
      cat("HDR multipliers, 1 to 4 numbers: ")
      HDR.mult <- sort(scan(nmax = 4))
      if (HDR.mult[1] >= 0  && HDR.mult[length(HDR.mult)] < 1000) {
        settings[["HDR.mult"]] <- HDR.mult
        tuned <- FALSE
      } else {
        cat("Value not changed!")
      }
    } else if (answ == "u") {
      cat("Restoring previous settings!")
      settings <- old.settings
    }
  }
  settings
}

#' Interactively select a measurement protocol
#'
#' Choose a protocol by name from a list of protocols, allowing the user to
#' correct the selection if needed.
#'
#' @keywords internal
#'
protocol_interactive <- function(protocols) {
  prompt <- paste("Protocols: ",
                  paste(names(protocols), collapse = ", "),
                  ": ")
  repeat{
    user.input <- readline(prompt = prompt)
    if (user.input[1] == "") {
      user.input <- names(protocols)[[1]]
    }
    if (user.input[1] %in% names(protocols)) {
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

#' Interactively select an instrument
#'
#' Choice of spectrometer by name from a list of serial numbers, allowing the
#' user to correct the selection if needed.
#'
#' @param instruments the returm value of \code{list_instruments(w)}.
#'
#' @keywords internal
#'
choose_sr_interactive <- function(instruments) {
  # if instruments has names these would not be needed
  sn.idx <- 3

  print("Connected spectrometers")
  num.inst <- nrow(instruments)
  if (num.inst < 1) {
    print("No spectrometer found. Please connect one, or abort.")
    if (readline("abort, retry (-/a)") != "a") {
      next()
    } else {
      stop("Aborting as requested! Bye.")
    }
  }
  # select instrument
  if (num.inst > 1) {
    prompt <- paste(1:nrow(instruments), ": ", instruments[[sn.idx]],
                    " (choose by index): ", sep = "")
    repeat{
      sr.idx <- as.integer(readline(prompt = prompt))
      if (sr.idx[1] == "") {
        sr.idx <- 1L
      }
      if (sr.idx[1] %in% 1L:nrow(instruments)) {
        sr.index <- sr.idx - 1L
        break()
      } else {
        print("A number between 1 and ", num.inst, " is required.")
      }
    }
  } else { # num.inst == 1
    sr.idx <- 1L
    sr.index <- sr.idx - 1L
  }
  print(instruments[sr.idx, ])
  sr.index
}


#' Interactively select a channel
#'
#' Choice of channel to be used if spectrometer has more than one channel.
#'
#' @param instruments the returm value of \code{list_instruments(w)}
#' @param sr.index integer The index to the spectrometer, starting from zero,
#'   following C conventions instead of R indexing conventions.
#' @param prompt.text character string to use as prompt.
#'
#' @keywords internal
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
        ch.index <- ch.idx - 1L
        break()
      } else {
        print(paste("A number between 1 and ", num.channels, " is required.", sep = ""))
      }
    }
  } else {
    ch.index <- 0L
  }
  ch.index
}

#' Interactively set sequential measurements
#'
#' Adjust integration time settings, allowing the user to repeat the tunning,
#' and to change some of the parameters used for tunning.
#'
#' @keywords internal
#'
set_seq_interactive <- function(seq.settings) {
  old.seq.settings <- seq.settings
  repeat{
    cat("Ready to set sequence parameters?\n")
    answ <- readline("s = step size, n = step number, u = undo (s/n/u/-): ")
    if (answ == "") {
      break()
    }
    if (answ == "s") {
      step <- readline(sprintf("Step = %.g2 seconds, new: ",
                                 seq.settings[["step"]]))
      step <- try(as.numeric(step))
      if (!is.na(step)) {
        seq.settings[["step"]] <- step
      } else {
        print("Value not changed!")
      }
    } else if (answ == "n") {
      num.steps <- readline(sprintf("Number of steps = %i, new: ",
                               seq.settings[["num.steps"]]))
      num.steps <- try(as.integer(num.steps))
      if (is.na(num.steps)) {
        print("Number of steps must be a positive integer. Value not changed!")
      } else if (num.steps < 1L || num.steps > 10000L) {
        warning("Number of steps must be in range 1..10000. Value not changed!")
      } else {
        seq.settings[[num.steps]] <- num.steps
      }
    } else if (answ == "u") {
      cat("Restoring previous settings!")
      seq.settings <- old.seq.settings
    }
  }
  seq.settings
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

    # define measurement protocols
    protocols <- list(rsd = c("reference", "sample", "dark"),
                      rs = c("reference", "sample"))

    w <- start_session()

    instruments <- list_instruments(w)
    print(instruments)

    sr.index <- 0L
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

    session.label <- paste("operator: ", readline("Operator's name: "),
                           ", instrument s.n.: ", rfr.descriptor[["spectrometer.sn"]],
                           sep = "")

    folder.name <- readline("Enter folder name (use forward slashes '/' instead of '\'): ")
    if (length(folder.name == 0)) {
      folder.name <- "."
    }
    # need to add folder.name sanitation
    if (!file.exists(folder.name)) {
      message("Folder does not exist, creating it...")
      dir.create(folder.name)
    }
    oldwd <- setwd(folder.name)
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
        obj.name <- readline("Give a name to the spectrum: ")
        if (length(obj.name) > 0 && !exists(obj.name)) break()
        print("A valid and unique name is required, please try again...")
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
                                       settings = rfr.settings)

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
                                       settings = tfr.settings)

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
          msmsply(trim_counts) %>%
          msmsply(linearize_counts) %>%
          raw2cps() %>%
          msmsply(merge_cps) -> rfr.cps.mspct

        cps2Rfr(rfr.cps.mspct$sample,
                rfr.cps.mspct$reference,
                rfr.cps.mspct$dark) -> rfr.spct

        assign(rfr.name, rfr.spct)

        # transmitance
        tfr.raw.mspct %>%
          msmsply(trim_counts) %>%
          msmsply(linearize_counts) %>%
          raw2cps() %>%
          msmsply(merge_cps) -> tfr.cps.mspct

        cps2Tfr(tfr.cps.mspct$sample,
                tfr.cps.mspct$reference,
                tfr.cps.mspct$dark) -> tfr.spct

        assign(tfr.name, tfr.spct)

        object.spct <-
          object_spct(w.length = rfr.spct[["w.length"]],
                      Rfr = rfr.spct[["Rfr"]],
                      Tfr = tfr.spct[["Tfr"]],
                      Rfr.type = "total",
                      Tfr.type = "total",
                      comment = obj.name)

        object.spct <-
          copy_attributes(rfr.spct, object.spct,
                          c("what_measured", "when_measured", "instr_desc"))

        object.spct <- clip_wl(object.spct)

        assign(spct.name, object.spct)

        save(list = c(rfr.raw.name, tfr.raw.name, spct.name, rfr.name, tfr.name),
             file = file.name)

        repeat {
          fig1 <- graphics::plot(rfr.spct, range = c(280, 850)) +
            ggplot2::labs(title = spct.name,
                          subtitle = paste(photobiology::getWhenMeasured(rfr.spct), " UTC, ",
                                           session.label, sep = ""),
                          caption = paste("ooacquire", utils::packageVersion("ooacquire"))) +
            ggplot2::theme_bw()
          fig2 <- graphics::plot(tfr.spct, range = c(280, 850)) +
            ggplot2::labs(title = spct.name,
                          subtitle = paste(photobiology::getWhenMeasured(tfr.spct), " UTC, ",
                                           session.label, sep = ""),
                          caption = paste("ooacquire", utils::packageVersion("ooacquire"))) +
            ggplot2::theme_bw()

          print(ggspectra::multiplot(fig1, fig2))

          answer <- readline("Change wavebands/discard/save and continue (/w/d/-): ")
          switch(answer,
                 # p = {options(photobiology.radiation.unit = "photon"); next()},
                 # e = {options(photobiology.radiation.unit = "energy"); next()},
                 w = {answer1 <- readline("Waveband set to use, UV+PAR, plants, visible, total, default (u/p/v/t/-)")
                 switch(answer1,
                        u = options(photobiology.plot.bands =
                                      list(photobiologyWavebands::UVC(),
                                           photobiologyWavebands::UVB(),
                                           photobiologyWavebands::UVA(),
                                           photobiologyWavebands::PAR())),
                        p = options(photobiology.plot.bands = photobiologyWavebands::Plant_bands()),
                        v = options(photobiology.plot.bands = photobiologyWavebands::VIS_bands()),
                        t = options(photobiology.plot.bands =
                                      list(new_waveband(min(object.spct), max(object.spct), wb.name = "Total"))),
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

      user.input <- readline("Next, change protocol, quit (-/p/q): ")

      if (user.input[1] == "") {
        next()
      } else if (user.input[1] == "p") {
        protocol <- protocol_interactive(protocols)
      } else if (user.input[1] == "q") {
        break()
      }
    }
    print("Ending...")
    end_session(w)
    setwd(oldwd)
    message("Folder reset to: ", getwd())
    message("Bye!")
  }
