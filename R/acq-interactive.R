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
#' @param interface.mode character One of "auto", "simple", "manual", "flash",
#'   "series", "auto-attr", "simple-attr", "manual-attr", "flash-attr", and
#'   "series-attr".
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
#' @details The different interface modes are suitable for different types of
#'   measurements.
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
           HDR.mult = ifelse(interface.mode %in% c("flash", "flash-attr"),
                             c(short = 1, long = 1),
                             c(short = 1, long = 10)),
           protocols = NULL,
           correction.method = NA,
           descriptors = NA,
           stray.light.method = "none",
           save.pdfs = TRUE,
           interface.mode = "auto") {

    # validate interface mode
    interface.mode <- tolower(interface.mode)
    if (!gsub("-attr$", "", interface.mode) %in%
        c("auto", "simple", "flash", "series")) {
      stop("Invalid argument for 'interface.mode', aborting.")
    }
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

    sr.index <- choose_sr_interactive(instruments, w)
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

    # We get metadata from user

    user.session.name <- readline("Session's name: ")
    session.name <- make.names(user.session.name)
    if (user.session.name == "") {
      session.name <- paste("session", trunc(stats::runif(n = 1L, max = 1) * 100000), sep = "")
    }
    if (session.name != user.session.name) {
      message("Using sanitised/generated name: '", session.name, "'.", sep = "")
    }

    session.label <- paste("Operator: ", readline("Operator's name: "),
                           "\nSession: ", session.name,
                           ", instrument s.n.: ", descriptor[["spectrometer.sn"]],
                           sep = "")

    user.attrs <- list(what.measured = "",
                       comment.text = "")

    folder.name <- set_folder_interactive()

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

    # initialize list to collect all names from session
    irrad.names <- character()
    raw.names <- character()
    file.names <- character()

    repeat { # with same settings
      repeat{
        user.obj.name <- readline("Give a name to the spectrum: ")
        obj.name <- make.names(user.obj.name)
        if (obj.name != user.obj.name) {
          answ <- readline(paste("Use sanitised name:", obj.name, " (-/n) :"))
          if (answ == "n") {
            obj.name <- ""
          }
        }
        if (length(obj.name) > 0 && obj.name != "" &&
            !exists(obj.name)) break()
        print("A valid and unique name is required, please try again...")
      }

      raw.name <- paste(obj.name, "raw_mspct", sep = ".")
      raw.names <- c(raw.names, raw.name)
      irrad.name <- paste(obj.name, "spct", sep = ".")
      irrad.names <- c(irrad.names, irrad.name)
      file.name <- paste(irrad.name, "Rda", sep = ".")
      file.names <- c(file.names, file.name)
      pdf.name <- paste(irrad.name, "pdf", sep = ".")

      if (grepl("-attr", interface.mode)) {
        user.attrs <- set_attributes_interactive(user.attrs)
      }

      settings <- tune_interactive(descriptor = descriptor,
                                   settings = settings,
                                   start.int.time = start.int.time)

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

      irrad.spct <- s_irrad_corrected(x = raw.mspct, correction.method = correction.method)

      if (length(user.attrs$what.measured) > 0) {
        setWhatMeasured(irrad.spct, user.attrs$what.measured)
      }

      if (length(user.attrs$comment.text) > 0) {
        comment(irrad.spct) <- paste(comment(irrad.spct), user.attrs$comment.text, sep = "\n")
      }

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

      user.input <- readline("Next, c = make and save collection (-/c): ")
      if (user.input[1] == "c") {
        collection.name <- make.names(readline("Name of the collection?: "))
        irrad.collection.name <- paste(collection.name, "irrad", "mspct", sep = ".")
        raw.collection.name <- paste(collection.name, "raw", "lst", sep = ".")
        collection.file.name <- paste(collection.name, "Rda", sep = ".")

        assign(irrad.collection.name,
               source_mspct(mget(irrad.names)))
        assign(raw.collection.name,
               mget(raw.names))
        save(list = c(irrad.collection.name, raw.collection.name),
             file = collection.file.name)

        file.names <- c(file.names, collection.file.name)

        # clean up
        rm(list = c(irrad.names, raw.names))
        irrad.names <- character()
        raw.names <- character()
      }

      user.input <- readline("Next, p = change protocol, q = quit (-/p/q): ")
      if (user.input[1] == "") {
        next()
      } else if (user.input[1] == "p") {
        protocol <- protocol_interactive(protocols)
      } else if (user.input[1] == "q") {
        break()
      }
    }
    save(file.names,
         file = paste("files4session-",
                      make.names(session.name),
                      ".Rda", sep = ""))

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

    # We get metadata from user

    user.session.name <- readline("Session's name: ")
    session.name <- make.names(user.session.name)
    if (user.session.name == "") {
      session.name <- trunc(stats::runif(max = 999))
    }
    if (session.name != user.session.name) {
      message("Using sanitised/generated name: '", session.name, "'.", sep = "")
    }

    session.label <- paste("Operator: ", readline("Operator's name: "),
                           "\nSession: ", session.name,
                           ", instrument s.n.: ", descriptor[["spectrometer.sn"]],
                           sep = "")

    user.attrs <- list(what.measured = "",
                       comment.text = "")

    folder.name <- set_folder_interactive()

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
        obj.name <- make.names(readline("Give a name to the spectrum: "))
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
          switch(substr(answer, 1, 1),
                 # p = {options(photobiology.radiation.unit = "photon"); next()},
                 # e = {options(photobiology.radiation.unit = "energy"); next()},
                 w = {answer1 <- readline("Waveband set to use, UV+PAR, plants, visible, total, default (u/p/v/t/-)")
                 switch(substr(answer1, 1, 1),
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

      if (user.input == "") {
        next()
      } else if (substr(user.input, 1, 1) == "p") {
        protocol <- protocol_interactive(protocols)
      } else if (substr(user.input, 1, 1) == "q") {
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
tune_interactive <- function(descriptor, settings, start.int.time = 0.1, interface.mode = "auto") {
  if (!interface.mode %in% c("simple", "auto", "flash", "manual")) {
    interface.mode <- "auto"
  }
  # configure interface for active mode
  prompt.text <- switch(interface.mode,
                        simple = "t = retune, r = range, h = HDR mult., u = undo (t/r/h/u/-): ",
                        auto = "t = retune, T = tune, s = skip, m = margin, r = range, h = HDR mult., u = undo (t/s/m/r/h/u/-): ",
                        manual = "f = fixed, s = skip, r = range, h = HDR mult., u = undo (f/r/h/u/-): ",
                        flash = "f = fixed, n = number of flashes, s = skip, h = HDR mult., u = undo (f/n/h/u/-): "
  )
  valid.input <- switch(interface.mode,
                        simple = c("t", "r", "h", "u", ""),
                        auto = c("t", "T", "s", "m", "r", "h", "u", ""),
                        manual = c("f", "s", "r", "h", "u", ""),
                        flash = c("f", "n", "s", "h", "u", "")
  )
  default.input <- switch(interface.mode,
                          simple = c("t", "s"),
                          auto = c("t", "s"),
                          flash =  c("f", "s"),
                          manual = c("f", "s")
  )
  # common code to all modes
  old.settings <- settings # allow starting over
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
    if (substr(answ, 1, 1) == "t") {
      settings <- tune_acq_settings(descriptor = descriptor, acq.settings = settings)
      tuned <- TRUE
    } else if (substr(answ, 1, 1) == "T") {
      settings[["integ.time"]] <- start.int.time * 1e6
      settings <- tune_acq_settings(descriptor = descriptor, acq.settings = settings)
      tuned <- TRUE
    } else if (substr(answ, 1, 1) == "f") {
      cat("Integration time (seconds): ")
      user.integ.time <- scan(nmax = 4L) * 1e6
      if (interface.mode == "flash") {
        settings <- set_integ_time(acq.settings = settings,
                                   integ.time = user.integ.time,
                                   single.scan = TRUE)
      } else {
        settings <- set_integ_time(acq.settings = settings,
                                   integ.time = user.integ.time)
      }
      tuned <- TRUE
    } else if (substr(answ, 1, 1) == "n") {
      cat("Number of flashes for each of", length(settings[["integ.time"]]), "scans.")
      repeat {
        num.flashes <- trunc(scan(nmax = 4L))
        if (length(num.flashes) == length(settings[["integ.time"]])) {
          break()
        }
        cat("Wrong length, please, try again...")
      }
      settings[["num.flashes"]] <- num.flashes
    } else if (substr(answ, 1, 1) == "m") {
      margin <- readline(sprintf("Saturation margin = %.2g, new: ",
                                 settings[["target.margin"]]))
      margin <- try(as.numeric(margin))
      if (!is.na(margin)) {
        settings[["target.margin"]] <- margin
        tuned <- FALSE
      } else {
        print("Value not changed!")
      }
    } else if (substr(answ, 1, 1) == "r") {
      cat("Total time range (seconds), 2 numbers: ")
      tot.time.range <- range(scan(nmax = 2)) * 1e6
      if (tot.time.range[1] >= 0) {
        settings[["tot.time.range"]] <- tot.time.range
        tuned <- FALSE
      } else {
        cat("Value not changed!")
      }
    }  else if (substr(answ, 1, 1) == "h") {
      cat("HDR multipliers, 1 to 4 numbers: ")
      HDR.mult <- sort(scan(nmax = 4))
      if (HDR.mult[1] >= 0  && HDR.mult[length(HDR.mult)] < 1000) {
        settings[["HDR.mult"]] <- HDR.mult
        tuned <- FALSE
      } else {
        cat("Value not changed!")
      }
    } else if (substr(answ, 1, 1) == "u") {
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

#' Interactively select an instrument
#'
#' Choice of spectrometer by name from a list of serial numbers, allowing the
#' user to correct the selection if needed.
#'
#' @param instruments the returm value of \code{list_instruments(w)}.
#' @param w handle to Omni Driver, used to retry if an spectrometer is connected.
#'
#' @keywords internal
#'
choose_sr_interactive <- function(instruments, w = NULL) {
  # if instruments has names these would not be needed
  sn.idx <- 3

  # make sure at least one instrument is connected
  repeat{
    num.inst <- nrow(instruments)
    if (num.inst >= 1) {
      print("Connected spectrometers")
      break()
    } else if (!is.null(w)) {
      cat("No spectrometer found. Abort, or connect one and then retry.")
      if (readline("abort, retry (-/a)") == "a") {
        stop("Aborting as requested! Bye.")
      }
      instruments <- list_instruments(w)
    } else {
      cat("No spectrometer found. Aborting.")
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
    if (substr(answ, 1, 1) == "s") {
      step <- readline(sprintf("Step = %.g2 seconds, new: ",
                               seq.settings[["step"]]))
      step <- try(as.numeric(step))
      if (!is.na(step)) {
        seq.settings[["step"]] <- step
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
        seq.settings[[num.steps]] <- num.steps
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
#' Enter values for "user supplied" attribute values.
#'
#' @keywords internal
#'
set_attributes_interactive <- function(user.attrs) {
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
#' @keywords internal
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
