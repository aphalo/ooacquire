#' Acquire flash spectra interactively
#'
#' Functions providing a simple interactive front-end to the functions in the
#' package. Mostly as example code that can be modified for diferent
#' uses.
#'
#' @details These functions can be useful as is for commonly done measurements
#'   but they also play the role of examples that users can modify according
#'   to their needs. They are all composed in a modular way from functions
#'   that can be reshuffled and combined with other functions to define
#'   new variations better suited to users needs and tastes. The examples we
#'   provide here cover the measurement of spectral irradiance of pulsed light
#'   sources: spectral fluence from flashes, and transmittance, reflectance
#'   and absorbance using a flash as light sources.
#'
#'   The default behaviour of the functions can be changed by passing different
#'   arguments through parameters, but for special use cases it could be best
#'   for users to define case specific data acquisition functions from the
#'   same building blocks.
#'
#' @param integ.time numeric Integration time for a measurement event
#'   in seconds.
#' @param num.scans integer Number of scans to average.
#' @param num.exposures integer Number or light pulses (flashes) per scan. Set
#'   to \code{-1L} to indicate that the light source is continuous.
#' @param f.trigger.pulses function Function to be called to trigger light
#'   pulse(s). Should accept as its only argument the number of pulses, and
#'   return \code{TRUE} on sucess and \code{FALSE} on failure.
#' @param protocols list of character vectors.
#' @param correction.method list The method to use when applying the calibration
#' @param descriptors list A list of instrument descriptors containing
#'   calibration data.
#' @param stray.light.method character Used only when the correction method is
#'   created on-the-fly.
#' @param save.pdfs logical Whether to save PDFs to files or not.
#' @param interface.mode character One of "manual", and "manual-attr".
#'
#' @export
#'
#' @note The integration time is set manually and the flash is triggered within
#'   this "window" one or more times. The default protocols are usually
#'   suitable, if new protocols are passed, each character vector must contain
#'   strings "light", "filter" and "dark", or "sample", "reference", and "dark",
#'   depending on the function.
#'
#' @details The different interface modes are suitable for different types of
#'   measurements.
#'
#' @examples
#'
#' \dontrun{
#' # requires an Ocean Optics spectrometer to be connected via USB
#' acq_fluence_interactive()
#' }
#'
acq_fluence_interactive <-
  function(integ.time = 2,
           num.scans = 1L,
           num.exposures = 1L,
           f.trigger.pulses = f.trigger.message,
           protocols = NULL,
           correction.method = NA,
           descriptors = NA,
           stray.light.method = "none",
           save.pdfs = TRUE,
           interface.mode = "manual") {

    # validate interface mode
    interface.mode <- tolower(interface.mode)
    if (!gsub("-attr$", "", interface.mode) %in%
        c("manual")) {
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

    instruments <- list_srs_interactive(w = w)
    sr.index <- choose_sr_interactive(instruments = instruments)
    if (sr.index < 0L) {
      print("Aborting...")
      end_session(w = w)
      message("Bye!")
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
      session.name <- make.names(lubridate::now())
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

    # data set measured with same protocol values but adjusted integration time

    settings <- acq_settings(descriptor = descriptor,
                             integ.time = integ.time,
                             target.margin = 0,
                             tot.time.range = integ.time,
                             HDR.mult = 1)

    seq.settings <- list(step = 0,
                         num.steps = 1L)

    # initialize list to collect all names from session
    fluence.names <- character()
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
      fluence.name <- paste(obj.name, "spct", sep = ".")
      fluence.names <- c(fluence.names, fluence.name)
      file.name <- paste(fluence.name, "Rda", sep = ".")
      file.names <- c(file.names, file.name)
      pdf.name <- paste(fluence.name, "pdf", sep = ".")

      if (grepl("-attr", interface.mode)) {
        user.attrs <- set_attributes_interactive(user.attrs)
      }

      raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                 acq.settings = settings,
                                 seq.settings = seq.settings,
                                 protocol = protocol,
                                 f.trigger.pulses = f.trigger.pulses,
                                 user.label = obj.name)

      if (length(raw.mspct) == 0) {
        next()
      }

      fluence.spct <- s_irrad_corrected(x = raw.mspct,
                                        correction.method = correction.method,
                                        num.exposures = num.exposures)

      if (length(user.attrs$what.measured) > 0) {
        setWhatMeasured(fluence.spct, user.attrs$what.measured)
      }

      if (length(user.attrs$comment.text) > 0) {
        comment(fluence.spct) <- paste(comment(fluence.spct), user.attrs$comment.text, sep = "\n")
      }

      assign(raw.name, raw.mspct)
      assign(fluence.name, fluence.spct)

      save(list = c(raw.name, fluence.name), file = file.name)

      repeat {
        fig <- graphics::plot(fluence.spct) +
          ggplot2::labs(title = obj.name,
                        subtitle = paste(photobiology::getWhenMeasured(fluence.spct), " UTC, ",
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
                                    list(new_waveband(min(fluence.spct), max(fluence.spct), wb.name = "Total"))),
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
        fluence.collection.name <- paste(collection.name, "fluence", "mspct", sep = ".")
        raw.collection.name <- paste(collection.name, "raw", "lst", sep = ".")
        collection.file.name <- paste(collection.name, "Rda", sep = ".")

        assign(fluence.collection.name,
               source_mspct(mget(fluence.names)))
        assign(raw.collection.name,
               mget(raw.names))
        save(list = c(fluence.collection.name, raw.collection.name),
             file = collection.file.name)

        file.names <- c(file.names, collection.file.name)

        # clean up
        rm(list = c(fluence.names, raw.names))
        fluence.names <- character()
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

#' @rdname acq_fluence_interactive
#'
#' @param ref.value numeric or filter_spct/reflector_spct object.
#' @param ref.persistence integer Number of measurements using the same
#'   reference reading.
#' @param ref.absolute logical Flag indicating whether the reference is
#'   absolute or relative. In the last case, cps spectra are normalised.
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
#'   internal transmittance, and between total and specular reflectance.
#'   In both cases which of these is measured depends on the measuring protocol
#'   (condition used as reference, use of an integrating sphere versus use of a
#'   probe with a narrow angle of aperture, etc.) and consequently the correct
#'   value should be entered to ensure that data are correctly tagged and
#'   later computations valid.
#'
#' @export
#'
acq_fraction_pulsed_interactive <-
  function(integ.time = 2,
           num.scans = 1L,
           num.exposures = 1L,
           f.trigger.pulses = f.trigger.message,
           protocols = NULL,
           correction.method = NA,
           descriptors = NA,
           ref.value = 1,
           ref.absolute = TRUE,
           ref.persistence = 1,
           qty.out = "Tfr",
           type = "total",
           stray.light.method = "simple",
           save.pdfs = TRUE,
           interface.mode = "manual") {

    # validate quantity
    stopifnot(qty.out %in% c("Tfr", "Rfr", "raw"))

    # validate interface mode
    interface.mode <- tolower(interface.mode)
    if (!gsub("-attr$", "", interface.mode) %in%
        c("manual")) {
      stop("Invalid argument for 'interface.mode', aborting.")
    }

    # define measurement protocols
    if (length(protocols) == 0L) {
      protocols <- list(rsd = c("reference", "sample", "dark"),
                        rs = c("reference", "sample"))
    }

    w <- start_session()

    instruments <- list_srs_interactive(w = w)
    sr.index <- choose_sr_interactive(instruments = instruments)
    if (sr.index < 0L) {
      print("Aborting...")
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

    user.session.name <- readline("Session's name: ")
    session.name <- make.names(user.session.name)
    if (user.session.name == "") {
      session.name <- make.names(lubridate::now())
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

    settings <- acq_settings(descriptor = descriptor,
                             integ.time = integ.time,
                             target.margin = 0,
                             tot.time.range = integ.time,
                             HDR.mult = 1)

    seq.settings <- list(step = 0,
                         num.steps = 1L)

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

      if (grepl("-attr", interface.mode)) {
        user.attrs <- set_attributes_interactive(user.attrs)
      }

      raw.mspct <- acq_raw_mspct(descriptor = descriptor,
                                 acq.settings = settings,
                                 num.exposures = num.exposures,
                                 seq.settings = seq.settings,
                                 protocol = protocol,
                                 f.trigger.pulses = f.trigger.pulses,
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
                                            ref.value = ref.value,
                                            ref.absolute = ref.absolute)
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
