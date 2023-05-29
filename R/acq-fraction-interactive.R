#' Acquire spectral fraction
#'
#' Interactive front-end allowing acquisition of spectral fractions using Ocean
#' Optics spectrometers. Output of spectral data in R data files stored in
#' objects suitable for use with packages 'photobiology' and 'ggspectra' as well
#' as plots as PDF files and summaries as comma separated files.
#'
#' @details This function can be used to acquire spectral reflectance,
#'   spectral tarnsmittance and/or spectral absorptance using
#'   different protocols for acquisition and stray light and dark corrections.
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
#' @param ref.value numeric or filter_spct/reflector_spct object.
#' @param type character Type of transmittance or reflectance measured.
#' @param stray.light.method character Used only when the correction method is
#'   created on-the-fly.
#' @param qty.out character One of "Tfr" (spectral transmittance as a fraction
#'   of one), "irrad" (spectral irardiance), "cps" (counts per second), or "raw"
#'   (raw sensor counts).
#' @param save.pdfs logical Whether to save
#'   plots to PDFs files or not, and collection summaries to csv files or not,
#'   enable collections user interface or not..
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

    # Transfer focus to console (e.g., from editor pane)
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      rstudioapi::executeCommand('activateConsole')
    }

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
      session.name <- make.names(lubridate::now(tzone = "UTC"))
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

#' @rdname acq_fraction_interactive
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
