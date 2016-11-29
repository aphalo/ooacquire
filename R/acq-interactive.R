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
#' @param method list The method to use when applying the calibration
#' @param descriptors list A list of instrument descriptors containing
#'   calibration data.
#' @export
#'
#' @note The integration time is set automatically so that the peak number of
#'   counts is close to 1 - \code{target.margin} of maximum range of the
#'   instrument detector. The minmum \code{tot.time} is achieved by increasing
#'   the number of scans.
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
           method = ooacquire::MAYP11278_ylianttila.mthd,
           descriptors = ooacquire::MAYP11278_descriptors) {

    descriptor <- which_descriptor(descriptors = descriptors)

    # define measurement protocols
    protocols <- list(l = "light",
                      ld = c("light", "dark"),
                      lf = c("light", "filter"),
                      lfd = c("light", "filter", "dark")
    )

    w <- start_session()

    list_instruments(w)

    descriptor.inst <- get_oo_descriptor(w)

    stopifnot(descriptor[["spectrometer.sn"]] == descriptor.inst[["spectrometer.sn"]])

    descriptor[["w"]] <- w

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

    protocol <- protocol_interactive(protocols)

    start.int.time <- 0.1 # seconds

    # data set measured with same protocol values but adjusted integration time

    settings <- acq_settings(descriptor,
                             start.int.time,
                             target.margin = target.margin,
                             tot.time.range = tot.time.range,
                             HDR.mult = HDR.mult)

    # save current value as starting value for next iteration

    repeat { # with same settings
      repeat{
        obj.name <- readline("Give a name to the spectrum: ")
        if (length(obj.name) > 0 && !exists(obj.name)) break()
        print("A valid and unique name is required, please try again...")
      }
      raw.name <- paste(obj.name, "raw_spct", sep = ".")
      irrad.name <- paste(obj.name, "spct", sep = ".")
      file.name <- paste(irrad.name, "Rda", sep = ".")
      pdf.name <- paste(irrad.name, "pdf", sep = ".")

      settings <- tune_interactive(descriptor = descriptor, settings = settings)

      raw.mspct <- acq_raw_mspct(descriptor,
                                 settings,
                                 protocol,
                                 user.label = obj.name)

      irrad.spct <- s_irrad_corrected(raw.mspct, method)

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
        grDevices::pdf(file = pdf.name, width = 8, height = 6)
        print(fig)
        grDevices::dev.off()
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
#' @param qty.out chearacter One of "Tfr" (transmittance as a fraction of one)
#'   or "raw" (raw counts).
#'
#' @export
#'
acq_trans_interactive <-
  function(tot.time.range = c(5, 15),
           target.margin = 0.2,
           HDR.mult = c(short = 1, long = 10),
           method = ooacquire::MAYP11278_ylianttila.mthd,
           descriptors = ooacquire::MAYP11278_descriptors,
           qty.out = "Tfr") {

    descriptor <- which_descriptor(descriptors = descriptors)

    # define measurement protocols
    protocols <- list(rsd = c("reference", "sample", "dark"),
                      srd = c("sample", "reference", "dark"))

    w <- start_session()

    list_instruments(w)

    descriptor.inst <- get_oo_descriptor(w)

    stopifnot(descriptor[["spectrometer.sn"]] == descriptor.inst[["spectrometer.sn"]])

    descriptor[["w"]] <- w

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

    protocol <- protocol_interactive(protocols)

    start.int.time <- 1 # seconds

    settings <- acq_settings(descriptor,
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
      raw.name <- paste(obj.name, "raw_spct", sep = ".")
      filter.name <- paste(obj.name, "spct", sep = ".")
      file.name <- paste(filter.name, "Rda", sep = ".")
      pdf.name <- paste(filter.name, "pdf", sep = ".")

      settings <- tune_interactive(descriptor = descriptor, settings = settings)

      raw.mspct <- acq_raw_mspct(descriptor,
                                 settings,
                                 protocol,
                                 user.label = obj.name)

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
        filter.spct <- s_fraction_corrected(raw.mspct, method = method, qty.out = "Tfr")
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
      grDevices::pdf(file = pdf.name, width = 8, height = 6)
      print(fig)
      grDevices::dev.off()

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
  tuned <- FALSE
  repeat{
    answ <- readline("Ready to adjust integration time? t = tune, s = skip, m = margin, r = range (t/s/m/-): ")
    if (answ == "") {
      answ <- ifelse(tuned, "s", "t")
    }
    if (answ == "t") {
      settings <- tune_acq_settings(descriptor, settings)
      tuned <- TRUE
    } else if (answ == "s") {
      break()
    } else if (answ == "m") {
      margin <- readline(sprintf("Saturation margin = %.2g, new: ",
                                 settings[["target.margin"]]))
      margin <- try(as.numeric(margin))
      if (!is.na(margin)) {
        settings[["target.margin"]] <- margin
      }
    }  else if (answ == "a") {
      return()
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
    if (length(user.input[1]) == 0) {
      protocol.name <- names(protocols)[[1]]
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

