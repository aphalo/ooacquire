#' Acquire spectra interactively
#'
#' Function providing a simple interactive front-end to the functions in the
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
  function(tot.time.range = c(1, 30),
           target.margin = 0.1,
           HDR.mult = c(short = 1, long = 10),
           method = ooacquire::MAYP11278_ylianttila.mthd,
           descriptors = ooacquire::MAYP11278_descriptors) {

  descriptor <- which_descriptor(descriptors)

  # define measurement protocols
  protocols <- list(l = "light",
                    ld = c("light", "dark"),
                    lf = c("light", "filter"),
                    lfd = c("light", "filter", "dark")
  )

  spc.init.ls <- start_session()

  session.label <- paste("Operator:", readline("Operator's name:"))

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

  repeat{
    protocol.name <- readline("Protocol: l, ld, lf, lfd: ")
    if (length(protocol.name) == 0) {
      protocol.name <- "lfd"
    }
    protocol <- protocols[[protocol.name[1]]]
    if (readline(paste("Will use protocol ",
                       paste(protocol, collapse = " -> "),
                       " o.k.? (-/n): ", sep = "")) != "n") {
      break()
    }
  }

  start.int.time <- 0.1 # seconds

  repeat { # data set measured with same protocol values but adjusted integration time

    settings <- acq_settings(descriptor,
                             start.int.time,
                             target.margin = target.margin,
                             tot.time.range = tot.time.range,
                             HDR.mult = HDR.mult)

    # save current value as starting value for next iteration

    repeat { # with same settings
      repeat{
        obj.name <- readline("Give a name to the spectrum:")
        if (length(obj.name) > 0 && !exists(obj.name)) break()
        print("A valid and unique name is required, please try again...")
      }
      raw.name <- paste(obj.name, "raw_spct", sep = ".")
      irrad.name <- paste(obj.name, "spct", sep = ".")
      file.name <- paste(irrad.name, "Rda", sep = ".")
      pdf.name <- paste(irrad.name, "pdf", sep = ".")

      repeat{
        answ <- readline("Ready to adjust integration time? t = tune, s = skip, a = abort")
        if (answ %in% c("t", "")) {
          settings <- tune_acq_settings(descriptor, settings)
        } else if (answ == "s") {
          break()
        } else if (answ == "a") {
          return()
        }
      }

      raw.mspct <- acq_raw_mspct(descriptor,
                                 settings,
                                 protocol,
                                 user.label = obj.name)

      irrad.spct <- s_irrad_corrected(raw.mspct, method)

      assign(raw.name, raw.mspct)
      assign(irrad.name, irrad.spct)

      save(list = list(raw.name, irrad.name), file = file.name)

      repeat {
        fig <- graphics::plot(irrad.spct) + ggplot2::theme_bw()
        print(fig)
        answer <- readline("Plot as photons/energy/wavebands/discard/save and continue (p/e/w/d/-): ")
        switch(answer,
               p = {options(photobiology.radiation.unit = "photon"); next()},
               e = {options(photobiology.radiation.unit = "energy"); next()},
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
                                    list(new_waveband(min(irrad.spct), max(irrad.spct), wb.name = "Total"))),
                      options(photobiology.plot.bands = NULL))
               next()},
               d = break()
        )
        grDevices::pdf(file = pdf.name, width = 8, height = 6)
        print(fig + ggplot2::labs(title=paste(obj.name, lubridate::now(), session.label, sep = ", ")))
        grDevices::dev.off()
        break()
      }
      if (!readline("Next (with same settings) (N/y)?: ") == "y") break()
    }
    if (readline("Next (new settings) (n/Y)?: ") == "n") break()
  }
  print("Ending...")
  end_session(spc.init.ls)
  setwd(oldwd)
  message("Folder reset to: ", getwd())
  message("Bye!")
}
