#' Acquire spectra interactively
#'
#' Function providing an interactive front-end to the functions in the
#' package. Mostly as example code that can be modified for diferent
#' uses.
#'
#' @param min.tot.time numeric Minimum total time for a measurement in seconds
#' @param method char The method to use when applying the calibration
#' @param HDR.mult numeric the integration time for the each integration
#' as a multiplier of the short integration time.
#'
#' @export
#'
#' @import photobiologyWavebands photobiologygg
#'
#' @seealso For the available methods see \code{\link{process_maya_arrays}}.
#'
#' @note The integration time is set automatically so that the
#' peak number of counts is not less than about 85 % of maximum
#' range of the instrument detector. The \code{min.tot.time} is
#' obtained by adjusting the number of scans.
#'
#' @examples
#'
#' \dontrun{
#' # requires a spectrometer to be connected via USB
#' acquire_interactive(min.tot.time = 0, method = "raw", HDR.mult = 1)
#' }
#'
acq_interactive <- function(min.tot.time = 5, method = "full",
                                HDR.mult = c(short = 1, long = 10)) {

  # define measurement protocols
  protocols <- list(md = c("meas", "dark1"),
                    mfd = c("meas", "filter", "dark1"),
                    dmfd = c("dark1", "meas", "filter", "dark2"),
                    m = c("meas")
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
    protocol.name <- readline("Protocol: m, md, mfd, dmfd: ")
    if (length(protocol.name) == 0) {
      protocol.name <- "mfd"
    }
    if (length(protocol.name) > 0) {
      protocol <- protocols[[protocol.name[1]]]
      if (readline(paste("Will use protocol ",
                         paste(protocol, collapse = " -> "),
                         " o.k.? (-/n): ", sep = "")) != "n") {
        break()
      }
    }
  }

  start.int.time <- 0.1 # seconds

  repeat { # data set measured with same protocol values but adjusted integration time

    spc.set.ls <- set_params_raw_spc(spc.init.ls,
                                     start.int.time = start.int.time,
                                     min.tot.time = min.tot.time,
                                     HDR.mult = HDR.mult,
                                     user.label = paste(session.label))

    # save current value as starting value for next iteration

    start.int.time <- spc.set.ls$settings$integ.time[1] * 1e-6

    repeat { # with same settings
      repeat{
        obj.name <- readline("Give a name to the spectrum:")
        if (length(obj.name) > 0 && !exists(obj.name)) break()
        print("A valid and unique name is required, please try again...")
      }
      spc.meas.ls <- copy_settings(spc.set.ls)
      spc.meas.ls <- take_readings(spc.meas.ls, protocol, add.label=obj.name)
      meas.spct <- process_maya_raw_ls(spc.meas.ls, method = method)
      setSourceSpct(meas.spct)
      raw.name <- paste(obj.name, "raw_spct", sep = ".")
      result.name <- paste(obj.name, "spct", sep = ".")
      file.name <- paste(result.name, "Rda", sep = ".")
      pdf.name <- paste(result.name, "pdf", sep = ".")
      assign(raw.name, spc.meas.ls)
      assign(result.name, meas.spct)
      save(list = list(raw.name, result.name), file = file.name)
      repeat {
        fig <- plot(meas.spct) + theme_bw()
        print(fig)
        answer <- readline("Plot as photons/energy/wavebands/discard/save and continue (p/e/w/d/-): ")
        switch(answer,
               p = {options(photobiology.radiation.unit = "photon"); next()},
               e = {options(photobiology.radiation.unit = "energy"); next()},
               w = {answer1 <- readline("Waveband set to use, UV+PAR, plants, CIE, total, default (u/p/c/t/-)")
               switch(answer1,
                      u = options(photobiology.plot.bands =
                                    list(new_waveband(250.2,280, wb.name = "UVC'"), UVB(), UVA(), PAR())),
                      p = options(photobiology.plot.bands = Plant_bands()),
                      c = options(photobiology.plot.bands = VIS_bands()),
                      t = options(photobiology.plot.bands =
                                    list(new_waveband(min(meas.spct), max(meas.spct), wb.name = "Total"))),
                      options(photobiology.plot.bands = NULL))
               next()},
               d = break()
        )
        pdf(file = pdf.name, width = 8, height = 6)
        print(fig + labs(title=paste(obj.name, lubridate::now(), session.label, sep = ", ")))
        dev.off()
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
