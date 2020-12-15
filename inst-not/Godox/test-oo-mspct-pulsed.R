library(photobiology)
library(photobiologyWavebands)
library(ggspectra)

#  rm(list = ls(pattern = "*"))

  files <- list.files("tests/testthat/test-fluence-mspct-data", pattern = "*.Rda")

  for (f in files) {
    print(f)
    load(paste("tests/testthat/test-fluence-mspct-data", f, sep = "/"))
    old.spct <- get(sub(".Rda", "", f))
    serial.no <- getInstrDesc(old.spct)$spectrometer.sn
    correction.method <-
      switch(serial.no,
             MAYP11278 = ooacquire::MAYP11278_ylianttila.mthd,
             MAYP112785 = ooacquire::MAYP112785_ylianttila.mthd,
             new_correction_method(descriptor,
                                   stray.light.method = NA)
      )
    print(getInstrDesc(old.spct))
    print(getInstrSettings(old.spct))
    old.PAR <- q_irrad(old.spct, PAR()) * 1e6
    old.raw.mspct <- get(sub("spct.Rda", "raw_mspct", f))
    new.spct <- s_irrad_corrected(old.raw.mspct, correction.method = correction.method)
    q_irrad(new.spct, PAR()) * 1e6 - old.PAR
    plot(new.spct)
    new.spct <- s_irrad_corrected(old.raw.mspct[c("light", "dark")], correction.method = correction.method)
    q_irrad(new.spct, PAR()) * 1e6 - old.PAR
    plot(new.spct)
    new.spct <- s_irrad_corrected(old.raw.mspct[c("light", "filter")], correction.method = correction.method)
    q_irrad(new.spct, PAR()) * 1e6 - old.PAR
    plot(new.spct)
    new.spct <- s_irrad_corrected(old.raw.mspct[c("light")], correction.method = correction.method)
    q_irrad(new.spct, PAR()) * 1e6 - old.PAR
    plot(new.spct)
    new.spct <- s_irrad_corrected(old.raw.mspct[c("dark")], spct.names = c(light = "dark"), correction.method = correction.method)
    q_irrad(new.spct, PAR()) * 1e6
    plot(new.spct)
    new.spct <- s_irrad_corrected(old.raw.mspct[c("dark")], correction.method = correction.method)
#    q_irrad(new.spct, PAR())
    plot(new.spct)
  }

