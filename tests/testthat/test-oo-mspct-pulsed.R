context("convert raw pulsed source")

library(photobiology)
# library(ggspectra)

test_that("ooacquire fluence", {

  rm(list = ls(pattern = "*"))

  files <- list.files("test-fluence-mspct-data", pattern = "*.Rda")

  for (f in files) {
    load(paste("test-fluence-mspct-data", f, sep = "/"))
    old.raw.mspct <- get(sub("spct.Rda", "raw_mspct", f))
    serial.no <- getInstrDesc(old.raw.mspct[[1]])$spectrometer.sn
    correction.method <-
      switch(serial.no,
             MAYP11278 = ooacquire::MAYP11278_ylianttila.mthd,
             MAYP112785 = ooacquire::MAYP112785_ylianttila.mthd,
             new_correction_method(descriptor,
                                   stray.light.method = NA)
      )
    new.spct <- s_irrad_corrected(old.raw.mspct, correction.method = correction.method)
    new.spct <- trimInstrDesc(new.spct) # needed to avoid futile call to .jcall
    expect_known_value(new.spct, file = paste("ref", f, sep = "-"), update = TRUE)
  }
})


test_that("ooacquire filter pulsed source", {

  rm(list = ls(pattern = "*"))

  files <- list.files("test-filter-pulsed-mspct-data", pattern = "*.Rda")

  for (f in files) {
    load(paste("test-filter-pulsed-mspct-data", f, sep = "/"))
    old.raw.mspct <- get(sub("spct.Rda", "raw_spct", f))
    serial.no <- getInstrDesc(old.raw.mspct[[1]])$spectrometer.sn
    ch.index <- getInstrDesc(old.raw.mspct[[1]])$ch.index
    correction.method <-
      switch(serial.no,
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
                                   stray.light.method = NA)
      )

    new.spct <- s_fraction_corrected(old.raw.mspct,
                                     correction.method = correction.method,
                                     type = "total",
                                     qty.out = "Tfr")
    new.spct <- trim_wl(new.spct, range = c(400:1100))
    print(getWhenMeasured(new.spct))
#    expect_known_value(new.spct, file = paste("ref", f, sep = "-"), update = TRUE)
  }
})

