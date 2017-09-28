context("convert raw ooacquire")

library(photobiology)

test_that("ooacquire irrad", {

  rm(list = ls(pattern = "*"))

  files <- list.files("test-irrad-mspct-data", pattern = "*.Rda")

  for (f in files) {
    print(f)
    load(paste("test-irrad-mspct-data", f, sep = "/"))
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
    old.raw.mspct <- get(sub("spct.Rda", "raw_mspct", f))
    new.spct <- s_irrad_corrected(old.raw.mspct, correction.method = correction.method)
    expect_equivalent(old.spct, new.spct)
  }
})


test_that("ooacquire filter", {

  rm(list = ls(pattern = "*"))

  files <- list.files("test-filter-mspct-data", pattern = "*.Rda")

  for (f in files) {
    load(paste("test-filter-mspct-data", f, sep = "/"))
    old.spct <- get(sub(".Rda", "", f))
    ch.index <-
    serial.no <- getInstrDesc(old.spct)$spectrometer.sn
    ch.index <- getInstrDesc(old.spct)$ch.index
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

    old.raw.mspct <- get(sub("spct.Rda", "raw_spct", f))
    new.spct <- s_fraction_corrected(old.raw.mspct,
                                     correction.method = correction.method,
                                     type = "total",
                                     qty.out = "Tfr")
    # old.spct <- trim_wl(old.spct, range = c(400:1100))
    # new.spct <- trim_wl(new.spct, range = c(400:1100))
#    expect_equivalent(old.spct, new.spct)
  }
})

