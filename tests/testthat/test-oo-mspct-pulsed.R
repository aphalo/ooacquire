context("convert raw pulsed source")

# set to TRUE to reset snapshots
updating <- FALSE
debugging <- FALSE

library(photobiology)
# library(ggspectra)

# temporarily commented two tests that seem to be R version and/or OS dependent
test_that("raw from pulsed to fluence works", {

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
    new.spct <- clip_wl(new.spct, range = c(315, NA))
    expect_equal(round(wl_range(new.spct), 2), c(315.05, 898.81))
    expect_known_value(irrad(new.spct), file = paste("current-refs/ref-fluence", f, sep = "-"), update = updating)
#    expect_known_value(summary(new.spct), file = paste("current-refs/ref-summary", f, sep = "-"), update = updating)
    expect_known_value(wls_at_target(new.spct), file = paste("current-refs/ref-wls", f, sep = "-"), update = updating)
#    expect_known_value(getInstrSettings(new.spct), file = paste("ref", f, sep = "-"), update = updating)
    expect_known_value(new.spct, file = paste("current-refs/ref", f, sep = "-"), update = updating)

    if (debugging) cat(" <- ", serial.no, " file: ", f, "\n")
  }

})


test_that("raw from pulsed to Tfr works", {

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

    disable_check_spct()
    new.spct <- s_fraction_corrected(old.raw.mspct,
                                     correction.method = correction.method,
                                     type = "total",
                                     qty.out = "Tfr")
    new.spct <- trimInstrDesc(new.spct)
    new.spct <- trim_wl(new.spct, range = c(450, 900))
    new.spct <- clean(new.spct)
    new.spct <- smooth_spct(new.spct, method = "supsmu")
    enable_check_spct()
    expect_known_value(transmittance(new.spct), file = paste("current-refs/ref-tfr", f, sep = "-"), update = updating)
#    expect_known_value(summary(new.spct), file = paste("current-refs/ref-summary", f, sep = "-"), update = updating)
    expect_known_value(new.spct, file = paste("current-refs/ref", f, sep = "-"), update = updating)

    if (debugging) cat(" <- ", serial.no, " file: ", f, "\n")

  }
})

