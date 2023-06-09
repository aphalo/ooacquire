context("convert raw continuous source")

# library(ggspectra)
test_that("ooacquire irrad continuous MAYA", {

  rm(list = ls(pattern = "*"))

  files <- list.files("test-irrad-mspct-maya-data", pattern = "*.[Rr]da")
  print(files)
  for (f in files) {
    load(paste("test-irrad-mspct-maya-data", f, sep = "/"))
    old.raw.mspct <- get(sub("spct.[Rr]da", "raw_mspct", f))
    serial.no <- getInstrDesc(old.raw.mspct[[1]])$spectrometer.sn
    correction.method <-
      switch(serial.no,
             MAYP11278 = ooacquire::MAYP11278_ylianttila.mthd,
             MAYP112785 = ooacquire::MAYP112785_ylianttila.mthd,
             MAYP114590 = ooacquire::MAYP114590_simple.mthd,
             new_correction_method(descriptor,
                                   stray.light.method = NA)
      )
    new.spct <- s_irrad_corrected(old.raw.mspct, correction.method = correction.method)
    new.spct <- trimInstrDesc(new.spct) # needed to avoid futile call to .jcall
    expect_known_value(new.spct, file = paste("ref", f, sep = "-"), update = TRUE)
  }

})

# Should be enabled after a few suitable files are added for the tests.
# test_that("ooacquire irrad continuous FLAME-S", {
#
#   rm(list = ls(pattern = "*"))
#
#   files <- list.files("test-irrad-mspct-flame-data", pattern = "*.Rda")
#
#   for (f in files) {
#     print(f)
#     load(paste("test-irrad-mspct-flame-data", f, sep = "/"))
# #    old.spct <- get(sub(".Rda", "", f))
#     serial.no <- getInstrDesc(old.spct)$spectrometer.sn
#     correction.method <-
#       switch(serial.no,
#              FLMS00673 = ooacquire::FLMS00673_none.mthd,
#              FLMS04133 = ooacquire::FLMS04133_none.mthd,
#              new_correction_method(descriptor,
#                                    stray.light.method = NA)
#       )
#     print(getInstrDesc(old.spct))
#     print(getInstrSettings(old.spct))
#     old.raw.mspct <- get(sub("spct.Rda", "raw_mspct", f))
#     new.spct <- s_irrad_corrected(old.raw.mspct, correction.method = correction.method, return.cps = TRUE)
#     expect_equivalent(old.spct, new.spct)
#   }
# })

test_that("ooacquire filter continuous", {

  rm(list = ls(pattern = "*"))

  files <- list.files("test-filter-mspct-data", pattern = "*.Rda")

  for (f in files) {
    load(paste("test-filter-mspct-data", f, sep = "/"))
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
    new.spct <- trim_wl(smooth_spct(new.spct, method = "supsmu", na.rm = TRUE),
                        range = c(450:800))
#    expect_known_value(new.spct, file = paste("ref", f, sep = "-"), update = TRUE)
  }
})

