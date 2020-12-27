library(photobiology)

load(file = "data-raw/raw-mspct-examples/Nichia.horticulture.5000K.spct.Rda")

white_LED.raw_mspct <- Nichia.horticulture.5000K.raw_mspct
rm(Nichia.horticulture.5000K.raw_mspct)

load(file = "data-raw/raw-mspct-examples/sun001.spct.Rda")

load(file = "data-raw/raw-mspct-examples/white-LED-lamp-2min.Rda")

white_LED_2min.raw_mspct <- msmsply(LED_lamp04_long.raw_spct, trimInstrDesc, fields = c("-", "w", "sr.index"))

white_LED_2min.raw_mspct <-
  msmsply(white_LED_2min.raw_mspct,
          setInstrDesc,
          MAYP11278_descriptors$cal_2016a)

rm(LED_lamp04_long.raw_spct, LED_lamp04_long.spct)

load(file = "data-raw/raw-mspct-examples/halogen-lamp.Rda")

halogen.raw_mspct <- msmsply(Halogen.raw_spct, trimInstrDesc, fields = c("-", "w", "sr.index"))

halogen.raw_mspct <-
  msmsply(halogen.raw_mspct,
          setInstrDesc,
          MAYP11278_descriptors$cal_2016a)

rm(Halogen.raw_spct, Halogen.spct)

load(file = "data-raw/raw-mspct-examples/flash-AD200.Rda")

xenon_flash.raw_mspct <- obj.reference.1.8.raw_mspct

settings <- getInstrSettings(xenon_flash.raw_mspct[["light"]])
settings <- ooacquire:::set_num_exposures(settings, 1L)

xenon_flash.raw_mspct[["light"]] <- setInstrSettings(xenon_flash.raw_mspct[["light"]], settings)
xenon_flash.raw_mspct[["dark"]] <- setInstrSettings(xenon_flash.raw_mspct[["dark"]], settings)

xenon_flash.raw_mspct[["light"]] <- setWhatMeasured(xenon_flash.raw_mspct[["light"]], "Bare bulb xenon flash")
xenon_flash.raw_mspct[["dark"]] <- setWhatMeasured(xenon_flash.raw_mspct[["dark"]], "Bare bulb xenon flash")

rm(obj.reference.1.8.raw_mspct, obj.reference.1.8.spct)


load(file = "data-raw/raw-mspct-examples/Heliopan695.Rda")

red_filter.raw_mspct <- msmsply(Heliopan695.raw_spct, trimInstrDesc, fields = c("-", "w", "sr.index"))

rm(Heliopan695.raw_spct, Heliopan695.spct)

load(file = "data-raw/raw-mspct-examples/UQG_Blue.spct.Rda")

blue_filter.raw_mspct <- msmsply(UQG_Blue.raw_spct, trimInstrDesc, fields = c("-", "w", "sr.index"))

rm(UQG_Blue.raw_spct)

save(white_LED.raw_mspct, sun001.raw_mspct, halogen.raw_mspct, xenon_flash.raw_mspct,
     red_filter.raw_mspct, blue_filter.raw_mspct,
     file = "data/raw-counts.rda",
     compress = "xz")

rm(list = ls(pattern = "*.raw_mspct"))

file.copy(from = "data-raw/raw-mspct-examples/white-LED-lamp.Rda",
          to = "inst/extdata",
          overwrite = TRUE,
          copy.date = TRUE)

file.copy(from = "data-raw/raw-mspct-examples/halogen-lamp.Rda",
          to = "inst/extdata",
          overwrite = TRUE,
          copy.date = TRUE)
