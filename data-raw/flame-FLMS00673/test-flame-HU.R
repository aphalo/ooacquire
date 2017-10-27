library(r4photobiology)
library(ooacquire)

felflame.raw_mspct <-
  read_files2mspct(
    list(light = "felflame01 (2016_08_07 14_23_48 UTC).txt",
         dark = "felflame03 (2016_08_07 14_23_48 UTC).txt")
  )


plot(felflame.raw_mspct[[1]])
plot(felflame.raw_mspct[[2]])

# Indian Flame
descriptor <- getInstrDesc(felflame.raw_mspct[[1]])
descriptor[["spectrometer.name"]] <- "Flame-S"
descriptor[["spectrometer.sn"]] <- "FLMS00440"
descriptor[["bench.grating"]] <- "03"
descriptor[["bench.filter"]] <- "000"
descriptor[["banch.slit"]] <- "025"
descriptor[["num.pixs"]] <- 2048
descriptor[["num.dark.pixs"]] <- 16
descriptor[["min.integ.time"]] <- 1000
descriptor[["max.integ.time"]] <- 65535000
descriptor[["max.counts"]] <- 65535
descriptor[["time"]] <- NA

settings <- getInstrSettings(felflame.raw_mspct[[1]])
settings[["integ.time"]] <- NA_real_
settings[["num.scans"]] <- NA_integer_
settings[["tot.time"]] <- NA_real_

save(descriptor, settings ,file = "FLMS00440.Rda")
