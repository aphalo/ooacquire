library(r4photobiology)
library(ooacquire)

felflame.raw_mspct <-
  read_files2mspct(
    list(light = "./data-raw/flame-FLMS00673/felflame01 (2016_08_07 14_23_48 UTC).txt",
         dark = "./data-raw/flame-FLMS00673/felflame03 (2016_08_07 14_23_48 UTC).txt")
  )

felflame.raw_mspct <- msmsply(felflame.raw_mspct, .fun = setInstrDesc, instr.desc = FLMS00673_descriptors[[1]])
lapply(felflame.raw_mspct, getInstrDesc)
plot(felflame.raw_mspct[[1]])
plot(felflame.raw_mspct[[2]])

felflame.cps_mspct <- raw2cps(felflame.raw_mspct)
plot(felflame.cps_mspct[[1]])
plot(felflame.cps_mspct[[2]])

plot(cps2irrad(felflame.cps_mspct$light))
