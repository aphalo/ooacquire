library(microbenchmark)
library(profvis)
library(ooacquire)

# 3000 spectra
load("./inst-not/irrad-series-2023-12-27/test_A_062.spct.Rda")
raw.mspct <- test_A_062.raw_mspct

# 100 spectra
# load("./inst-not/irrad-series-2023-12-27/test_D_002.spct.Rda")
# raw.mspct <- test_D_002.raw_mspct

# 10 spectra
# load("./inst-not/irrad-series-2023-12-27/test_D_003.spct.Rda")
# raw.mspct <- test_D_003.raw_mspct

format(object.size(raw.mspct), units = "MB")

getInstrSettings(raw.mspct$light.001)

spct.names <-
  list(light = grep("^light", names(raw.mspct), value = TRUE),
       filter = "filter",
       dark = "dark")

correction.method <- ooacquire::MAYP11278_ylianttila.mthd
# correction.method <- ooacquire::MAYP11278_simple.mthd

irrad.spct <- s_irrad_corrected(x = raw.mspct,
                                spct.names = spct.names,
                                correction.method = correction.method)

format(object.size(irrad.spct), units = "MB")

microbenchmark(s_irrad_corrected(x = raw.mspct,
                                 spct.names = spct.names,
                                 correction.method = correction.method),
               times = 5)


profvis({
  irrad.spct <- s_irrad_corrected(x = raw.mspct,
                                  spct.names = spct.names,
                                  correction.method = correction.method)
}, interval = 0.005, rerun = TRUE)
