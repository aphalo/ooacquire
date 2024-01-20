library(microbenchmark)
library(ooacquire)

raw.mspct <- sun009.raw_mspct

spct.names <-
  list(light = grep("^light", names(raw.mspct), value = TRUE),
       filter = "filter",
       dark = "dark")

correction.method <- ooacquire::MAYP11278_ylianttila.mthd

irrad.spct <- s_irrad_corrected(x = raw.mspct,
                                spct.names = spct.names,
                                correction.method = correction.method)

microbenchmark(s_irrad_corrected(x = raw.mspct,
                                 spct.names = spct.names,
                                 correction.method = correction.method),
               times = 5)

irrad.spct <- s_irrad_corrected(x = raw.mspct,
                  spct.names = spct.names,
                  correction.method = correction.method)

library(profvis)

profvis({
  irrad.spct <- s_irrad_corrected(x = raw.mspct,
                                  spct.names = spct.names,
                                  correction.method = correction.method)
}, interval = 0.005, rerun = TRUE)
