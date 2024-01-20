library(ooacquire)
library(lubridate)

raw.mspct <- test_007.raw_mspct

correction.method <- ooacquire::MAYP11278_ylianttila.mthd

spct.names <-
  list(light = grep("^light", names(raw.mspct), value = TRUE),
       filter = "filter",
       dark = "dark")

disable_check_spct()
start.time <- now()
irrad.spct <- s_irrad_corrected(x = raw.mspct,
                                spct.names = spct.names,
                                correction.method = correction.method)

end.time <- now()
elapsed_check_disabled <- end.time - start.time
cat("Elapsed time, checks disabled:", format(elapsed_check_disabled), "\n")

enable_check_spct()
start.time <- now()
irrad.spct <- s_irrad_corrected(x = raw.mspct,
                                spct.names = spct.names,
                                correction.method = correction.method)

end.time <- now()
elapsed_check_enabled <- end.time - start.time
cat("Elapsed time, checks enabled:", format(elapsed_check_enabled), "\n")


