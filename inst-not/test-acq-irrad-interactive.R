## run tests
library(ooacquire)

# options(photobiology.verbose = TRUE)

photon_as_default()

# default for async.saves

acq_irrad_interactive(interface.mode = "series",
                      HDR.mult = 1,
                      tot.time.range = c(0, 10),
#                      verbose = TRUE, # report memory use
                      folder.name = sprintf("./inst-not/irrad-series-%s", lubridate::today()))

acq_irrad_interactive(interface.mode = "simple",
                      folder.name = sprintf("./inst-not/irrad-full-%s", lubridate::today()))

acq_irrad_interactive(interface.mode = "full",
                      folder.name = sprintf("./inst-not/irrad-full-%s", lubridate::today()))

acq_irrad_interactive(interface.mode = "auto",
                      folder.name = sprintf("./inst-not/irrad-auto-%s", lubridate::today()))

acq_irrad_interactive(qty.out = "fluence",
                      folder.name = sprintf("./inst-not/fluence-%s", lubridate::today()))

# enabled async.saves

acq_irrad_interactive(interface.mode = "series",
                      HDR.mult = 1,
#                      qty.out = "raw",
                      tot.time.range = c(0, 10),
                      async.saves = TRUE,
                      folder.name = sprintf("./inst-not/irrad-series-%s", lubridate::today()))

acq_irrad_interactive(interface.mode = "simple",
                      async.saves = TRUE,
                      folder.name = sprintf("./inst-not/irrad-full-%s", lubridate::today()))

acq_irrad_interactive(interface.mode = "full",
                      async.saves = TRUE,
                      folder.name = sprintf("./inst-not/irrad-full-%s", lubridate::today()))

acq_irrad_interactive(interface.mode = "auto",
                      async.saves = TRUE,
                      folder.name = sprintf("./inst-not/irrad-auto-%s", lubridate::today()))

acq_irrad_interactive(qty.out = "fluence",
                      async.saves = TRUE,
                      folder.name = sprintf("./inst-not/fluence-%s", lubridate::today()))
