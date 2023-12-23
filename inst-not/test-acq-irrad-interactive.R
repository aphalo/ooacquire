## run tests
library(ooacquire)

photon_as_default()

acq_irrad_interactive(interface.mode = "series",
                      HDR.mult = 1,
                      tot.time.range = c(0, 10),
                      folder.name = sprintf("./inst-not/irrad-series-%s", lubridate::today()))

acq_irrad_interactive(interface.mode = "simple",
                      folder.name = sprintf("./inst-not/irrad-full-%s", lubridate::today()))

acq_irrad_interactive(interface.mode = "full",
                      folder.name = sprintf("./inst-not/irrad-full-%s", lubridate::today()))

acq_irrad_interactive(interface.mode = "auto",
                      folder.name = sprintf("./inst-not/irrad-auto-%s", lubridate::today()))

acq_irrad_interactive(qty.out = "fluence",
                      folder.name = sprintf("./inst-not/fluence-%s", lubridate::today()))
