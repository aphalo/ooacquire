# Using a spectrometer with a single channel
# Tested with a Maya 2000 Pro spectrometer

library(ooacquire)

photon_as_default()

acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./inst-not/series",
                      tot.time.range = 5,
                      target.margin = .05,
                      qty.out = "cps",
                      HDR.mult = 1,
                      seq.settings = list(initial.delay = 10,
                                          step.delay = 10,
                                          num.steps = 5))

acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./inst-not/single",
                      tot.time.range = 5,
                      HDR.mult = c(1, 10))