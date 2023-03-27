# Using a spectrometer with a single channel
# Tested with a Maya 2000 Pro spectrometer

library(ooacquire)

photon_as_default()

acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series",
                      tot.time.range = 5,
                      qty.out = "raw",
                      HDR.mult = 1,
                      seq.settings = list(initial.delay = 0,
                                          step.delay = 10,
                                          num.steps = 10))

acq_irrad_interactive(interface.mode = "auto",
                      folder.name = ".",
                      tot.time.range = 5,
                      HDR.mult = 1)

acq_irrad_interactive(folder.name = "kkkk")
