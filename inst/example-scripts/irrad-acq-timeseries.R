# Using a spectrometer with a single channel
# Tested with a Maya 2000 Pro spectrometer

# This script can be used for interactive tests of the package functioning

library(ooacquire)

photon_as_default()

# one hour of measurements every minute
# Before starting make sure spectrometer temperature is stable!!

acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./inst-not/series",
                      tot.time.range = c(5, 10),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(0.3, 1, 3, 10),
                      seq.settings = list(start.boundary = "minute",
                                          initial.delay = 0,
                                          step.delay = 60,
                                          num.steps = 60),
                      entrance.optics = "cosine")

# equivalent defaults for single time point acquisition
acq_irrad_interactive(interface.mode = "full",
                      folder.name = "./inst-not/single",
                      tot.time.range = c(5, 10),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(0.3, 1, 3, 10),
                      entrance.optics = "cosine")

