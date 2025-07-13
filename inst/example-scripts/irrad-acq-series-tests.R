# Using a spectrometer with a single channel
# Tested with a Maya 2000 Pro spectrometer

# This script can be used for interactive tests of the package functioning

library(ooacquire)

photon_as_default()

# half minute of measurements at 2 seconds interval
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./inst-not/series",
                      tot.time.range = 1,
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = 1L,
                      seq.settings = list(start.boundary = "second",
                                          initial.delay = 0,
                                          step.delay = 2,
                                          num.steps = 15),
                      entrance.optics = "cosine")

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
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./inst-not/single",
                      tot.time.range = c(5, 10),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(0.3, 1, 3, 10),
                      entrance.optics = "cosine")

# high speed measurements as fast as possible
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./inst-not/series",
                      tot.time.range = c(0.2, Inf),
                      target.margin = .2,
                      qty.out = "irrad",
                      HDR.mult = 1L,
                      seq.settings = list(start.boundary = "second",
                                          initial.delay = 0,
                                          step.delay = 0,
                                          num.steps = 100),
                      entrance.optics = "cosine")

# equivalent defaults for single time point acquisition
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./inst-not/single",
                      tot.time.range = c(0.2, Inf),
                      target.margin = .2,
                      qty.out = "irrad",
                      HDR.mult = 1L,
                      entrance.optics = "cosine")


