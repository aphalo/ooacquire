# This script can be used for interactive tests of the acquisition of
# irradiance and fluence spectra with function acq_irrad_interactive()
#
# They mainly aim to test the user interface

library(ooacquire)

photon_as_default()

getwd()
oldwd <- setwd("./test-scripts-interactive/")
on.exit(setwd(oldwd))

if (!file.exists("defaults")) {dir.create("defaults")}
if (!file.exists("series")) {dir.create("series")}
if (!file.exists("auto")) {dir.create("auto")}
if (!file.exists("simple")) {dir.create("simple")}
if (!file.exists("options")) {dir.create("options")}

# defaults "irrad" --------------------------------------------------------

acq_irrad_interactive(folder.name = "./defaults/irrad-test")

acq_irrad_interactive(folder.name = "./defaults/fluence-test",
                      qty.out = "irrad")

# defaults "fluence" ------------------------------------------------------

acq_irrad_interactive(folder.name = "./defaults/fluence-test",
                      qty.out = "fluence")

# defaults "cps" ----------------------------------------------------------

acq_irrad_interactive(folder.name = "./defaults/cps-test",
                      HDR.mult = 0.67,
                      qty.out = "cps")

# defaults "raw" ----------------------------------------------------------

acq_irrad_interactive(folder.name = "./defaults/fluence-test",
                      HDR.mult = 0.67,
                      qty.out = "raw")

# time series and timed ---------------------------------------------------

# buffered, no HDR, step delay = 0
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series/buffered-test",
                      tot.time.range = c(0, 0.1),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = 1L,
                      seq.settings = list(start.boundary = "none",
                                          initial.delay = 0,
                                          step.delay = 0,
                                          num.steps = 25),
                      entrance.optics = "cosine")

# fast but not buffered, with HDR, step delay = 0
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series/fast-test",
                      tot.time.range = c(0, 0.1),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(1L, 2L),
                      seq.settings = list(start.boundary = "none",
                                          initial.delay = 0,
                                          step.delay = 0,
                                          num.steps = 25),
                      entrance.optics = "cosine")

# timed, no HDR, step delay = 0.2
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series/timed-test",
                      tot.time.range = c(0, 0.1),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = 1L,
                      seq.settings = list(start.boundary = "none",
                                          initial.delay = 0,
                                          step.delay = 0.3,
                                          num.steps = 10),
                      entrance.optics = "cosine")

# timed, no HDR, step delay = 0.2
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series/timed-delay-hdr-test",
                      tot.time.range = c(0, 0.1),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = 1L,
                      seq.settings = list(start.boundary = "none",
                                          initial.delay = 0,
                                          step.delay = 0.3,
                                          num.steps = 10),
                      entrance.optics = "cosine")

# timed single, no HDR, initial delay = 1
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series/timed-boundary-nohdr-test",
                      tot.time.range = c(0, 0.1),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = 1L,
                      seq.settings = list(start.boundary = "none",
                                          initial.delay = 1,
                                          step.delay = 0,
                                          num.steps = 1),
                      entrance.optics = "cosine")

# timed, with HDR, step delay = 0.2
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series/timed-delay-hdr-test",
                      tot.time.range = c(0, 0.1),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(1L, 2L),
                      seq.settings = list(start.boundary = "none",
                                          initial.delay = 0,
                                          step.delay = 0.3,
                                          num.steps = 10),
                      entrance.optics = "cosine")

# timed single, with HDR, initial delay = 1
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series/timed-boundary-hdr-test",
                      tot.time.range = c(0, 0.1),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(1L, 2L),
                      seq.settings = list(start.boundary = "none",
                                          initial.delay = 1,
                                          step.delay = 0,
                                          num.steps = 1),
                      entrance.optics = "cosine")

# timed single, with HDR, boundary = "second"
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series/timed-second-hdr-test",
                      tot.time.range = c(0, 0.1),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(1L, 2L),
                      seq.settings = list(start.boundary = "second",
                                          initial.delay = 0,
                                          step.delay = 0,
                                          num.steps = 1),
                      entrance.optics = "cosine")

# timed single, with HDR, boundary = "minute"
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series/timed-minute-hdr-test",
                      tot.time.range = c(0, 0.1),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(1L, 2L),
                      seq.settings = list(start.boundary = "second",
                                          initial.delay = 0,
                                          step.delay = 0,
                                          num.steps = 1),
                      entrance.optics = "cosine")

# A bit longer steps, a more realisitc example
# half minute of measurements every 2 seconds
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

# "quickly" test a long series, use strong light
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series/buffered-test",
                      tot.time.range = c(0, 0.01), # max 10ms
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = 1L,
                      seq.settings = list(start.boundary = "none",
                                          initial.delay = 0,
                                          step.delay = 0,
                                          num.steps = 1000),
                      entrance.optics = "cosine")

# series "cps" ------------------------------------------------------------

# "quickly" test a long series, use strong light
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series/cps-buffered-test",
                      tot.time.range = c(0, 0.01), # max 10ms
                      target.margin = .1,
                      qty.out = "cps",
                      HDR.mult = 1L,
                      seq.settings = list(start.boundary = "none",
                                          initial.delay = 0,
                                          step.delay = 0,
                                          num.steps = 1000),
                      entrance.optics = "cosine")

# series "raw" ------------------------------------------------------------

# "quickly" test a long series, use strong light
acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./series/raw-buffered-test",
                      tot.time.range = c(0, 0.01), # max 10ms
                      target.margin = .1,
                      qty.out = "raw",
                      HDR.mult = 1L,
                      seq.settings = list(start.boundary = "none",
                                          initial.delay = 0,
                                          step.delay = 0,
                                          num.steps = 1000),
                      entrance.optics = "cosine")


# auto "irrad" -----------------------------------------------------------

# HDR with 4 multipliers
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./auto/hdr-x4",
                      tot.time.range = c(5, 20),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(0.3, 1, 3, 10),
                      entrance.optics = "cosine")

# HDR with 2 multipliers
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./auto/hdr-x4",
                      tot.time.range = c(5, 20),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(1, 10),
                      entrance.optics = "cosine")

# no HDR
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./auto/hdr-x4",
                      tot.time.range = c(5, 20),
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(1, 10),
                      entrance.optics = "cosine")

# HDR with 4 multipliers, total time fixed
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./auto/hdr-x4",
                      tot.time.range = 10,
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(0.3, 1, 3, 10),
                      entrance.optics = "cosine")

# HDR with 2 multipliers, total time fixed
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./auto/hdr-x4",
                      tot.time.range = 10,
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(1, 10),
                      entrance.optics = "cosine")

# no HDR, total time fixed
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./auto/hdr-x4",
                      tot.time.range = 10,
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = c(1, 10),
                      entrance.optics = "cosine")

# auto "fluence" ----------------------------------------------------------

# 1 pulse
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./auto/fluence-x1",
                      tot.time.range = c(0, Inf),
                      qty.out = "fluence",
                      HDR.mult = 1L,
                      entrance.optics = "cosine",
                      num.exposures = 1L)

# 2 pulses
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./auto/fluence-x2",
                      tot.time.range = c(0, Inf),
                      qty.out = "fluence",
                      HDR.mult = 1L,
                      entrance.optics = "cosine",
                      num.exposures = 2L)

# auto "cps" ----------------------------------------------------------

# HDR with 2 multipliers
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./auto/cpshdr-x4",
                      tot.time.range = c(5, 20),
                      target.margin = .1,
                      qty.out = "cps",
                      HDR.mult = c(1, 10),
                      entrance.optics = "cosine")

# no HDR
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./auto/cps-hdr-x4",
                      tot.time.range = c(5, 20),
                      target.margin = .1,
                      qty.out = "cps",
                      HDR.mult = c(1, 10),
                      entrance.optics = "cosine")

# auto "raw" ----------------------------------------------------------

# HDR with 2 multipliers
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./auto/raw-hdr-x4",
                      tot.time.range = c(5, 20),
                      target.margin = .1,
                      qty.out = "raw",
                      HDR.mult = c(1, 10),
                      entrance.optics = "cosine")

# no HDR
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./auto/raw-hdr-x4",
                      tot.time.range = c(5, 20),
                      target.margin = .1,
                      qty.out = "raw",
                      HDR.mult = c(1, 10),
                      entrance.optics = "cosine")

# simple "irrad" ----------------------------------------------------------

# no HDR
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./simple/irrad-hdr-x4",
                      tot.time.range = c(5, 20),
                      qty.out = "irrad",
                      entrance.optics = "cosine")

# simple "fluence" --------------------------------------------------------

# no HDR
acq_irrad_interactive(interface.mode = "auto",
                      folder.name = "./simple/fluence-hdr-x4",
                      tot.time.range = c(5, 20),
                      qty.out = "irrad",
                      entrance.optics = "cosine")

# R options ---------------------------------------------------------------

options(digits.secs = 1)

acq_irrad_interactive(interface.mode = "series",
                      folder.name = "./options/digits.secs-test")

getOption("digits.secs") # should return 1
options(digits.secs = NULL)

options(warn = 0)

acq_irrad_interactive(folder.name = "./options/digits.secs-test")

getOption("warn") # should return 0

