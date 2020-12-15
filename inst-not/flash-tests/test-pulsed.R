# test flash measurements
library(photobiology)
library(ooacquire)
library(ggspectra)

setwd("./not-inst/flash-tests")
load("PLX.1C33.yellow.spct.Rda")

light.spct <- PLX.1C33.yellow.raw_mspct$light
setts <- getInstrSettings(light.spct)
setts <- set_num_exposures(setts, 1L, TRUE)
str(setts)
setInstrSettings(light.spct, setts)
setTimeUnit(light.spct, "exposure")
getTimeUnit(light.spct)
getInstrSettings(light.spct)$num.exposures

dark.spct <- PLX.1C33.yellow.raw_mspct$dark
setts <- getInstrSettings(dark.spct)
ooacquire:::set_num_exposures(setts, 1L, TRUE)
setInstrSettings(dark.spct, setts)
setTimeUnit(dark.spct, "exposure")
getTimeUnit(dark.spct)

light.cps <- raw2corr_cps(light.spct)
getTimeUnit(light.cps)
dark.cps <- raw2corr_cps(dark.spct)
getTimeUnit(dark.cps)

plot(light.cps)
plot(dark.cps)
z.cps <- light.cps - dark.cps
plot(z.cps)
light.irrad <- cps2irrad(light.cps)
dark.irrad <- cps2irrad(dark.cps)
plot(light.irrad)
plot(dark.irrad)
plot(light.irrad - dark.irrad)
