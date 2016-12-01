library(photobiology)
library(ggspectra)
library(ooacquire)
library(anytime)

oldwd <- setwd(system.file("extdata/transmittance", package = "ooacquire"))

load(file = "UQG_Blue.spct.Rda")

UQG_Blue.mspct <- UQG_Blue.raw_spct

# examples of use with no bracketing

my.spct <- s_fraction_corrected(x = UQG_Blue.mspct,
                                descriptor = which_descriptor(getWhenMeasured(osram.mspct$light[[1]])),
                                method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(290, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
# getInstrDesc(my.spct)
# getInstrSettings(my.spct)
cat(comment(my.spct))

my.spct <- s_irrad_corrected(x = osram.mspct[c("light", "dark")],
                             descriptor = which_descriptor(getWhenMeasured(osram.mspct$light[[1]])),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(290, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

# examples of use with no bracketing with decimal dots

my.spct <- s_irrad_corrected(x = osram.mspct[c("light", "filter")],
                             descriptor = which_descriptor("2016-10-11"),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(290, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

setwd(oldwd)