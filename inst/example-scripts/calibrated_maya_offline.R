library(photobiology)
library(photobiologyPlants)
library(ggplot2)
library(ggspectra)
library(ooacquire)
library(readxl)


calibration.data <- read_excel("cal-coeffs.xlsx")

# get a descriptor from raw_spct object
descriptor <- getInstrDesc(Aralab.UV.4V1.spct)
descriptor[["inst.calib"]] <- list(z = NA)
# set Maya hot pixels and correct time limits
descriptor <- set_descriptor_wl(descriptor, calibration.data$w.length)
descriptor <- set_descriptor_irrad_mult(descriptor, calibration.data$multiplier * 1e4)
descriptor <- set_descriptor_bad_pixs(descriptor, c(123,380,1829,1994))

# # get a descriptor for the first channel of the first spectrometer
# descriptor <- get_oo_descriptor(w)
# # set Maya hot pixels and correct time limits
# descriptor <- set_descriptor_integ_time(descriptor, NA, 7.2)
# descriptor <- set_descriptor_wl(descriptor, calibration.data$w.length)
# descriptor <- set_descriptor_irrad_mult(descriptor, calibration.data$multiplier * 1e4)
# descriptor <- set_descriptor_bad_pixs(descriptor, c(123,380,1829,1994))
# # descriptor <- set_descriptor_slit_fun(descriptor, ooacquire::maya_tail_correction)
# #

## PROTOCOL 2
# acquire a measure in the light and a dark reference spectrum
# (The character vector for protocol can be of any length, and the strings
# can be anything you want. A prompt is issued if two consecutive strings
# are different. A protocol like c(rep("light", 20), "filter", "dark") is
# legal.)
#

# example list of file names
my.locale <- readr::locale("en", decimal_mark = ",", tz = "EET")

files <- list(light = "data-example/pheno1normal.txt",
              filter = "data-example/pheno1normaldark.txt",
              dark = "data-example/pheno1normaldark.txt")

files <- list(light = "data-example/pheno2normal.txt",
              filter = "data-example/pheno2normaldark.txt",
              dark = "data-example/pheno2normaldark.txt")

mspct_1 <- read_files2mspct(files, locale = my.locale)

mspct_1

## use new function
##

corrected.spct <-
  uvb_corrections(x = mspct_1[[1]],
                  flt = mspct_1[[2]],
                  dark = mspct_1[[3]],
                  method = "original",
                  stray.light.wl = c(218.5, 228.5),
                  flt.dark.wl = c(193, 209.5),
                  flt.ref.wl = c(360, 379.5),
                  worker_fun = ooacquire::maya_tail_correction,
                  trim = 0,
                  verbose = FALSE)
plot(corrected.spct)

temp <- getInstrDesc(corrected.spct)
temp[["inst.calib"]] <- descriptor[["inst.calib"]]

setInstrDesc(corrected.spct, temp)

irrad.spct <- cps2irrad(corrected.spct)

plot(irrad.spct)
plot(irrad.spct, unit.out = "photon", range = c(250, 800))

R_FR(irrad.spct)
Pfr_Ptot(irrad.spct)


