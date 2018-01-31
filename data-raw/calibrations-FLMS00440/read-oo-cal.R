# read calibration
library(readr)
library(magrittr)
library(ggplot2)
library(r4photobiology)
library(ooacquire)

read_oo_caldata("./data-raw/calibrations-FLMS00440/FLMS00440_2016-06-03_VIS-patched.IRRADCAL") %>%
  oo_calib2irrad_mult(diff.type = "UV-J1002-SMA") -> cal.spct

class_spct(cal.spct)
names(cal.spct)
cat(comment(cal.spct))
getWhenMeasured(cal.spct)
getWhatMeasured(cal.spct)

plot(cal.spct,
     annotations = c("+", "boundaries", "title:what:when"),
     range = c(355, 850))

save(cal.spct, file = "./data-raw/calibrations-FLMS00440/FLMS00440_2016-06-03_cal.rda")
