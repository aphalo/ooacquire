# read calibration
library(magrittr)
library(ggplot2)
library(r4photobiology)
library(ooacquire)

read_oo_caldata("./data-raw/calibrations-FLMS00416/FLMS00416_20150728_VIS.IRRADCAL") %>%
  oo_calib2irrad_mult(diff.type = "UV-J1002-SMA") -> cal.spct

class_spct(cal.spct)
names(cal.spct)
cat(comment(cal.spct))
getWhenMeasured(cal.spct)
getWhatMeasured(cal.spct)

plot(cal.spct,
     annotations = c("+", "boundaries", "title:what:when"))

plot(cal.spct,
     annotations = c("+", "boundaries", "title:what:when"),
     range = c(352, 920), norm = 520)

save(cal.spct, file = "./data-raw/calibrations-FLMS00416/FLMS00416_2015-07-28_cal.rda")
