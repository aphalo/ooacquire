# read calibration
library(magrittr)
library(ggplot2)
library(r4photobiology)
library(ooacquire)

read_oo_caldata("./data-raw/calibrations-FLMS00673/FLMS00673_12022015.IrradCal") %>%
  oo_calib2irrad_mult(diff.type = "CC-3-DA") -> cal.spct

class_spct(cal.spct)
names(cal.spct)
cat(comment(cal.spct))
getWhenMeasured(cal.spct)
getWhatMeasured(cal.spct)

plot(cal.spct,
     annotations = c("+", "boundaries", "title:what:when"))

plot(cal.spct,
     annotations = c("+", "boundaries", "title:what:when"),
     range = c(205, 925), norm = 500)

save(cal.spct, file = "./data-raw/calibrations-FLMS00673/FLMS00673_2015-12-02_cal.rda")
