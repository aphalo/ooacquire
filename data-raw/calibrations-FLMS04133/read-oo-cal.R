# read calibration
library(magrittr)
library(ggplot2)
library(r4photobiology)
library(ooacquire)

read_oo_caldata("./data-raw/calibrations-FLMS04133/FLMS04133_20180320_CC.IrradCal") %>%
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
     range = c(230, 850), norm = 500)

save(cal.spct, file = "./data-raw/calibrations-FLMS04133/FLMS04133_2018-03-20_cal.rda")
