# read calibration
library(magrittr)
library(ggplot2)
library(r4photobiology)
library(ooacquire)

read_oo_caldata("FLMS00416_20150728_VIS.IRRADCAL") %>%
  oo_calib2irrad_mult(diff.type = "UV-J1002-SMA") -> cal.spct

class_spct(cal.spct)
names(cal.spct)
cat(comment(cal.spct))
getWhenMeasured(cal.spct)
getWhatMeasured(cal.spct)

plot(cal.spct, annotations = c("+", "boundaries"),
     range = c(280, 800)) +
  ggtitle(getWhatMeasured(cal.spct)$sn, getWhatMeasured(cal.spct)$file.name)
