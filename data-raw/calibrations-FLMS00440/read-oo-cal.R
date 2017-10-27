# read calibration
library(readr)
library(magrittr)
library(ggplot2)
library(r4photobiology)
library(ooacquire)

read_oo_caldata("FLMS00440_2016-06-03_UV.IRRADCAL") %>%
  oo_calib2irrad_mult(diff.type = "UV-J1002-SMA") -> cal.spct

# the two calibrations files have different values for the same wavelengths
# cal_uv.tb <- read_csv("FLMS00440_2016-06-03_UV.IRRADCAL", skip = 9, col_names = c("w'length", "irrad.mult1"))
# cal_oo.tb <- read_csv("FLMS00440_2016-06-03_OOIIrrad.cal", skip = 9, col_names = "irrad.mult2")
#
# cal_zz.spct <- cbind(cal_uv.spct, cal_oo.tb)

class_spct(cal.spct)
names(cal.spct)
cat(comment(cal.spct))
getWhenMeasured(cal.spct)
getWhatMeasured(cal.spct)

plot(cal.spct,
     annotations = c("+", "boundaries", "title:what:when"),
     range = c(280, 800))

save(cal.spct, file = "FLMS00440_2016-06-03_cal.rda")
