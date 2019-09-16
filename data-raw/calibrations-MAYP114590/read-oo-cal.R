# read calibration
library(magrittr)
library(ggplot2)
library(r4photobiology)
library(ooacquire)
library(lubridate)

read_oo_caldata("./data-raw/calibrations-MAYP114590/MAYP114590_cus_20190806.IRRADCAL") %>%
  oo_calib2irrad_mult(diff.type = "D7-H-SMA") -> cal.spct

class_spct(cal.spct)
names(cal.spct)
cat(comment(cal.spct))
setWhenMeasured(cal.spct, mdy("8/6/2019"))
getWhenMeasured(cal.spct)
getWhatMeasured(cal.spct)

plot(cal.spct,
     annotations = c("+", "boundaries", "title:what:when"))

plot(cal.spct,
     annotations = c("+", "boundaries", "title:what:when"),
     range = c(230, 850), norm = 500)

save(cal.spct, file = "./data-raw/calibrations-MAYP114590/MAYP114590_2019-08-06_cal.rda")
