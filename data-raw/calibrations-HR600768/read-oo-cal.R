# read calibration
library(ggspectra)
library(photobiology)
library(ooacquire)

read_oo_caldata("./data-raw/oo-calibration-2025/HR600768_cus_20250507.IRRADCAL") |>
  oo_calib2irrad_mult() -> cal.spct

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
