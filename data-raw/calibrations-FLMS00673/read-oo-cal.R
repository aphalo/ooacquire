# read calibration
library(magrittr)
library(ggplot2)
library(r4photobiology)
library(ooacquire)

read_oo_caldata("FLMS00673_12022015.IrradCal") %>%
  oo_calib2irrad_mult(diff.type = "CC-3-DA") -> cal.spct

class_spct(cal.spct)
names(cal.spct)
cat(comment(cal.spct))
getWhenMeasured(cal.spct)
getWhatMeasured(cal.spct)

plot(cal.spct, annotations = c("+", "boundaries"))
plot(cal.spct, annotations = c("+", "boundaries"),
     range = c(280, 800))
ggplot(cal.spct) +
  geom_line()
