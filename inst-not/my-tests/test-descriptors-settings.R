library(photobiology)
library(ooacquire)
load("./not-inst/led_desk201.spct.Rda")
white_led.raw_spct <- led_desk201.raw_spct$light
white_led.source_spct <- led_desk201.spct
white_led.cps_spct <- s_irrad_corrected(led_desk201.raw_spct, method = MAYP11278_ylianttila.mthd, return.cps = TRUE)

str(getInstrDesc(white_led.raw_spct))
trimInstrDesc(white_led.raw_spct, fields = c("-", "w", "inst.calib"))
setInstrSettings(white_led.raw_spct, NA)
getInstrSettings(white_led.raw_spct)
str(white_led.raw_spct)

getInstrDesc(white_led.cps_spct)
trimInstrDesc(white_led.raw_spct, c("-", "w", "inst.calib"))
getInstrDesc(white_led.cps_spct)
setInstrDesc(white_led.cps_spct, NA)
getInstrDesc(white_led.cps_spct)
setInstrSettings(white_led.cps_spct, NA)
getInstrSettings(white_led.cps_spct)

setInstrDesc(white_led.source_spct, NA)
getInstrDesc(white_led.source_spct)
setInstrSettings(white_led.source_spct, NA)
getInstrSettings(white_led.source_spct)

save(white_led.raw_spct, white_led.cps_spct, white_led.source_spct, file = "white-led-spct.rda")
