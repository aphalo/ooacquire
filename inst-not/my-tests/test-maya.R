library(photobiology)
library(readr)
library(ooacquire)
library(polynom)

my.locale <- locale(decimal_mark = ",", tz = "EET")

old.wd <- setwd("data-raw/maya")
test.spct <- read_oo_ssdata(file = "cal2016b01.txt", locale = my.locale)
test.spct
getInstrDesc(test.spct)
getInstrSettings(test.spct)
getWhenMeasured(test.spct)
attributes(test.spct)
cat(comment(test.spct))
raw2cps(test.spct)

# test.desc <- getInstrDesc(test.spct)
# nl.fun <- polynomial()
# set_descriptor_nl(test.desc, nl.fun)
# set_descriptor_wl(test.desc, wl)

setwd(old.wd)
