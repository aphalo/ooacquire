library(photobiology)
library(readr)
library(ooacquire)

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

my.locale <- locale(decimal_mark = ".", tz = "CET")

setwd("../qe")
test.spct <- read_oo_ssdata(file = "spectrum.txt", locale = my.locale)
test.spct
getInstrDesc(test.spct)
getInstrSettings(test.spct)
getWhenMeasured(test.spct)
attributes(test.spct)
cat(comment(test.spct))
raw2cps(test.spct)

my.locale <- default_locale()

setwd("../pi")
test.spct <- read_oo_pidata(file = "vk_open_b00.txt", locale = my.locale)
test.spct
getInstrDesc(test.spct)
getInstrSettings(test.spct)
getWhenMeasured(test.spct)
attributes(test.spct)
cat(comment(test.spct))
raw2cps(test.spct)

test.spct <- read_oo_pidata(file = "vk_open_b_dark.txt", locale = my.locale)
test.spct
getInstrDesc(test.spct)
getInstrSettings(test.spct)
getWhenMeasured(test.spct)
attributes(test.spct)
cat(comment(test.spct))
raw2cps(test.spct)

setwd(old.wd)
