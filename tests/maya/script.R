library(photobiology)
library(readr)
library(ooacquire)

my.locale <- locale(decimal_mark = ",", tz = "EET")

old.wd <- setwd("data-raw/maya")
test.spct <- read_oo_ssdata(file = "cal2016b01.txt", locale = my.locale)
test.spct
getWhenMeasured(test.spct)
attributes(test.spct)
cat(comment(test.spct))
test1.spct <- set_oo_ssdata_descriptor(test.spct)
getWhenMeasured(test1.spct)
attributes(test1.spct)
cat(comment(test1.spct))
test2.spct <- set_oo_ssdata_settings(test1.spct)
getWhenMeasured(test2.spct)
attributes(test2.spct)
cat(comment(test2.spct))

my.locale <- locale(decimal_mark = ".", tz = "CET")

setwd("../qe")
test.spct <- read_oo_ssdata(file = "spectrum.txt", locale = my.locale)
test.spct
getWhenMeasured(test.spct)
attributes(test.spct)
cat(comment(test.spct))
test1.spct <- set_oo_ssdata_descriptor(test.spct)
getWhenMeasured(test1.spct)
attributes(test1.spct)
cat(comment(test1.spct))
test2.spct <- set_oo_ssdata_settings(test1.spct)
getWhenMeasured(test2.spct)
attributes(test2.spct)
cat(comment(test2.spct))

setwd(old.wd)
