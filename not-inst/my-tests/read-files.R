library(photobiology)
library(readr)
library(ooacquire)
library(lubridate)

my.locale <- locale(decimal_mark = ",", tz = "EET")

old.wd <- setwd("data-raw/maya")
test.spct <- read_oo_ssdata(file = "cal2016b01.txt", locale = my.locale)
test.spct
getInstrDesc(test.spct)
getInstrSettings(test.spct)
getWhenMeasured(test.spct)
attributes(test.spct)
cat(comment(test.spct))

my.locale <- locale(decimal_mark = ".", tz = "CET")

setwd("../qe")
test.spct <- read_oo_ssdata(file = "spectrum.txt", locale = my.locale)
test.spct
getInstrDesc(test.spct)
getInstrSettings(test.spct)
getWhenMeasured(test.spct)
attributes(test.spct)
cat(comment(test.spct))

my.locale <- default_locale()

setwd("../pi")
test.spct <- read_oo_pidata(file = "vk_open_b00.txt", locale = my.locale)
test.spct
getInstrDesc(test.spct)
getInstrSettings(test.spct)
getWhenMeasured(test.spct)
attributes(test.spct)
cat(comment(test.spct))

test.spct <- read_oo_pidata(file = "vk_open_b_dark.txt", locale = my.locale)
test.spct
getInstrDesc(test.spct)
getInstrSettings(test.spct)
getWhenMeasured(test.spct)
attributes(test.spct)
cat(comment(test.spct))

# test date

# my.file <- "vk_open_b00.txt" # just to make easy to test different files

my.file <- "vk_open_b_dark.txt"

my.locale <- locale(decimal_mark = ".", tz = "EET")

test.spct <- read_oo_pidata(file = my.file, locale = my.locale)
getWhenMeasured(test.spct)

test.spct <- read_oo_pidata(file = my.file, locale = my.locale, date = lubridate::ymd_hm("2016-01-01 10:30", tz = "EET"))
getWhenMeasured(test.spct)

test.spct <- read_oo_pidata(file = my.file, locale = my.locale, date = NA)
getWhenMeasured(test.spct)

test.spct <- read_oo_pidata(file = my.file, locale = my.locale, date = NULL)
getWhenMeasured(test.spct)

setwd(old.wd)

