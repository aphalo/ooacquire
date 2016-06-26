library(photobiologyInOut)
library(photobiology)
library(readr)

my.locale <- locale(decimal_mark = ",", tz = "EET")

old.wd <- setwd("data-raw/maya")
read_oo_ssdata()
