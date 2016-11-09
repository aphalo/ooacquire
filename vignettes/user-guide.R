## ----"setup", include=FALSE----------------------------------------------
require("knitr")
opts_knit$set(root.dir = system.file("extdata", package = "ooacquire"))

## ------------------------------------------------------------------------
library(photobiology)
library(ggspectra)
library(ooacquire)

## ------------------------------------------------------------------------
my.locale <- readr::locale("en", decimal_mark = ",", tz = "EET")


