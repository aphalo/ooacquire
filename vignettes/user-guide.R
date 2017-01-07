## ----"setup", include=FALSE----------------------------------------------
require("knitr")
opts_knit$set(root.dir = system.file("extdata/irradiance", package = "ooacquire"))

## ------------------------------------------------------------------------
library(photobiology)
library(photobiologyWavebands)
library(ggspectra)
library(ooacquire)

## ------------------------------------------------------------------------
files_names <- list(light = "light-short.txt")

