## ----"setup", include=FALSE---------------------------------------------------
require("knitr")
opts_knit$set(cache = FALSE, root.dir = system.file("extdata", package = "ooacquire"))

## -----------------------------------------------------------------------------
library(photobiology)
library(photobiologyWavebands)
library(ggspectra)
library(ooacquire)

## -----------------------------------------------------------------------------
file_names <- list(light = c("foreign-files/light-short.txt",
                             "foreign-files/light-long.txt"),
                   filter = "foreign-files/flt-long.txt",
                   dark = c("foreign-files/dark-short.txt",
                            "foreign-files/dark-long.txt"))

## -----------------------------------------------------------------------------
irrad.spct <- 
  s_irrad_corrected(x = file_names,
                    descriptor = which_descriptor("2016-10-11", 
                                                  MAYP11278_descriptors),
                    correction.method = MAYP11278_ylianttila.mthd)

## ----fig.height=5, fig.width=7------------------------------------------------
autoplot(irrad.spct)

## -----------------------------------------------------------------------------
cps.spct <- 
  s_irrad_corrected(x = file_names,
                    descriptor = which_descriptor("2016-10-11", 
                                                  MAYP11278_descriptors),
                    correction.method = MAYP11278_ylianttila.mthd,
                    return.cps = TRUE)

## ----fig.height=5, fig.width=7------------------------------------------------
autoplot(cps.spct)

## ----eval=FALSE---------------------------------------------------------------
# cal.spct <- read_oo_caldata("oo-calibration/xxxx")

## ----eval=FALSE---------------------------------------------------------------
# cal.multipliers <- oo_calib2irrad_mult(cal.spct,
#                                        diff.type = "CC-3")

