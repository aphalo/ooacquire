## ---- echo=FALSE, message=FALSE------------------------------------------
require("knitr")
dirpath <- system.file("extdata", package="ooacquire")
opts_knit$set(autodep = TRUE, fig.width=8, fig.asp=0.5, out.width = '90%')
# options(photobioloy.verbose = TRUE)

## ------------------------------------------------------------------------
library(ggplot2)
library(ggspectra)
library(photobiology)
library(photobiologyWavebands)
library(ooacquire)
library(magrittr)

## ------------------------------------------------------------------------
names(white_LED_2min.raw_mspct)

## ------------------------------------------------------------------------
names(white_LED_2min.raw_mspct[["light"]])

## ------------------------------------------------------------------------
summary(white_LED_2min.raw_mspct[["light"]])

## ------------------------------------------------------------------------
attr(white_LED_2min.raw_mspct[["light"]], which = "instr.desc")$spectrometer.name

## ---- eval=FALSE---------------------------------------------------------
#  # not run
#  summary(white_LED_2min.raw_mspct[["light"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp_recalc.spct <-
  s_irrad_corrected(white_LED_2min.raw_mspct, 
                    correction.method= ooacquire::MAYP11278_ylianttila.mthd)

## ------------------------------------------------------------------------
summary(LED_lamp_recalc.spct)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
autoplot(LED_lamp_recalc.spct)

