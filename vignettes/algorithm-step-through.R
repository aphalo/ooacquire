## ---- echo=FALSE, message=FALSE------------------------------------------
require("knitr")
dirpath <- system.file("extdata", package="ooacquire")
opts_knit$set(autodep = TRUE, root.dir = dirpath, fig.width=8, fig.asp=0.5, out.width = '90%')


## ------------------------------------------------------------------------
library(ggplot2)
library(ggspectra)
library(photobiology)
library(photobiologyWavebands)
library(ooacquire)

## ------------------------------------------------------------------------
load("white-LED-lamp.Rda")
ls()

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.spct
plot(LED_lamp04_long.spct)

## ------------------------------------------------------------------------
LED_lamp04_long.raw_spct

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(LED_lamp04_long.raw_spct[["light"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(LED_lamp04_long.raw_spct[["dark"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.raw_spct[["light"]] <-
  skip_bad_pixs(LED_lamp04_long.raw_spct[["light"]])
plot(LED_lamp04_long.raw_spct[["light"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.raw_spct[["dark"]] <-
  skip_bad_pixs(LED_lamp04_long.raw_spct[["dark"]])
plot(LED_lamp04_long.raw_spct[["dark"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.raw_spct[["light"]] <-
  trim_counts(LED_lamp04_long.raw_spct[["light"]])
plot(LED_lamp04_long.raw_spct[["light"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.raw_spct[["light"]] <-
  bleed_nas(LED_lamp04_long.raw_spct[["light"]])
plot(LED_lamp04_long.raw_spct[["light"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.raw_spct[["light"]] <-
  linearize_counts(LED_lamp04_long.raw_spct[["light"]])
plot(LED_lamp04_long.raw_spct[["light"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.raw_spct[["dark"]] <-
  linearize_counts(LED_lamp04_long.raw_spct[["dark"]])
plot(LED_lamp04_long.raw_spct[["dark"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.raw_spct[["light"]] <-
  fshift(LED_lamp04_long.raw_spct[["light"]], range = c(250,290))
plot(LED_lamp04_long.raw_spct[["light"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.raw_spct[["dark"]] <-
  fshift(LED_lamp04_long.raw_spct[["dark"]], range = c(250,290))
plot(LED_lamp04_long.raw_spct[["dark"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.cps_spct <- cps_mspct()
LED_lamp04_long.cps_spct[["light"]] <-
  raw2cps(LED_lamp04_long.raw_spct[["light"]])
plot(LED_lamp04_long.cps_spct[["light"]])


## ---- fig.width=8, fig.asp=0.5-------------------------------------------
ggplot(LED_lamp04_long.cps_spct[["light"]], aes(x = w.length)) +
  geom_line(aes(y = cps_1), color = "blue") +
  geom_line(aes(y = cps_2), color = "red") +
  geom_line(aes(y = cps_3), color = "green")


## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.cps_spct[["dark"]] <-
  raw2cps(LED_lamp04_long.raw_spct[["dark"]])
plot(LED_lamp04_long.cps_spct[["dark"]])


## ---- fig.width=8, fig.asp=0.5-------------------------------------------
ggplot(LED_lamp04_long.cps_spct[["dark"]], aes(x = w.length)) +
  geom_line(aes(y = cps_1), color = "blue") +
  geom_line(aes(y = cps_2), color = "red") +
  geom_line(aes(y = cps_3), color = "green")


## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.cps_spct[["light"]] <-
  merge_cps(LED_lamp04_long.cps_spct[["light"]])
plot(LED_lamp04_long.cps_spct[["light"]])


## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.cps_spct[["dark"]] <-
  merge_cps(LED_lamp04_long.cps_spct[["dark"]])
plot(LED_lamp04_long.cps_spct[["dark"]])


## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp.cps_spct <- 
  LED_lamp04_long.cps_spct[["light"]] - LED_lamp04_long.cps_spct[["dark"]]
LED_lamp.cps_spct <- 
  copy_attributes(LED_lamp04_long.cps_spct[["light"]],
                  LED_lamp.cps_spct,
                  copy.class = FALSE)
plot(LED_lamp.cps_spct)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp.spct <- cps2irrad(LED_lamp.cps_spct)
plot(LED_lamp.spct)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp.spct <- smooth_spct(LED_lamp.spct)
plot(LED_lamp.spct)

