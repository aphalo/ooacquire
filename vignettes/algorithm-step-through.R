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
library(magrittr)

## ------------------------------------------------------------------------
load("white-LED-lamp.Rda")
ls()

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.spct
plot(LED_lamp04_long.spct)
plot(LED_lamp04_long.spct, range = c(250, 410))

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp_recalc.spct <-
  s_irrad_corrected(LED_lamp04_long.raw_spct, 
                    method = ooacquire::MAYP11278_ylianttila.mthd)
plot(LED_lamp_recalc.spct)
plot(LED_lamp_recalc.spct, range = c(250, 410))

## ------------------------------------------------------------------------
LED_lamp04_long.raw_spct

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(LED_lamp04_long.raw_spct[["light"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(LED_lamp04_long.raw_spct[["dark"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(LED_lamp04_long.raw_spct)) {
  LED_lamp04_long.raw_spct[[m]] <-
    skip_bad_pixs(LED_lamp04_long.raw_spct[[m]])
  print(plot(LED_lamp04_long.raw_spct[[m]]) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(LED_lamp04_long.raw_spct)) {
  LED_lamp04_long.raw_spct[[m]] <-
    trim_counts(LED_lamp04_long.raw_spct[[m]])
  print(plot(LED_lamp04_long.raw_spct[[m]]) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(LED_lamp04_long.raw_spct)) {
  LED_lamp04_long.raw_spct[[m]] <-
    bleed_nas(LED_lamp04_long.raw_spct[[m]])
  print(plot(LED_lamp04_long.raw_spct[[m]]) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(LED_lamp04_long.raw_spct)) {
  LED_lamp04_long.raw_spct[[m]] <-
    linearize_counts(LED_lamp04_long.raw_spct[[m]])
  print(plot(LED_lamp04_long.raw_spct[[m]]) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(LED_lamp04_long.raw_spct)) {
  LED_lamp04_long.raw_spct[[m]] <-
    fshift(LED_lamp04_long.raw_spct[[m]], range = c(250,290))
  print(plot(LED_lamp04_long.raw_spct[[m]]) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp04_long.cps_spct <- cps_mspct()
for (m in names(LED_lamp04_long.raw_spct)) {
  LED_lamp04_long.cps_spct[[m]] <-
    raw2cps(LED_lamp04_long.raw_spct[[m]])
  print(
    ggplot(LED_lamp04_long.cps_spct[[m]], aes(x = w.length)) +
      geom_line(aes(y = cps_1), color = "blue") +
      geom_line(aes(y = cps_2), color = "red") +
      geom_line(aes(y = cps_3), color = "green") +
      ggtitle(m)
  )
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(LED_lamp04_long.cps_spct)) {
  LED_lamp04_long.cps_spct[[m]] <-
    merge_cps(LED_lamp04_long.cps_spct[[m]])
  print(plot(LED_lamp04_long.cps_spct[[m]]) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp.cps_spct <- 
  LED_lamp04_long.cps_spct[["light"]] - LED_lamp04_long.cps_spct[["dark"]]
LED_lamp.cps_spct <- 
  copy_attributes(LED_lamp04_long.cps_spct[["light"]],
                  LED_lamp.cps_spct,
                  copy.class = FALSE)
plot(LED_lamp.cps_spct) + ggtitle("Dark subtracted")

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp.spct <- cps2irrad(LED_lamp.cps_spct)
plot(LED_lamp.spct)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
LED_lamp.spct <- smooth_spct(LED_lamp.spct)
plot(LED_lamp.spct)

## ------------------------------------------------------------------------
load("halogen-lamp.Rda")
ls()

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
Halogen.spct
plot(Halogen.spct)
plot(Halogen.spct, range = c(250, 410))

