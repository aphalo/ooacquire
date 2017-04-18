## ---- echo=FALSE, message=FALSE------------------------------------------
require("knitr")
dirpath <- system.file("extdata", package="ooacquire")
opts_knit$set(autodep = TRUE, root.dir = dirpath, fig.width=8, fig.asp=0.5, out.width = '90%')
options(photobioloy.verbose = TRUE)

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
LED_lamp04_long.raw_spct <- 
  msmsply(LED_lamp04_long.raw_spct, 
          setInstrDesc,   MAYP11278_descriptors$cal_2016a)
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

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
Halogen.raw_spct <-msmsply(Halogen.raw_spct, setInstrDesc, MAYP11278_descriptors$cal_2016a)
Halogen_recalc.spct <-
  s_irrad_corrected(Halogen.raw_spct, method = MAYP11278_ylianttila.mthd)
plot(Halogen_recalc.spct)
plot(Halogen_recalc.spct, range = c(250, 410))

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
Halogen_recalc_simple.spct <-
  s_irrad_corrected(Halogen.raw_spct, method = MAYP11278_simple.mthd)
plot(Halogen_recalc_simple.spct)
plot(Halogen_recalc_simple.spct, range = c(250, 410))

## ------------------------------------------------------------------------
Halogen.raw_spct

## ------------------------------------------------------------------------
Halogen.cps_spct <- cps_mspct()
for (m in names(Halogen.raw_spct)) {
  Halogen.raw_spct[[m]] %>%
    skip_bad_pixs() %>%
    trim_counts() %>%
    bleed_nas() %>%
    linearize_counts() %>%
    fshift(range = c(187.82,189.26)) %>%
    raw2cps() %>% 
    merge_cps() -> Halogen.cps_spct[[m]]
}
Halogen.cps_spct

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(Halogen.cps_spct)) {
  print(plot(Halogen.cps_spct[[m]]) + ggtitle(m))
  print(plot(Halogen.cps_spct[[m]], range = c(250, 410)) + ggtitle(m))
  print(plot(Halogen.cps_spct[[m]], range = c(500, 520)) + ggtitle(m))
}

## ------------------------------------------------------------------------
Halogen01.cps_spct <- cps_mspct()
for (m in setdiff(names(Halogen.cps_spct), "dark")) {
  Halogen01.cps_spct[[m]] <- Halogen.cps_spct[[m]] - Halogen.cps_spct[["dark"]]
  Halogen01.cps_spct[[m]] <- 
    copy_attributes(Halogen.cps_spct[[m]],
                    Halogen01.cps_spct[[m]],
                    copy.class = FALSE)
}
Halogen01.cps_spct

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(Halogen01.cps_spct)) {
  print(plot(Halogen01.cps_spct[[m]]) + ggtitle(m))
  print(plot(Halogen01.cps_spct[[m]], range = c(250, 410)) + ggtitle(m))
}

## ------------------------------------------------------------------------
Halogen_corrected.cps_spct <-
  filter_correction(Halogen01.cps_spct[["light"]], 
                    Halogen01.cps_spct[["filter"]],
                    stray.light.method = "original",
                    flt.Tfr = 1.4)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(Halogen_corrected.cps_spct)
plot(Halogen_corrected.cps_spct, range = c(250, 410))
mean(clip_wl(Halogen_corrected.cps_spct, range = c(250, 300))[["cps"]])

## ------------------------------------------------------------------------
cps2irrad(Halogen_corrected.cps_spct) %>%
  smooth_spct() -> Halogen.source_spct

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(Halogen.source_spct)
plot(Halogen.source_spct, range = c(250, 410))

