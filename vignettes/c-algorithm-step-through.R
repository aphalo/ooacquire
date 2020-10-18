## ---- echo=FALSE, message=FALSE-----------------------------------------------
require("knitr")
dirpath <- system.file("extdata", package="ooacquire")
opts_knit$set(autodep = TRUE, fig.width=8, fig.asp=0.5, out.width = '90%')
# options(photobioloy.verbose = TRUE)

## -----------------------------------------------------------------------------
library(ggplot2)
library(ggspectra)
library(photobiology)
library(photobiologyWavebands)
library(ooacquire)
library(magrittr)

## -----------------------------------------------------------------------------
names(white_LED.raw_mspct)

## -----------------------------------------------------------------------------
names(white_LED.raw_mspct[["light"]])

## -----------------------------------------------------------------------------
summary(white_LED.raw_mspct[["light"]])

## ---- eval=FALSE--------------------------------------------------------------
#  # not run
#  str(white_LED.raw_mspct[["light"]], max.level = 3)

## -----------------------------------------------------------------------------
attr(white_LED.raw_mspct[["light"]], which = "instr.desc")$spectrometer.name

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
LED_lamp_recalc.spct <-
  s_irrad_corrected(white_LED.raw_mspct, 
                    correction.method = ooacquire::MAYP11278_ylianttila.mthd)

## -----------------------------------------------------------------------------
summary(LED_lamp_recalc.spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(LED_lamp_recalc.spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
LED_lamp_recalc.spct <-
  s_irrad_corrected(white_LED.raw_mspct, 
                    correction.method = ooacquire::MAYP11278_sun.mthd)

## -----------------------------------------------------------------------------
summary(LED_lamp_recalc.spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(LED_lamp_recalc.spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
LED_lamp_recalc.cps_spct <-
  s_irrad_corrected(white_LED.raw_mspct, 
                    correction.method = ooacquire::MAYP11278_ylianttila.mthd,
                    return.cps = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  get_attributes(LED_lamp_recalc.cps_spct)

## -----------------------------------------------------------------------------
summary(LED_lamp_recalc.cps_spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(LED_lamp_recalc.cps_spct)

## -----------------------------------------------------------------------------
LED_lamp_recalc.source_spct <- cps2irrad(LED_lamp_recalc.cps_spct)

## -----------------------------------------------------------------------------
summary(LED_lamp_recalc.source_spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(LED_lamp_recalc.source_spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(white_LED.raw_mspct[["light"]])

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(white_LED.raw_mspct[["dark"]])

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
for (m in names(white_LED.raw_mspct)) {
  white_LED.raw_mspct[[m]] <-
    skip_bad_pixs(white_LED.raw_mspct[[m]])
  print(autoplot(white_LED.raw_mspct[[m]]) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
for (m in names(white_LED.raw_mspct)) {
  white_LED.raw_mspct[[m]] <-
    trim_counts(white_LED.raw_mspct[[m]])
  print(autoplot(white_LED.raw_mspct[[m]]) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
for (m in names(white_LED.raw_mspct)) {
  white_LED.raw_mspct[[m]] <-
    bleed_nas(white_LED.raw_mspct[[m]])
  print(autoplot(white_LED.raw_mspct[[m]]) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
for (m in names(white_LED.raw_mspct)) {
  white_LED.raw_mspct[[m]] <-
    linearize_counts(white_LED.raw_mspct[[m]])
  print(autoplot(white_LED.raw_mspct[[m]], ylim = c(NA, 65000)) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
for (m in names(white_LED.raw_mspct)) {
  white_LED.raw_mspct[[m]] <-
    fshift(white_LED.raw_mspct[[m]], range = c(218.5,228.5))
  print(autoplot(white_LED.raw_mspct[[m]], ylim = c(NA, 65000)) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
white_LED.cps_mspct <- cps_mspct()
for (m in names(white_LED.raw_mspct)) {
  white_LED.cps_mspct[[m]] <-
    raw2cps(white_LED.raw_mspct[[m]])
  print(
    ggplot(white_LED.cps_mspct[[m]], aes(x = w.length)) +
      ylim(NA, 200000) +
      geom_line(aes(y = cps_1), color = "blue") +
      geom_line(aes(y = cps_2), color = "red") +
      ggtitle(m)
  )
}

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
for (m in names(white_LED.cps_mspct)) {
  white_LED.cps_mspct[[m]] <-
    merge_cps(white_LED.cps_mspct[[m]])
  print(autoplot(white_LED.cps_mspct[[m]], ylim = c(NA, 200000)) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
white_LED.cps_spct <- 
  white_LED.cps_mspct[["light"]] - white_LED.cps_mspct[["dark"]]
white_LED.cps_spct <- 
  copy_attributes(white_LED.raw_mspct[["light"]],
                  white_LED.cps_spct,
                  copy.class = FALSE)
autoplot(white_LED.cps_spct) + ggtitle("Dark subtracted")

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
white_LED.spct <- cps2irrad(white_LED.cps_spct)
autoplot(white_LED.spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
white_LED.spct <- smooth_spct(white_LED.spct)
autoplot(white_LED.spct)

## -----------------------------------------------------------------------------
names(halogen.raw_mspct)

## -----------------------------------------------------------------------------
names(halogen.raw_mspct[["light"]])

## -----------------------------------------------------------------------------
summary(halogen.raw_mspct[["light"]])

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
halogen.spct <-
  s_irrad_corrected(halogen.raw_mspct, correction.method= MAYP11278_ylianttila.mthd)
autoplot(halogen.spct)

## -----------------------------------------------------------------------------
halogen.cps_mspct <- cps_mspct()
for (m in names(halogen.raw_mspct)) {
  halogen.raw_mspct[[m]] %>%
    skip_bad_pixs() %>%
    trim_counts() %>%
    bleed_nas() %>%
    linearize_counts() %>%
    fshift(range = c(187.82,189.26)) %>%
    raw2cps() %>% 
    merge_cps() -> halogen.cps_mspct[[m]]
}
names(halogen.cps_mspct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
for (m in names(halogen.cps_mspct)) {
  print(autoplot(halogen.cps_mspct[[m]]) + ggtitle(m))
  print(autoplot(halogen.cps_mspct[[m]], range = c(250, 410)) + ggtitle(m))
}

## -----------------------------------------------------------------------------
halogen01.cps_mspct <- cps_mspct()
for (m in setdiff(names(halogen.cps_mspct), "dark")) {
  halogen01.cps_mspct[[m]] <- halogen.cps_mspct[[m]] - halogen.cps_mspct[["dark"]]
  halogen01.cps_mspct[[m]] <- 
    copy_attributes(halogen.cps_mspct[[m]],
                    halogen01.cps_mspct[[m]],
                    copy.class = FALSE)
}
names(halogen01.cps_mspct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
for (m in names(halogen01.cps_mspct)) {
  print(autoplot(halogen01.cps_mspct[[m]]) + ggtitle(m))
  print(autoplot(halogen01.cps_mspct[[m]], range = c(250, 410)) + ggtitle(m))
}

## -----------------------------------------------------------------------------
halogen_corrected.cps_spct <-
  filter_correction(halogen01.cps_mspct[["light"]], 
                    halogen01.cps_mspct[["filter"]],
                    stray.light.method = "original",
                    flt.Tfr = 0.9)
names(halogen_corrected.cps_spct)
getTimeUnit(halogen_corrected.cps_spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(halogen_corrected.cps_spct)
autoplot(halogen_corrected.cps_spct, range = c(250, 410))
mean(clip_wl(halogen_corrected.cps_spct, range = c(250, 300))[["cps"]])

## -----------------------------------------------------------------------------
cps2irrad(halogen_corrected.cps_spct) -> halogen.source_spct
names(halogen.source_spct)

## -----------------------------------------------------------------------------
q_ratio(halogen.source_spct, list(UVC(), UVB(), UVA()), PAR()) * 1e3

## -----------------------------------------------------------------------------
halogen_sm0.source_spct <- smooth_spct(halogen.source_spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(halogen_sm0.source_spct)
autoplot(halogen_sm0.source_spct, range = c(250, 410))

## -----------------------------------------------------------------------------
q_ratio(halogen_sm0.source_spct, list(UVC(), UVB(), UVA()), PAR()) * 1e3

## -----------------------------------------------------------------------------
halogen_sm.source_spct <- smooth_spct(halogen.source_spct, method = "supsmu", strength = 3)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(halogen_sm.source_spct)
autoplot(halogen_sm.source_spct, range = c(250, 410))

## -----------------------------------------------------------------------------
q_ratio(halogen_sm.source_spct, list(UVC(), UVB(), UVA()), PAR()) * 1e3

## -----------------------------------------------------------------------------
names(xenon_flash.raw_mspct)

## -----------------------------------------------------------------------------
names(xenon_flash.raw_mspct[["light"]])

## -----------------------------------------------------------------------------
summary(xenon_flash.raw_mspct[["light"]])

## -----------------------------------------------------------------------------
getInstrSettings(xenon_flash.raw_mspct[["light"]])$num.exposures

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
xenon_flash.spct <-
  s_irrad_corrected(xenon_flash.raw_mspct, correction.method = MAYP11278_ylianttila.mthd)
getTimeUnit(xenon_flash.spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(xenon_flash.spct, range = c(315, NA))

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
xenon_flash.cps_spct <-
  s_irrad_corrected(xenon_flash.raw_mspct, correction.method= MAYP11278_ylianttila.mthd, return.cps = TRUE)
getTimeUnit(xenon_flash.cps_spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(xenon_flash.cps_spct, range = c(315, NA))

