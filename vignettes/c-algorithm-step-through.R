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
plot(LED_lamp_recalc.spct)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(white_LED_2min.raw_mspct[["light"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(white_LED_2min.raw_mspct[["dark"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(white_LED_2min.raw_mspct)) {
  white_LED_2min.raw_mspct[[m]] <-
    skip_bad_pixs(white_LED_2min.raw_mspct[[m]])
  print(plot(white_LED_2min.raw_mspct[[m]]) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(white_LED_2min.raw_mspct)) {
  white_LED_2min.raw_mspct[[m]] <-
    trim_counts(white_LED_2min.raw_mspct[[m]])
  print(plot(white_LED_2min.raw_mspct[[m]]) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(white_LED_2min.raw_mspct)) {
  white_LED_2min.raw_mspct[[m]] <-
    bleed_nas(white_LED_2min.raw_mspct[[m]])
  print(plot(white_LED_2min.raw_mspct[[m]]) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(white_LED_2min.raw_mspct)) {
  white_LED_2min.raw_mspct[[m]] <-
    linearize_counts(white_LED_2min.raw_mspct[[m]])
  print(plot(white_LED_2min.raw_mspct[[m]], ylim = c(NA, 65000)) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(white_LED_2min.raw_mspct)) {
  white_LED_2min.raw_mspct[[m]] <-
    fshift(white_LED_2min.raw_mspct[[m]], range = c(250,290))
  print(plot(white_LED_2min.raw_mspct[[m]], ylim = c(NA, 65000)) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
white_LED_2min.cps_mspct <- cps_mspct()
for (m in names(white_LED_2min.raw_mspct)) {
  white_LED_2min.cps_mspct[[m]] <-
    raw2cps(white_LED_2min.raw_mspct[[m]])
  print(
    ggplot(white_LED_2min.cps_mspct[[m]], aes(x = w.length)) +
      ylim(NA, 40000) +
      geom_line(aes(y = cps_1), color = "blue") +
      geom_line(aes(y = cps_2), color = "red") +
      geom_line(aes(y = cps_3), color = "green") +
      ggtitle(m)
  )
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(white_LED_2min.cps_mspct)) {
  white_LED_2min.cps_mspct[[m]] <-
    merge_cps(white_LED_2min.cps_mspct[[m]])
  print(plot(white_LED_2min.cps_mspct[[m]], ylim = c(NA, 40000)) + ggtitle(m))
}

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
white_LED_2min.cps_spct <- 
  white_LED_2min.cps_mspct[["light"]] - white_LED_2min.cps_mspct[["dark"]]
white_LED_2min.cps_spct <- 
  copy_attributes(white_LED_2min.raw_mspct[["light"]],
                  white_LED_2min.cps_spct,
                  copy.class = FALSE)
plot(white_LED_2min.cps_spct) + ggtitle("Dark subtracted")

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
white_LED_2min.spct <- cps2irrad(white_LED_2min.cps_spct)
plot(white_LED_2min.spct)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
white_LED_2min.spct <- smooth_spct(white_LED_2min.spct)
plot(white_LED_2min.spct)

## ------------------------------------------------------------------------
names(halogen.raw_mspct)

## ------------------------------------------------------------------------
names(halogen.raw_mspct[["light"]])

## ------------------------------------------------------------------------
summary(halogen.raw_mspct[["light"]])

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
halogen.spct <-
  s_irrad_corrected(halogen.raw_mspct, correction.method= MAYP11278_ylianttila.mthd)
plot(halogen.spct)

## ------------------------------------------------------------------------
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

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(halogen.cps_mspct)) {
  print(plot(halogen.cps_mspct[[m]]) + ggtitle(m))
  print(plot(halogen.cps_mspct[[m]], range = c(250, 410)) + ggtitle(m))
}

## ------------------------------------------------------------------------
halogen01.cps_mspct <- cps_mspct()
for (m in setdiff(names(halogen.cps_mspct), "dark")) {
  halogen01.cps_mspct[[m]] <- halogen.cps_mspct[[m]] - halogen.cps_mspct[["dark"]]
  halogen01.cps_mspct[[m]] <- 
    copy_attributes(halogen.cps_mspct[[m]],
                    halogen01.cps_mspct[[m]],
                    copy.class = FALSE)
}
names(halogen01.cps_mspct)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
for (m in names(halogen01.cps_mspct)) {
  print(plot(halogen01.cps_mspct[[m]]) + ggtitle(m))
  print(plot(halogen01.cps_mspct[[m]], range = c(250, 410)) + ggtitle(m))
}

## ------------------------------------------------------------------------
halogen_corrected.cps_spct <-
  filter_correction(halogen01.cps_mspct[["light"]], 
                    halogen01.cps_mspct[["filter"]],
                    stray.light.method = "original",
                    flt.Tfr = 0.9)
names(halogen_corrected.cps_spct)
getTimeUnit(halogen_corrected.cps_spct)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(halogen_corrected.cps_spct)
plot(halogen_corrected.cps_spct, range = c(250, 410))
mean(clip_wl(halogen_corrected.cps_spct, range = c(250, 300))[["cps"]])

## ------------------------------------------------------------------------
cps2irrad(halogen_corrected.cps_spct) -> halogen.source_spct
names(halogen.source_spct)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(halogen.source_spct)
plot(halogen.source_spct, range = c(250, 410))

## ------------------------------------------------------------------------
q_ratio(halogen.source_spct, list(UVC(), UVB(), UVA()), PAR()) * 1e3

## ------------------------------------------------------------------------
halogen_sm.source_spct <- smooth_spct(halogen.source_spct, method = "supsmu", strength = 3)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(halogen_sm.source_spct)
plot(halogen_sm.source_spct, range = c(250, 410))

## ------------------------------------------------------------------------
q_ratio(halogen.source_spct, list(UVC(), UVB(), UVA()), PAR()) * 1e3

## ------------------------------------------------------------------------
names(xenon_flash.raw_mspct)

## ------------------------------------------------------------------------
names(xenon_flash.raw_mspct[["light"]])

## ------------------------------------------------------------------------
summary(xenon_flash.raw_mspct[["light"]])

## ------------------------------------------------------------------------
getInstrSettings(halogen.raw_mspct[["light"]])$num.exposures
getInstrSettings(xenon_flash.raw_mspct[["light"]])$num.exposures

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
xenon_flash.spct <-
  s_irrad_corrected(xenon_flash.raw_mspct, correction.method = MAYP11278_ylianttila.mthd)
getTimeUnit(xenon_flash.spct)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(xenon_flash.spct, range = c(315, NA))

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
xenon_flash.cps_spct <-
  s_irrad_corrected(xenon_flash.raw_mspct, correction.method= MAYP11278_ylianttila.mthd, return.cps = TRUE)
getTimeUnit(xenon_flash.cps_spct)

## ---- fig.width=8, fig.asp=0.5-------------------------------------------
plot(xenon_flash.cps_spct, range = c(315, NA))

