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
photon_as_default()

## -----------------------------------------------------------------------------
names(sun001.raw_mspct)

## -----------------------------------------------------------------------------
colnames(sun001.raw_mspct[["light"]])

## -----------------------------------------------------------------------------
summary(sun001.raw_mspct[["light"]])

## -----------------------------------------------------------------------------
# not run
str(sun001.raw_mspct[["light"]], max.level = 2)

## -----------------------------------------------------------------------------
getInstrSettings(sun001.raw_mspct[["light"]])

## -----------------------------------------------------------------------------
attr(sun001.raw_mspct[["light"]], which = "instr.desc")$spectrometer.name

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
sun001_recalc.spct <-
  s_irrad_corrected(sun001.raw_mspct, 
                    correction.method = ooacquire::MAYP11278_ylianttila.mthd)

## -----------------------------------------------------------------------------
summary(sun001_recalc.spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(sun001_recalc.spct)

## ---- eval=FALSE--------------------------------------------------------------
#  get_attributes(sun001_recalc.spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
sun001_recalc_sun.spct <-
  s_irrad_corrected(sun001.raw_mspct, 
                    correction.method = ooacquire::MAYP11278_sun.mthd)

## -----------------------------------------------------------------------------
summary(sun001_recalc_sun.spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(sun001_recalc_sun.spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
sun001_recalc.cps_spct <-
  s_irrad_corrected(sun001.raw_mspct, 
                    correction.method = ooacquire::MAYP11278_ylianttila.mthd,
                    return.cps = TRUE)

## -----------------------------------------------------------------------------
summary(sun001_recalc.cps_spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(sun001_recalc.cps_spct)

## -----------------------------------------------------------------------------
getInstrDesc(sun001_recalc.cps_spct)

## -----------------------------------------------------------------------------
str(getInstrDesc(sun001_recalc.cps_spct))

## ---- eval = FALSE------------------------------------------------------------
#  sun001_recalc.source_spct <- cps2irrad(sun001_recalc.cps_spct)

## ---- eval = FALSE------------------------------------------------------------
#  summary(sun001_recalc.source_spct)

## ---- fig.width=8, fig.asp=0.5, eval = FALSE----------------------------------
#  autoplot(sun001_recalc.source_spct)

## -----------------------------------------------------------------------------
wavelengths <- getInstrDesc(sun001_recalc.cps_spct)$wavelengths
multipliers <- getInstrDesc(sun001_recalc.cps_spct)$inst.calib$irrad.mult

calib.spct <- calibration_spct(w.length = wavelengths,
                               irrad.mult = multipliers)

autoplot(calib.spct)

## -----------------------------------------------------------------------------
getInstrDesc(sun001.raw_mspct[["light"]])$max.counts

## -----------------------------------------------------------------------------
getInstrSettings(sun001.raw_mspct[["light"]])

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(sun001.raw_mspct[["light"]])

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(sun001.raw_mspct[["filter"]])

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(sun001.raw_mspct[["dark"]])

## ---- fig.width=8, fig.asp=1.5------------------------------------------------
for (m in names(sun001.raw_mspct)) {
  sun001.raw_mspct[[m]] <-
    skip_bad_pixs(sun001.raw_mspct[[m]])
}
autoplot(sun001.raw_mspct, facets = 1)

## ---- fig.width=8, fig.asp=1.5------------------------------------------------
for (m in names(sun001.raw_mspct)) {
  sun001.raw_mspct[[m]] <-
    trim_counts(sun001.raw_mspct[[m]])
}
autoplot(sun001.raw_mspct, facets = 1)

## ---- fig.width=8, fig.asp=1.5------------------------------------------------
for (m in names(sun001.raw_mspct)) {
  sun001.raw_mspct[[m]] <-
    bleed_nas(sun001.raw_mspct[[m]])
}
autoplot(sun001.raw_mspct, facets = 1)

## ---- fig.width=8, fig.asp=1.5------------------------------------------------
for (m in names(sun001.raw_mspct)) {
  sun001.raw_mspct[[m]] <-
    linearize_counts(sun001.raw_mspct[[m]])
}
autoplot(sun001.raw_mspct, facets = 1)

## ---- fig.width=8, fig.asp=1.5------------------------------------------------
for (m in names(sun001.raw_mspct)) {
  sun001.raw_mspct[[m]] <-
    fshift(sun001.raw_mspct[[m]], range = c(218.5,228.5))
}
autoplot(sun001.raw_mspct, facets = 1)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
sun001.cps_mspct <- raw2cps(sun001.raw_mspct)
autoplot(sun001.cps_mspct, facets = TRUE)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
for (m in names(sun001.cps_mspct)) {
  sun001.cps_mspct[[m]] <-
    merge_cps(sun001.cps_mspct[[m]])
}
autoplot(sun001.cps_mspct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
sun001_mdark.cps_mspct <- cps_mspct()

sun001_mdark.cps_mspct[["light"]] <- 
  sun001.cps_mspct[["light"]] - sun001.cps_mspct[["dark"]]

sun001_mdark.cps_mspct[["filter"]] <- 
  sun001.cps_mspct[["filter"]] - sun001.cps_mspct[["dark"]]

sun001_mdark.cps_mspct[["dark"]] <- NULL

autoplot(sun001_mdark.cps_mspct) + ggtitle("Dark subtracted")

autoplot(clip_wl(sun001_mdark.cps_mspct, range = c(280, 310))) +
  ggtitle("Dark subtracted")

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
ggplot(clip_wl(sun001_mdark.cps_mspct[["filter"]] /
                   sun001_mdark.cps_mspct[["light"]], 
                 range = c(280, 310))) +
  geom_line() +
  geom_hline(yintercept = 0.85, linetype = "dotted") +
  ggtitle("Stray light contribution to cps") +
  labs(y = "Contribution of stray light to readings (/1)",
       x = "Waavelength (nm)")

## -----------------------------------------------------------------------------
sun001_mdark.cps_mspct[["light"]] <-
  copy_attributes(sun001.cps_mspct[["light"]],
                  sun001_mdark.cps_mspct[["light"]])

sun001_mdark.cps_mspct[["filter"]] <-
  copy_attributes(sun001.cps_mspct[["filter"]],
                sun001_mdark.cps_mspct[["filter"]])

## ---- fig.width=8, fig.asp=0.5, eval = FALSE----------------------------------
#  sun001.irrad_spct <- cps2irrad(sun001_mdark.cps_mspct[["light"]])
#  autoplot(sun001.irrad_spct)
#  sun001_filter.irrad_spct <- cps2irrad(sun001_mdark.cps_mspct[["filter"]])
#  autoplot(sun001_filter.irrad_spct)

## ---- fig.width=8, fig.asp=0.5, eval = FALSE----------------------------------
#  sun001.irrad_spct <- smooth_spct(sun001.irrad_spct)
#  autoplot(sun001.irrad_spct)

## ---- fig.width=8, fig.asp=0.5------------------------------------------------
autoplot(sun001_recalc.spct)

## ---- fig.width=8, fig.asp=0.5, eval = FALSE----------------------------------
#  autoplot(smooth_spct(sun001_recalc.spct))

## -----------------------------------------------------------------------------
str(MAYP11278_ylianttila.mthd)

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

