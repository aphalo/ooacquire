## ----"setup", include=FALSE----------------------------------------------
require("knitr")
opts_knit$set(root.dir = system.file("extdata", package = "ooacquire"))

## ---- eval=-2------------------------------------------------------------
folderpath <- system.file("extdata",package="ooacquire")
file.copy(from = folderpath, to = ".", recursive = TRUE)

## ------------------------------------------------------------------------
library(photobiology)
library(photobiologyWavebands)
library(ggspectra)
library(ooacquire)

## ------------------------------------------------------------------------
file_names <- list(light = "irrad-files/light-short.txt")

## ------------------------------------------------------------------------
one_file.spct <- 
  s_irrad_corrected(x = file_names,
                    descriptor = which_descriptor("2016-10-11" , 
                                                  MAYP11278_descriptors),
                    correction.method = MAYP11278_ylianttila.mthd)

## ------------------------------------------------------------------------
one_file.spct <- trim_wl(one_file.spct, 
                         range = c(290, NA), 
                         use.hinges = FALSE, 
                         fill = 0)

## ------------------------------------------------------------------------
one_file.spct

## ---- fig.height=5, fig.width=7------------------------------------------
plot(one_file.spct, unit.out = "photon")

## ------------------------------------------------------------------------
getWhenMeasured(one_file.spct)
cat(getWhatMeasured(one_file.spct))
getWhereMeasured(one_file.spct)
cat(comment(one_file.spct))

## ------------------------------------------------------------------------
getInstrDesc(one_file.spct)

## ------------------------------------------------------------------------
getInstrSettings(one_file.spct)

## ------------------------------------------------------------------------
file_names <- list(light = c("irrad-files/light-short.txt",
                             "irrad-files/light-long.txt"),
                   filter = "irrad-files/flt-long.txt",
                   dark = c("irrad-files/dark-short.txt",
                            "irrad-files/dark-long.txt"))

## ------------------------------------------------------------------------
five_files.spct <- 
  s_irrad_corrected(x = file_names,
                    descriptor = which_descriptor("2016-10-11", 
                                                  MAYP11278_descriptors),
                    correction.method = MAYP11278_ylianttila.mthd)

## ------------------------------------------------------------------------
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
five_files.spct <- trim_wl(five_files.spct, 
                           range = c(290, NA), 
                           use.hinges = FALSE, 
                           fill = 0)

## ------------------------------------------------------------------------
five_files.spct

## ---- fig.height=5, fig.width=7------------------------------------------
plot(five_files.spct, unit.out = "photon")

## ------------------------------------------------------------------------
getWhenMeasured(five_files.spct)
getWhatMeasured(five_files.spct)
getWhereMeasured(five_files.spct)
cat(comment(five_files.spct))

## ------------------------------------------------------------------------
getInstrDesc(five_files.spct)

## ------------------------------------------------------------------------
getInstrSettings(five_files.spct)

## ------------------------------------------------------------------------
(q_irrad(one_file.spct) / q_irrad(five_files.spct) - 1) * 100

## ------------------------------------------------------------------------
kable(
  t((q_irrad(one_file.spct, c(UV_bands(), VIS_bands())) / 
     q_irrad(five_files.spct, c(UV_bands(), VIS_bands())) - 1) * 100),
  digits = 2
)

## ------------------------------------------------------------------------
file_names <- list(light = "irrad-files/light_MAYP112785.txt",
#                    filter = "irrad-files/ilter_MAYP112785.txt",
                    dark = "irrad-files/dark_MAYP112785.txt")

## ------------------------------------------------------------------------
ov_files.raw_mspct <- 
  ooacquire::read_files2mspct(file_names,
                              descriptor = which_descriptor("2017-01-05", 
                                                            MAYP112785_descriptors))
summary(ov_files.raw_mspct[[1]])
summary(ov_files.raw_mspct[[2]])

## ------------------------------------------------------------------------
ov_files.spct <- 
  s_irrad_corrected(x = ov_files.raw_mspct,
                    correction.method = ooacquire::MAYP112785_ylianttila.mthd)

## ------------------------------------------------------------------------
ov_files.spct <- 
  s_irrad_corrected(x = file_names,
                    descriptor = which_descriptor("2017-01-05", 
                                                  MAYP112785_descriptors),
                    correction.method = ooacquire::MAYP112785_ylianttila.mthd)

## ------------------------------------------------------------------------
ov_files.spct

## ---- fig.height=5, fig.width=7------------------------------------------
plot(ov_files.spct, unit.out = "photon")

## ---- fig.height=5, fig.width=7, eval=FALSE------------------------------
#  ## error!
#  plot(smooth_spct(ov_files.spct, strength = 5), unit.out = "photon")

## ------------------------------------------------------------------------
descriptor <- 
  which_descriptor(getWhenMeasured(white_LED.raw_mspct$light))
irrad01.spct <- 
  s_irrad_corrected(x = white_LED.raw_mspct,
                       descriptor = descriptor,
                       correction.method = MAYP11278_ylianttila.mthd)

## ------------------------------------------------------------------------
irrad01.spct

## ---- fig.height=5, fig.width=7------------------------------------------
plot(irrad01.spct, unit.out = "photon")

## ------------------------------------------------------------------------
getWhenMeasured(irrad01.spct)
getWhereMeasured(irrad01.spct)
getWhatMeasured(irrad01.spct)
cat(comment(irrad01.spct))

## ------------------------------------------------------------------------
getInstrDesc(irrad01.spct)

## ------------------------------------------------------------------------
getInstrSettings(irrad01.spct)

## ------------------------------------------------------------------------
descriptor <- 
  which_descriptor(getWhenMeasured(blue_filter.raw_mspct$sample))
tfr01.spct <- 
  s_fraction_corrected(x = blue_filter.raw_mspct,
                       descriptor = descriptor,
                       correction.method = ooacquire::MAYP11278_ylianttila.mthd,
                       dyn.range = 3e2)

## ------------------------------------------------------------------------
tfr01.spct

## ---- fig.height=5, fig.width=7------------------------------------------
plot(tfr01.spct)

## ------------------------------------------------------------------------
tfr01.spct <- clip_wl(tfr01.spct, range = c(355, 1100))

## ---- fig.height=5, fig.width=7------------------------------------------
plot(tfr01.spct, unit.out = "photon", w.band = VIS_bands())

## ------------------------------------------------------------------------
getWhenMeasured(tfr01.spct)
getWhereMeasured(tfr01.spct)
getWhatMeasured(tfr01.spct)
cat(comment(tfr01.spct))

## ------------------------------------------------------------------------
getInstrDesc(tfr01.spct)

## ------------------------------------------------------------------------
getInstrSettings(tfr01.spct)

## ------------------------------------------------------------------------
descriptor <- 
  which_descriptor(getWhenMeasured(blue_filter.raw_mspct$sample))
tfr02.spct <- 
  s_fraction_corrected(x = blue_filter.raw_mspct,
                       ref.value = 0.95,
                       descriptor = descriptor,
                       correction.method = MAYP11278_ylianttila.mthd,
                       dyn.range = 3e2)
tfr02.spct <- trim_wl(tfr02.spct, c(460, 1000))
tfr02.spct

## ---- fig.height=5, fig.width=7------------------------------------------
plot(tfr02.spct)

## ------------------------------------------------------------------------
descriptor <- 
  which_descriptor(getWhenMeasured(blue_filter.raw_mspct$sample))
rfr01.spct <- 
  s_fraction_corrected(x = blue_filter.raw_mspct,
                       ref.value = as.reflector_spct(white_body.spct) * 0.97,
                       descriptor = descriptor,
                       correction.method = MAYP11278_ylianttila.mthd,
                       dyn.range = 3e2,
                       qty.out = "Rfr",
                       type = "total")
rfr01.spct <- trim_wl(rfr01.spct, c(460, 1000))
rfr01.spct

## ---- fig.height=5, fig.width=7------------------------------------------
plot(rfr01.spct)
plot(tfr02.spct)

