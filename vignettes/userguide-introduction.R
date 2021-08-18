## ----"setup", include=FALSE, cache=FALSE--------------------------------------
require("knitr")
opts_knit$set(cache = FALSE, root.dir = system.file("extdata", package = "ooacquire"))
sr.online <- FALSE

## ---- eval=FALSE--------------------------------------------------------------
#  folderpath <- system.file("extdata", package="ooacquire")
#  oldwd <- setwd(folderpath)

## -----------------------------------------------------------------------------
library(photobiology)
library(photobiologyWavebands)
library(ggspectra)
library(ooacquire)

## -----------------------------------------------------------------------------
file_names <- list(light = "irrad-files/light-short.txt")

## -----------------------------------------------------------------------------
one_file.spct <- 
  s_irrad_corrected(x = file_names,
                    descriptor = which_descriptor("2016-10-11" , 
                                                  MAYP11278_descriptors),
                    correction.method = MAYP11278_ylianttila.mthd)

## -----------------------------------------------------------------------------
one_file.spct <- trim_wl(one_file.spct, 
                         range = c(290, NA), 
                         use.hinges = FALSE, 
                         fill = 0)

## -----------------------------------------------------------------------------
one_file.spct

## ---- fig.height=5, fig.width=7-----------------------------------------------
autoplot(one_file.spct, unit.out = "photon")

## -----------------------------------------------------------------------------
getWhenMeasured(one_file.spct)
cat(getWhatMeasured(one_file.spct))
getWhereMeasured(one_file.spct)
cat(comment(one_file.spct))

## -----------------------------------------------------------------------------
getInstrDesc(one_file.spct)

## -----------------------------------------------------------------------------
getInstrSettings(one_file.spct)

## -----------------------------------------------------------------------------
file_names <- list(light = c("irrad-files/light-short.txt",
                             "irrad-files/light-long.txt"),
                   filter = "irrad-files/flt-long.txt",
                   dark = c("irrad-files/dark-short.txt",
                            "irrad-files/dark-long.txt"))

## -----------------------------------------------------------------------------
five_files.spct <- 
  s_irrad_corrected(x = file_names,
                    descriptor = which_descriptor("2016-10-11", 
                                                  MAYP11278_descriptors),
                    correction.method = MAYP11278_ylianttila.mthd)

## -----------------------------------------------------------------------------
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
five_files.spct <- trim_wl(five_files.spct, 
                           range = c(290, NA), 
                           use.hinges = FALSE, 
                           fill = 0)

## -----------------------------------------------------------------------------
five_files.spct

## ---- fig.height=5, fig.width=7-----------------------------------------------
autoplot(five_files.spct, unit.out = "photon")

## -----------------------------------------------------------------------------
getWhenMeasured(five_files.spct)
getWhatMeasured(five_files.spct)
getWhereMeasured(five_files.spct)
cat(comment(five_files.spct))

## -----------------------------------------------------------------------------
getInstrDesc(five_files.spct)

## -----------------------------------------------------------------------------
getInstrSettings(five_files.spct)

## -----------------------------------------------------------------------------
(q_irrad(one_file.spct) / q_irrad(five_files.spct) - 1) * 100

## -----------------------------------------------------------------------------
kable(
  t((q_irrad(one_file.spct, c(UV_bands(), VIS_bands())) / 
     q_irrad(five_files.spct, c(UV_bands(), VIS_bands())) - 1) * 100),
  digits = 2
)

## -----------------------------------------------------------------------------
file_names <- list(light = "irrad-files/light_MAYP112785.txt",
#                    filter = "irrad-files/filter_MAYP112785.txt",
                    dark = "irrad-files/dark_MAYP112785.txt")

## -----------------------------------------------------------------------------
ov_files.raw_mspct <- 
  ooacquire::read_files2mspct(file_names,
                              descriptor = 
                                which_descriptor("2017-01-05", MAYP112785_descriptors))
summary(ov_files.raw_mspct[[1]])
summary(ov_files.raw_mspct[[2]])

