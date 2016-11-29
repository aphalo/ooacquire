## ----"setup", include=FALSE----------------------------------------------
require("knitr")
opts_knit$set(root.dir = system.file("extdata/irradiance", package = "ooacquire"))

## ------------------------------------------------------------------------
library(photobiology)
library(photobiologyWavebands)
library(ggspectra)
library(ooacquire)

## ------------------------------------------------------------------------
files_names <- list(light = "light-short.txt")

## ------------------------------------------------------------------------
one_file.spct <- 
  s_irrad_corrected(x = list(light = "light-short.txt"),
                    descriptor = which_descriptor("2016-10-11" , 
                                                  MAYP11278_descriptors),
                    method = MAYP11278_ylianttila.mthd)

## ------------------------------------------------------------------------
one_file.spct <- trim_wl(one_file.spct, 
                         range = c(290, NA), 
                         use.hinges = FALSE, 
                         fill = 0)

## ---- fig.height=5, fig.width=7------------------------------------------
plot(one_file.spct, unit.out = "photon")

## ------------------------------------------------------------------------
files_names <- list(light = c("light-short.txt",
                              "light-long.txt"),
                    filter = "flt-long.txt",
                    dark = c("dark-short.txt",
                             "dark-long.txt"))

## ------------------------------------------------------------------------
five_files.spct <- 
  s_irrad_corrected(x = files_names,
                    descriptor = which_descriptor("2016-10-11" , 
                                                  MAYP11278_descriptors),
                    method = MAYP11278_ylianttila.mthd)

## ------------------------------------------------------------------------
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
five_files.spct <- trim_wl(five_files.spct, 
                           range = c(290, NA), 
                           use.hinges = FALSE, 
                           fill = 0)

## ---- fig.height=5, fig.width=7------------------------------------------
plot(five_files.spct, unit.out = "photon")

## ------------------------------------------------------------------------
(q_irrad(one_file.spct) / q_irrad(five_files.spct) - 1) * 100

## ------------------------------------------------------------------------
kable(
  t((q_irrad(one_file.spct, c(UV_bands(), VIS_bands())) / 
     q_irrad(five_files.spct, c(UV_bands(), VIS_bands())) - 1) * 100),
  digits = 2
)

