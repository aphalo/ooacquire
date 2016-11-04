## ----"setup", include=FALSE----------------------------------------------
require("knitr")
opts_knit$set(root.dir = system.file("extdata", package = "ooacquire"))

## ------------------------------------------------------------------------
library(photobiology)
library(ggspectra)
library(ooacquire)

## ------------------------------------------------------------------------
my.locale <- readr::locale("en", decimal_mark = ",", tz = "EET")


## ------------------------------------------------------------------------
my.spct <- s_irrad_corrected(x = list(light = "light-short.txt"),
                             descriptor = maya_descriptor,
                             method.data = maya_ylianttila,
                             locale = my.locale)

## ------------------------------------------------------------------------
# clip data outside calibration range
my.spct <- clip_wl(my.spct, range = c(250, 899))
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(290, NA), use.hinges = FALSE, fill = 0)

## ---- fig.height=5, fig.width=7------------------------------------------
plot(my.spct, unit.out = "photon")

## ------------------------------------------------------------------------
files_names <- list(light = c("light-short.txt",
                              "light-long.txt"),
                    filter = "flt-long.txt",
                    dark = c("dark-short.txt",
                             "dark-long.txt"))

## ------------------------------------------------------------------------
my.spct <- s_irrad_corrected(x = files_names,
                             descriptor = maya_descriptor,
                             method.data = maya_ylianttila,
                             locale = my.locale)

## ------------------------------------------------------------------------
# clip data outside calibration range
my.spct <- clip_wl(my.spct, range = c(250, 899))
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(290, NA), use.hinges = FALSE, fill = 0)

## ---- fig.height=5, fig.width=7------------------------------------------
plot(my.spct, unit.out = "photon")

