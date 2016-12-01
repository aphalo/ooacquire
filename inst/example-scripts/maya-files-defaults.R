library(photobiology)
library(ggspectra)
library(ooacquire)
library(anytime)

oldwd <- setwd(system.file("extdata/irradiance", package = "ooacquire"))

# examples of use with no bracketing

files1 <- list(light = "light-short.txt",
               dark = "dark-short.txt")

my.spct <- s_irrad_corrected(x = files1,
                             descriptor = which_descriptor("2016-10-11"),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(290, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

files2 <- list(light = "light-short.txt")

my.spct <- s_irrad_corrected(x = files2,
                             descriptor = which_descriptor("2016-10-11"),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(290, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

# examples of use with no bracketing with decimal dots

files1 <- list(light = "light-short-dot.txt",
               dark = "dark-short-dot.txt")

my.spct <- s_irrad_corrected(x = files1,
                             descriptor = which_descriptor("2016-10-11"),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(290, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

# examples of use with no bracketing

filesBR0 <- list(light = c("light-short.txt",
                           "light-long.txt"),
                 filter = "flt-long.txt",
                 dark = c("dark-short.txt",
                          "dark-long.txt"))

my.spct <- s_irrad_corrected(x = filesBR0,
                             descriptor = which_descriptor("2016-10-11"),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 293 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(293, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

filesBR1 <- list(light = c("light-short.txt",
                           "light-long.txt"),
                 filter = "flt-long.txt")

my.spct <- s_irrad_corrected(x = filesBR1,
                             descriptor = which_descriptor("2016-10-11"),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 293 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(293, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

## WINTER time 2014

filesUVA0 <- list(light = c("UVA365-light-short.txt",
                           "UVA365-light-long.txt"),
                 filter = c("UVA365-flt-short.txt",
                            "UVA365-flt-long.txt"),
                 dark = c("UVA365-dark-short.txt",
                          "UVA365-dark-long.txt"))

my.spct <- s_irrad_corrected(x = filesUVA0,
                             descriptor = which_descriptor("2014-03-10"),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

# **

filesUVA1 <- list(light = c("UVA365-light-short.txt",
                            "UVA365-light-long.txt"),
                  filter = "UVA365-flt-long.txt")

my.spct <- s_irrad_corrected(x = filesUVA1,
                             descriptor = which_descriptor("2014-03-10"),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

# **

filesUVA2 <- list(light = "UVA365-light-short.txt",
                  filter = "UVA365-flt-short.txt",
                  dark = "UVA365-dark-short.txt")

my.spct <- s_irrad_corrected(x = filesUVA2,
                             descriptor = which_descriptor("2014-03-10"),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))


## WINTER time 2016

filesLED0 <- list(light = "led-light.txt",
                  filter = "led-flt.txt",
                  dark = "led-dark.txt")

my.spct <- s_irrad_corrected(x = filesLED0,
                             descriptor = which_descriptor("2016-10-11"),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 293 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(293, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

my.spct <- s_irrad_corrected(x = filesLED0,
                             time = anytime("2016-11-10 12:00:00"),
                             descriptor = which_descriptor("2016-10-11"),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 293 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(293, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

## Titta's canopy measurements

filesB2 <- list(light = c("canopyb2normal.txt",
                           "canopyb2normallong.txt"),
                 filter = c("canopyb2normalPC.txt",
                            "canopyb2normallongPC.txt"),
                 dark = c("canopyb2normaldark.txt",
                          "canopyb2normallongdark.txt"))

my.spct <- s_irrad_corrected(x = filesB2,
                             descriptor = which_descriptor("2016-11-17"),
                             method = MAYP11278_ylianttila.mthd)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 293 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(293, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

filesB2short <- list(light = "canopyb2normal.txt",
                filter = "canopyb2normalPC.txt",
                dark = "canopyb2normaldark.txt")

my.spct <- s_irrad_corrected(x = filesB2short,
                             descriptor = which_descriptor("2016-11-17"),
                             method = MAYP11278_ylianttila.mthd,
                             verbose = TRUE)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 293 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(293, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
getWhenMeasured(my.spct)
getWhereMeasured(my.spct)
getWhatMeasured(my.spct)
cat(comment(my.spct))

setwd(oldwd)