library(photobiology)
library(ggspectra)
library(ooacquire)

setwd(system.file("extdata", package = "ooacquire"))

# locale where files were saved
my.locale <- readr::locale("en", decimal_mark = ",", tz = "EET")

# examples of use with no bracketing

files1 <- list(light = "light-short.txt",
              dark = "dark-short.txt")

files2 <- list(light = "light-short.txt")

my.spct <- s_irrad_corrected(x = files2,
                             descriptor = maya_descriptor,
                             method.data = maya_ylianttila,
                             locale = my.locale)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(290, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")

# examples of use with no bracketing

filesBR0 <- list(light = c("light-short.txt",
                           "light-long.txt"),
                 filter = "flt-long.txt",
                 dark = c("dark-short.txt",
                          "dark-long.txt"))

filesBR1 <- list(light = c("light-short.txt",
                           "light-long.txt"),
                 filter = "flt-long.txt")

my.spct <- s_irrad_corrected(x = filesBR1,
                             descriptor = maya_descriptor,
                             method.data = maya_ylianttila,
                             locale = my.locale)

plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(290, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
