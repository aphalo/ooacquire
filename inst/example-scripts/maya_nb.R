library(photobiology)
library(ggspectra)
library(ooacquire)

descriptor.file <- system.file("extdata", "MAYP11278.rda", package = "ooacquire")

load(file = descriptor.file)

## example of use

my.locale <- readr::locale("en", decimal_mark = ",", tz = "EET")

files0 <- list(light = "data-example/pheno1normal.txt",
              filter = "data-example/pheno1normaldark.txt",
              dark = "data-example/pheno1normaldark.txt")

files1 <- list(light = "data-example/pheno1normal.txt",
              dark = "data-example/pheno1normaldark.txt")

files2 <- list(light = "data-example/pheno1normal.txt")

my.spct <- uvb_corrections_files(files2, descriptor, my.locale)
# clip data outside calibration range
my.spct <- clip_wl(my.spct, range = c(250, 899))
plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(290, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")

# test 2

filesBR0 <- list(light = c("data-example/pheno1normal.txt",
                           "data-example/pheno1normallong.txt"),
               filter = c("data-example/pheno1normallongPC.txt"),
               dark = c("data-example/pheno1normaldark.txt",
                        "data-example/pheno1normallongdark.txt"))

filesBR1 <- list(light = c("data-example/pheno1normal.txt",
                           "data-example/pheno1normallong.txt"),
                 filter = c("data-example/pheno1normallongPC.txt"))

my.spct <- uvb_corrections_files(filesBR1, descriptor, my.locale)
# clip data outside calibration range
my.spct <- clip_wl(my.spct, range = c(250, 899))
plot(my.spct, unit.out = "photon")
# force to zero wavelengths < 290 nm use only for sunlight, and after checking plot
my.spct <- trim_wl(my.spct, range = c(290, NA), use.hinges = FALSE, fill = 0)
plot(my.spct, unit.out = "photon")
