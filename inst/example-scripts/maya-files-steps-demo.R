# This script is just to demonstrate the processing steps by plotting the
# spectral data at each intermediate step in the processing


# Load packages -----------------------------------------------------------

library(photobiology)
library(ggplot2)
library(ggspectra)
library(ooacquire)

# Read raw counts from file ----------------------------------------------

# set working directory to where the example files are stored
oldwd <- setwd(system.file("extdata", package = "ooacquire"))

# locale where files were saved
my.locale <- readr::locale("en", decimal_mark = ",", tz = "EET")


# EXAMPLE 1: One raw counts file ------------------------------------------

# read one file into a raw_spct object

spct_1 <- read_oo_ssdata("light-short.txt",
                         descriptor = which_descriptor("2016-10-11"),
                         locale = my.locale)

# convert clipped pixels if any to NAs
spct_1 <- trim_counts(spct_1)
plot(spct_1)

# if there are NAs from clipping then extend the NAs to neighbours as these
# are affected by the neighbours' clipping
spct_1 <- bleed_nas(spct_1)
plot(spct_1)

# apply linearization to sensor counts as the response of the array is not
# linear
spct_1 <- linearize_counts(spct_1)
plot(spct_1)

# use an internal "dark" reference from 191 to 290 nm
spct_1 <- fshift(spct_1, range = c(191,290))
plot(spct_1)

# conversion to counts per second
spct_1 <- raw2cps(spct_1)
plot(spct_1)

# conversion to irradiance using the calibration in the instrument descriptor
spct_1 <- cps2irrad(spct_1)
plot(spct_1)

spct_1 <- trim_wl(spct_1, range = c(290,NA), fill = 0)
plot(spct_1)


# EXAMPLE 2: Brackting of integration time --------------------------------

# read one file into a raw_spct object

mspct_2 <- read_files2mspct(list(a = "light-short.txt", b = "light-long.txt"),
                            descriptor = which_descriptor("2016-10-11"),
                            locale = my.locale)

# we merge the two spectra into single one
spct_2 <- merge_raw_mspct(mspct_2)
plot(spct_2)

# convert clipped pixels if any to NAs
spct_2 <- trim_counts(spct_2)
plot(spct_2)

# if there are NAs from clipping then extend the NAs to neighbours as these
# are affected by the neighbours' clipping
spct_2 <- bleed_nas(spct_2)
plot(spct_2)

# apply linearization to sensor counts as the response of the array is not
# linear
spct_2 <- linearize_counts(spct_2)
plot(spct_2)

# use an internal "dark" reference from 191 to 290 nm
spct_2 <- fshift(spct_2, range = c(191,290))
plot(spct_2)

# conversion to counts per second
spct_2 <- raw2cps(spct_2)
plot(spct_2)

ggplot(spct_2, aes(x = w.length)) +
  geom_line(aes(y = cps_1), color = "blue") +
  geom_line(aes(y = cps_2), color = "red")

# HDR, splicing of bracketed measurements
spct_2 <- merge_cps(spct_2)
plot(spct_2)

# conversion to irradiance using the calibration in the instrument descriptor
spct_2 <- cps2irrad(spct_2)
plot(spct_2)

spct_2 <- trim_wl(spct_2, range = c(290,NA), fill = 0)
plot(spct_2)

setwd(oldwd)