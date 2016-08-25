library(photobiology)
library(ggplot2)
library(ggspectra)
library(ooacquire)
library(readxl)


calibration.data <- read_excel("cal-coeffs.xlsx")

w <- start_session()

list_instruments(w)

# get a descriptor for the first channel of the first spectrometer
descriptor <- get_oo_descriptor(w)
# set Maya hot pixels and correct time limits
descriptor <- set_descriptor_bad_pixs(descriptor, c(123,380,1829,1994))
descriptor <- set_descriptor_integ_time(descriptor, NA, 7.2)
descriptor <- set_descriptor_wl(descriptor, calibration.data$w.length)
descriptor <- set_descriptor_irrad_mult(descriptor, calibration.data$multiplier * 1e4)
# descriptor <- set_descriptor_slit_fun(descriptor, ooacquire::maya_tail_correction)
#

# PROTOCOL 1
# set measurement settings for automatic adjustment
# there is no restriction for HDR settings but lengths between 1 and 3
# seem reasonable.
#
settings <- acq_settings(descriptor,
                         HDR.mult = c(1,10),
                         tot.time.range = c(5,10))

settings <- tune_acq_settings(descriptor, settings)
settings

spct_1.acq <- acq_raw_spct(descriptor, settings)
plot(spct_1.acq)

# processing of raw counts

spct_1 <- trim_counts(spct_1.acq)
plot(spct_1)

spct_1 <- linearize_counts(spct_1)
plot(spct_1)

# use an internal "dark" reference from 191 to 350 nm
spct_1 <- fshift(spct_1, range = c(191,290))
plot(spct_1)

# conversion to counts per second
spct_1a <- raw2cps(spct_1)
plot(spct_1a)

# merge data from long and short integration times
spct_1b <- merge_cps(spct_1a)
plot(spct_1b)

irrad_1b <- cps2irrad(spct_1b)
plot(irrad_1b)
plot(irrad_1b, unit.out = "photon")

## PROTOCOL 2
# acquire a measure in the light and a dark reference spectrum
# (The character vector for protocol can be of any length, and the strings
# can be anything you want. A prompt is issued if two consecutive strings
# are different. A protocol like c(rep("light", 20), "filter", "dark") is
# legal.)
#
# We first tune again the settings in case the light level has changed.
settings <- tune_acq_settings(descriptor, settings)

# we acquire two pairs of bracketed spectra
mspct_1 <- acq_raw_mspct(descriptor, settings,
                         protocol = c("light", "filter", "dark"),
                         user.label = "window.sun")
mspct_1

## use new function
##

corrected.spct <- uvb_corrections(x = mspct_1[[1]],
                            flt = mspct_1[[2]],
                            dark = mspct_1[[3]],
                            method = "original",
                            stray.light.wl = c(218.5, 228.5),
                            flt.dark.wl = c(193, 209.5),
                            flt.ref.wl = c(360, 379.5),
                            worker_fun = ooacquire::maya_tail_correction,
                            trim = 0,
                            verbose = FALSE)
plot(corrected.spct)

irrad.spct <- cps2irrad(corrected.spct)

plot(irrad.spct)

end_session(w)
