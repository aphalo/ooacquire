# check linearity and calculate coefficients
# as per OO practice we do this by changing the integration time

library(ggplot2)
library(r4photobiology)
library(ooacquire)

# script to be run under high irradiance to ensure low dark noise
# make sure maximum integration time is very close to 400 * minimum integration time.
# for Jaz use 0.4 s as base integration time as minmum is 0.001 s (1 ms = 1000 us)
# adjust light source to get a suitable integration time
# use a steady light source, e.g. LED driven at constant current for best
# estimates.

# these gives the different integration time multipliers
# Not exactly what OO uses. I added aditional points near ends, to better anchor the
# fitted line, and in the more curved regions. The number of intermediate values
# can be adjusted.
HDR.multipliers <-
  c(1.2, 1.1, 1.05, 1.0, 0.95, 0.9, 0.85, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3,
    0.2, 0.15, 0.1, 0.05, 0.03, 0.03)

w <- start_session()

list_instruments(w)

descriptor_ch1 <- get_oo_descriptor(w, ch.index = 0L)
descriptor_ch2 <- get_oo_descriptor(w, ch.index = 1L)

# channel 1 (index 0L)

descriptor <- descriptor_ch1

# Increasing total time simply increases the number of averaged spectra.
# If you use an AC driven light source set total time to a fixed value and make
# sure this value matches a whole number of cycles, or just use as minimum
# total time a length of time that encompases many cycles.

settings <- acq_settings(descriptor,
                         HDR.mult = HDR.multipliers,
                         tot.time.range = c(3:5),
                         target.margin = 0.1)

# you will need to iterate running this line and adjusting the irradiance
# until the base integration time (that for multiplier = 1) is 0.3 s.
settings <- tune_acq_settings(descriptor, settings)


mspct_010 <- acq_raw_mspct(descriptor, settings, protocol = c("light", "dark"))
plot(mspct_010[["light"]])
plot(mspct_010[["dark"]])

# data processing
# remove clipped
mspct_011 <- trim_counts(mspct_010)
mspct_012 <- raw2cps(mspct_011)

spct_012 <- mspct_012[["light"]]

# we may want to smooth and/or clean the spectrum before proceeding
# OO uses boxcar 10

# index to highest peak in a non-clipped spectrum
row.selector <- find_peaks(spct_012[[5]], span = NULL)

# get the row, i.e. different counts for the same pixel
row.vec <- as.vector(spct_011[row.selector, -1])

# we assume that at 1/10 full signal we are in the linear region

ch1.df <- data.frame(cps = row.vec,
                     rel.signal = HDR.multipliers * 0.95,
                     expected.cps = mean(cps[HDR.multipliers == 0.1]) *
                       HDR.multipliers * 0.95 / 0.1)

plot(cps ~ rel.signal, data = ch1.df)

# need to think what model to use!!
mf_ch1 <- lm(rel.signal * cps ~ poly(cps, 7))
summary(mf_ch1)
plot(mf_ch1)
coef(mf_ch1)

# channel 2 (index 1L)

descriptor <- descriptor_ch1

settings <- acq_settings(descriptor,
                         HDR.mult = HDR.multipliers,
                         tot.time.range = c(10:30),
                         target.margin = 0.1)


settings <- tune_acq_settings(descriptor, settings)

# settings

mspct_020 <- acq_raw_mspct(descriptor, settings, protocol = "test")
mspct_021 <- msmsply(mspct_020, trim_counts)




