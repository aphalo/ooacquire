# Using an spectrometer with two channels
# Tested with a Jaz spectrometer

library(r4photobiology)
library(ooacquire)

w <- start_session()

list_instruments(w)

descriptor_ch1 <- get_oo_descriptor(w, ch.index = 0L)
descriptor_ch1$max.counts <- 50000
descriptor_ch2 <- get_oo_descriptor(w, ch.index = 1L)
descriptor_ch2$max.counts <- 50000

# measure reflectance

descriptor <- descriptor_ch1

settings <- acq_settings(descriptor,
                         HDR.mult = c(1,4,16),
                         tot.time.range = c(25,25))

settings <- tune_acq_settings(descriptor, settings)
# settings

mspct_1 <- acq_raw_mspct(descriptor, settings, protocol = c("reference", "filter", "dark"))
names(mspct_1)
plot(mspct_1[[1]])
plot(mspct_1[[2]])
plot(mspct_1[[3]])
getWhatMeasured(mspct_1)
mspct_01 <- msmsply(mspct_1, trim_counts)
mspct_01 <- msmsply(mspct_01, linearize_counts)
plot(mspct_01[[1]])
plot(mspct_01[[2]])
plot(mspct_01[[3]])

spct_filter <- raw2cps(mspct_01$filter)
spct_reference <- raw2cps(mspct_01$reference)
spct_dark <- raw2cps(mspct_01$dark)

plot(spct_filter)
plot(spct_reference)
plot(spct_dark)

spct_filter <- ref_correction(spct_filter, spct_dark)
spct_reference <- ref_correction(spct_reference, spct_dark)
plot(spct_filter)
plot(spct_reference)

# no HDR

spct_filter01 <- spct_filter[ , 1:2]
spct_reference01 <- spct_reference[ , 1:2]
spct_dark01 <- spct_dark[ , 1:2]
plot(spct_filter01)
plot(spct_reference01)
plot(spct_filter01 / spct_reference01, range = c(250, 900))

# with HDR

spct_filter <- merge_cps(spct_filter)
spct_reference <- merge_cps(spct_reference)
spct_dark <- merge_cps(spct_dark)

plot(spct_filter)
plot(spct_reference)
plot(spct_dark)

reflectance.spct <- cps2Rfr(clean(spct_filter), clean(spct_reference))
plot(reflectance.spct)

# measure transmittance

descriptor <- descriptor_ch2

settings <- acq_settings(descriptor,
                         HDR.mult = c(1,5,25),
                         tot.time.range = c(30,30))

settings <- tune_acq_settings(descriptor, settings)
# settings

mspct_1 <- acq_raw_mspct(descriptor, settings, protocol = c("reference", "filter", "dark"))
names(mspct_1)
plot(mspct_1[[1]])
plot(mspct_1[[2]])
plot(mspct_1[[3]])
getWhatMeasured(mspct_1)
mspct_01 <- msmsply(mspct_1, trim_counts)
mspct_01 <- msmsply(mspct_01, linearize_counts)
plot(mspct_01[[1]])
plot(mspct_01[[2]])
plot(mspct_01[[3]])

spct_filter <- merge_cps(raw2cps(mspct_01$filter))
spct_reference <- merge_cps(raw2cps(mspct_01$reference))
spct_dark <- merge_cps(raw2cps(mspct_01$dark))

plot(spct_filter)
plot(spct_reference)
plot(spct_dark)

transmittance.spct <- cps2Tfr(spct_filter, spct_reference)
plot(transmittance.spct)

# combine spectra

end_session(w)
