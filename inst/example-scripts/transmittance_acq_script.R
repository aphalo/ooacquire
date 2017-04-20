# Using an spectrometer with two channels
# Tested with a Jaz spectrometer

library(photobiology)
library(ooacquire)

w <- start_session()

list_instruments(w)

descriptor_ch1 <- get_oo_descriptor(w, ch.index = 0L)
descriptor_ch2 <- get_oo_descriptor(w, ch.index = 1L)

descriptor <- descriptor_ch2

settings <- acq_settings(descriptor,
                         HDR.mult = c(1,10),
                         tot.time.range = c(10,10))

settings <- tune_acq_settings(descriptor, settings)
# settings

mspct_1 <- acq_raw_mspct(descriptor, settings, protocol = c("filter", "clear", "dark"))
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

spct_filter <- ref_correction(mspct_01$spct_1, mspct_01$spct_3)
spct_clear <- ref_correction(mspct_01$spct_2, mspct_01$spct_3)
plot(spct_filter)
plot(spct_clear)

spct_filter <- merge_cps(raw2cps(spct_filter))
spct_clear <- merge_cps(raw2cps(spct_clear))
plot(spct_filter)
plot(spct_clear)

test <- cps2Tfr(clean(spct_filter), clean(spct_clear))
plot(test)

end_session(w)
