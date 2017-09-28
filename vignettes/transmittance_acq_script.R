# Using an spectrometer with two channels
# Tested with a Jaz spectrometer

library(ggplot2)
library(r4photobiology)
library(ooacquire)

w <- start_session()

list_instruments(w)

descriptor_ch1 <- get_oo_descriptor(w, ch.index = 0L)
# descriptor_ch1$max.counts <- 65000
descriptor_ch2 <- get_oo_descriptor(w, ch.index = 1L)
# descriptor_ch2$max.counts <- 65000

# measure reflectance

descriptor <- descriptor_ch1

settings <- acq_settings(descriptor,
#                         HDR.mult = c(1,10),
                         HDR.mult = 1,
                         tot.time.range = c(30,30))

settings <- tune_acq_settings(descriptor, settings)
# settings

mspct_1 <- acq_raw_mspct(descriptor, settings, protocol = c("reference", "sample", "dark"))
names(mspct_1)
plot(mspct_1[[1]], range = c(200, 1100))
plot(mspct_1[[2]], range = c(200, 1100))
plot(mspct_1[[3]], range = c(200, 1100))
getWhatMeasured(mspct_1)
mspct_01 <- msmsply(mspct_1, trim_counts)
plot(mspct_01[[1]])
plot(mspct_01[[2]])
plot(mspct_01[[3]])
mspct_01 <- msmsply(mspct_01, linearize_counts)
plot(mspct_01[[1]])
plot(mspct_01[[2]])
plot(mspct_01[[3]])

mspct_cps_01 <- raw2cps(mspct_01)

plot(mspct_cps_01$reference)
plot(mspct_cps_01$sample)
plot(mspct_cps_01$dark)

mspct_merged_01 <- msmsply(mspct_cps_01, merge_cps)

plot(mspct_merged_01$reference)
plot(mspct_merged_01$sample)
plot(mspct_merged_01$dark)

reflectance_01.spct <- cps2Rfr(mspct_merged_01$sample,
                               mspct_merged_01$reference,
                               mspct_merged_01$dark)

plot(reflectance_01.spct, annotations = "boundaries")

plot(reflectance_01.spct, range = c(290, 850),
     annotations = c("+", "boundaries"))

# measure transmittance

descriptor <- descriptor_ch2

settings <- acq_settings(descriptor,
                         HDR.mult = 1,
#                         HDR.mult = c(1,10),
                         tot.time.range = c(30,30))

settings <- tune_acq_settings(descriptor, settings)
# settings

mspct_1 <- acq_raw_mspct(descriptor, settings, protocol = c("reference", "sample", "dark"))
names(mspct_1)
plot(mspct_1[[1]])
plot(mspct_1[[2]])
plot(mspct_1[[3]])
getWhatMeasured(mspct_1)
mspct_01 <- msmsply(mspct_1, trim_counts)
plot(mspct_01[[1]])
plot(mspct_01[[2]])
plot(mspct_01[[3]])
mspct_01 <- msmsply(mspct_01, linearize_counts)
plot(mspct_01[[1]])
plot(mspct_01[[2]])
plot(mspct_01[[3]])

mspct_cps_01 <- raw2cps(mspct_01)

plot(mspct_cps_01$reference)
plot(mspct_cps_01$sample)
plot(mspct_cps_01$dark)

mspct_merged_01 <- msmsply(mspct_cps_01, merge_cps)

plot(mspct_merged_01$reference)
plot(mspct_merged_01$sample)
plot(mspct_merged_01$dark)

transmittance.spct <- cps2Tfr(mspct_merged_01$sample,
                              mspct_merged_01$reference,
                              mspct_merged_01$dark)
plot(transmittance.spct, range = c(250, 900))

end_session(w)
