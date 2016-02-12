library(photobiology)
library(ooacquire)

w <- start_session()

list_instruments(w)

descriptor_ch1 <- get_oo_descriptor(w, ch.index = 0L)
descriptor_ch2 <- get_oo_descriptor(w, ch.index = 1L)

descriptor <- descriptor_ch2

current_settings <- get_oo_settings(descriptor)

settings <- acq_settings(descriptor, integ.time = 10e-3, num.scans = 1,
                         tot.time.range = c(1,1))

spct_0 <- acq_raw_spct(descriptor, settings)
plot(spct_0, type = "l")

settings <- tune_acq_settings(descriptor, settings)
settings

spct_1 <- acq_raw_spct(descriptor, settings)
plot(counts_1 ~ w.length, data = spct_1, type = "l")
lines(counts_2 ~ w.length, data = spct_1, col = "red")

spct_1_lin <- linearize_counts(spct_1)
plot(counts_1 ~ w.length, data = spct_1_lin, type = "l")
lines(counts_2 ~ w.length, data = spct_1_lin, col = "red")

settings <- acq_settings(descriptor, integ.time = c(10e-3, 100e-3),
                         num.scans = c(20, 5))

spct_2 <- acq_raw_spct(descriptor, settings)
plot(counts_1 ~ w.length, data = spct_2, type = "l")
lines(counts_2 ~ w.length, data = spct_2, col = "red")
class(spct_2)

mspct_1 <- acq_raw_mspct(descriptor, settings, protocol = rep("test", 3))
mspct_1
class(mspct_1)

end_session(w)
