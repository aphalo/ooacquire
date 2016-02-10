library(ooacquire)

w <- start_session()

list_instruments(w)

descriptor <- get_oo_descriptor(w)

current_settings <- get_oo_settings(descriptor)

settings <- acq_settings(descriptor, integ.time = 10e-3, num.scans = 1)

acq_raw_spct(descriptor, settings)

settings <- acq_settings(descriptor, integ.time = c(10e-3, 100e-3),
                         num.scans = c(20, 5))

spct1 <- acq_raw_spct(descriptor, settings)
class(spct1)

mspct1 <- acq_raw_mspct(descriptor, settings, protocol = rep("test", 3))
class(mspct1)

end_session(w)
