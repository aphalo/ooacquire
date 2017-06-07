library(ooacquire)

w <- start_session()

list_instruments(w)

## query descriptor from instrument
descriptor <- get_oo_descriptor(w)

descriptor <-
  set_descriptor_bad_pixs(descriptor,
                          bad.pixs = c(123, 380, 388, 697, 1829, 1994))

save(descriptor, file = "data-raw/maya-descriptor/MAYP11278.Rda")
