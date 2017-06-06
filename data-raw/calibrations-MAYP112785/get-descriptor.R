library(ooacquire)

w <- start_session()

list_instruments(w)

## query descriptor from instrument
descriptor <- get_oo_descriptor(w)

save(descriptor, file = "data-raw/maya-descriptor/MAYP112785.Rda")
