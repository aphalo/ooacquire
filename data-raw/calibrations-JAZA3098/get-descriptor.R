library(ooacquire)

w <- start_session()

list_instruments(w)

## query descriptor from instrument
descriptor_ch1 <- get_oo_descriptor(w, 0L, 0L)
descriptor_ch2 <- get_oo_descriptor(w, 0L, 1L)

save(descriptor_ch1, descriptor_ch2, file = "data-raw/jaz-descriptor/JAZA3098.Rda")
