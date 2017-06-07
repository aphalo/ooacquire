library(ooacquire)

w <- start_session()

list_instruments(w)

## query descriptor from instrument
descriptor_ch1 <- get_oo_descriptor(w, 0L, 0L)
descriptor_ch2 <- get_oo_descriptor(w, 0L, 1L)

# these are teken from certificate (not same as in firmware)
descriptor_ch1 <-
  set_descriptor_nl(descriptor_ch1,
                  nl.coeff = c(0.834187, 8.78431e-6, -3.065558e-11,
                               -1.10252e-14, 4.52434e-19, -8.91676e-24,
                               9.91782e-29, -5.245e-34))
descriptor_ch1 <-
  set_descriptor_bad_pixs(descriptor_ch1,
                        bad.pixs = 837)

# these are teken from certificate (not same as in firmware)
descriptor_ch2 <-
  set_descriptor_nl(descriptor_ch2,
                  nl.coeff = c(0.894998, 9.36573e-6, -7.00359e-10,
                               5.08055e-14, -2.31932e-18, 5.74352e-23,
                               -7.04268e-28, 3.29791e-33))
descriptor_ch2 <-
  set_descriptor_bad_pixs(descriptor_ch1,
                        bad.pixs = c(503, 1609, 2021))

save(descriptor_ch1, descriptor_ch2, file = "data-raw/jaz-descriptor/JAZA3098.Rda")
