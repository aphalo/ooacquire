library(ooacquire)
library(ggspectra)

# setwd("./not-inst/problem-spectra-from-Matt")

test1.ls <- list(light = "MapleSun34A00016.txt",
                  dark = "MapleSun34Adark.txt")

yli.spct <- s_irrad_corrected(test1.ls,
                  descriptor = which_descriptor("2018-01-01",
                                                MAYP11278_descriptors),
                  correction.method = MAYP11278_ylianttila.mthd)
plot(yli.spct)

simple.spct <- s_irrad_corrected(test1.ls,
                  descriptor = which_descriptor("2018-01-01",
                                                MAYP11278_descriptors),
                  correction.method = MAYP11278_simple.mthd)
plot(simple.spct)

sun.spct <- s_irrad_corrected(test1.ls,
                  descriptor = which_descriptor("2018-01-01",
                                                MAYP11278_descriptors),
                  correction.method = MAYP11278_sun.mthd)
plot(sun.spct)

test2.ls <- list(light = "MapleSun34A00016.txt",
                 dark = "MapleSun34Adark.txt",
                 filter = "MapleSun34APC.txt")
yli.spct <- s_irrad_corrected(test2.ls,
                  descriptor = which_descriptor("2018-01-01",
                                                MAYP11278_descriptors),
                  correction.method = MAYP11278_ylianttila.mthd)
plot(yli.spct)

simple.spct <- s_irrad_corrected(test2.ls,
                  descriptor = which_descriptor("2018-01-01",
                                                MAYP11278_descriptors),
                  correction.method = MAYP11278_simple.mthd)
plot(simple.spct)

sun.spct <- s_irrad_corrected(test2.ls,
                  descriptor = which_descriptor("2018-01-01",
                                                MAYP11278_descriptors),
                  correction.method = MAYP11278_sun.mthd)
plot(sun.spct)
