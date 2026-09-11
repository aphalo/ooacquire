library(photobiology)
library(photobiologyWavebands)
library(ggplot2)
library(ggspectra)
library(ooacquire)

setwd(system.file("extdata/calibration-2016", package = "ooacquire"))

# locale where files were saved
my.locale <- readr::locale("en", decimal_mark = ",", tz = "EET")

my.descriptor <- which_descriptor()
# my.descriptor <- set_descriptor_slit_fun(which_descriptor())
# my.descriptor <- set_descriptor_irrad_mult(which_descriptor(), irrad.mult = 1)

descriptor.one.mult <- set_descriptor_irrad_mult(which_descriptor(),
                                                 irrad.mult = 1)

# Compare D2 lamp ---------------------------------------------------------

D2.lst <- list(light = "D2-light-short.txt",
               dark = "D2-dark-short.txt",
               filter = "D2-filter-short.txt")

# D2.lst <- list(light = "D2-light-short.txt", dark = "D2-dark-short.txt")

D2.cps.spct <- s_irrad_corrected(D2.lst,
                                 descriptor = my.descriptor,
                                 method = MAYP11278_ylianttila.mthd,
                                 locale = my.locale,
                                 return.cps = TRUE,
                                 verbose = FALSE)
plot(D2.cps.spct)

D2.irrad.spct <- s_irrad_corrected(D2.lst,
                                   descriptor = which_descriptor(),
                                   method = MAYP11278_ylianttila.mthd,
                                   locale = my.locale)
plot(D2.irrad.spct)

plot(D2.irrad.spct) +
  geom_line(data = D2_spectrum(D2.irrad.spct$w.length), color = "red")

plot(D2.irrad.spct / D2_spectrum(D2.irrad.spct$w.length))


# Compare FEL lamp --------------------------------------------------------


FEL.lst <- list(light = c("FEL-light-short.txt", "FEL-light-long.txt"),
               dark = c("FEL-dark-short.txt", "FEL-dark-long.txt"),
               filter = "FEL-filter-long.txt")

FEL.cps.spct <- s_irrad_corrected(FEL.lst,
                                  descriptor = which_descriptor(),
                                  method = MAYP11278_ylianttila.mthd,
                                  locale = my.locale,
                                  return.cps = TRUE,
                                  verbose = TRUE)
plot(FEL.cps.spct)

FEL.irrad.spct <- s_irrad_corrected(FEL.lst,
                                    descriptor = which_descriptor(),
                                    method = MAYP11278_ylianttila.mthd,
                                    locale = my.locale,
                                    verbose = TRUE)
plot(FEL.irrad.spct) +
  geom_line(data = FEL_spectrum(FEL.irrad.spct$w.length), color = "red")

plot(FEL.irrad.spct / FEL_spectrum(FEL.irrad.spct$w.length)) +
  xlim(400, 900) + ylim(0.99, 1.01)

# compute calibration ---------------------------------------------------

# make sure that the code is working we remove the calibration data

FEL.raw.mspct <- read_files2mspct(FEL.lst,
                                  descriptor = descriptor.one.mult,
                                  locale = my.locale)

plot(FEL.raw.mspct[["light"]])
plot(FEL.raw.mspct[["filter"]])
plot(FEL.raw.mspct[["dark"]])

D2.raw.mspct <- read_files2mspct(D2.lst,
                                 descriptor = descriptor.one.mult,
                                 locale = my.locale)

plot(D2.raw.mspct[["light"]])
plot(D2.raw.mspct[["filter"]])
plot(D2.raw.mspct[["dark"]])

pix.wavelengths <- which_descriptor()$wavelengths

FEL <- list(TK = 3106.87561155307, kc = 1.5032470318064E-10 * 3.74183488349896E-16 * 0.0000001,
            kb = c(1.48299373059776E-19, -6.73783136663075E-16, 1.28071973512943E-12, -1.31864190324975E-09, 7.93362535586965E-07, -0.000278602532732717, 0.052698171960183, -3.66729840729808))
comment(FEL) <- "FEL lamp BN-9101-165, from calibration document T-R_387.xls"

D2.UV653 <- c(-4.1090384E-17, 7.6305376E-14, -5.6315241E-11, 2.0714647E-08, -3.8162638E-06, 0.0002842542)
comment(D2.UV653) <- "D2 lamp UV-653"

# D2.UV586 <- c(7.1215397E-18, -4.0918226E-15, -2.9069045E-12, 3.0340054E-09, -8.9554589E-07, 9.1200585E-05)
# comment(D2.UV586) <- "D2 lamp UV-586"
#
# D2.UV654 <- c(-6.2181585E-17, 1.135044E-13, -8.2196585E-11, 2.9598246E-08, -5.3217495E-06, 0.00038528517)
# comment(D2.UV654) <- "D2 lamp UV-654"

new.descriptor <- compute_irrad_calibration(FEL.raw.mspct,
                                   D2.raw.mspct,
                                   pix.wavelengths,
                                   FEL.k = FEL,
                                   D2.k = D2.UV653,
                                   method = MAYP11278_ylianttila.mthd,
                                   verbose = TRUE)

plot(x = pix.wavelengths,
     y = new.descriptor$inst.calib$irrad.mult -
       which_descriptor()$inst.calib$irrad.mult)

plot(x = pix.wavelengths,
     y = new.descriptor$inst.calib$irrad.mult)

plot(x = pix.wavelengths,
     y = which_descriptor()$inst.calib$irrad.mult)

# Compare D2 lamp ---------------------------------------------------------


D2.lst <- list(light = "D2-light-short.txt",
               dark = "D2-dark-short.txt",
               filter = "D2-filter-short.txt")

D2.cps.spct <- s_irrad_corrected(D2.lst,
                                 descriptor = new.descriptor,
                                 method = MAYP11278_ylianttila.mthd,
                                 locale = my.locale,
                                 return.cps = TRUE)
plot(D2.cps.spct)

D2.irrad.spct <- s_irrad_corrected(D2.lst,
                                   descriptor = new.descriptor,
                                   method = MAYP11278_ylianttila.mthd,
                                   locale = my.locale)
plot(D2.irrad.spct)

plot(D2.irrad.spct) +
  geom_line(data = D2_spectrum(D2.irrad.spct$w.length) * 1e4, color = "red")


# Compare FEL lamp --------------------------------------------------------


FEL.lst <- list(light = c("FEL-light-short.txt", "FEL-light-long.txt"),
                dark = c("FEL-dark-short.txt", "FEL-dark-long.txt"),
                filter = "FEL-filter-long.txt")

FEL.cps.spct <- s_irrad_corrected(FEL.lst,
                                  descriptor = new.descriptor,
                                  method = MAYP11278_ylianttila.mthd,
                                  locale = my.locale,
                                  return.cps = TRUE)
plot(FEL.cps.spct)

FEL.irrad.spct <- s_irrad_corrected(FEL.lst,
                                    descriptor = new.descriptor,
                                    method = MAYP11278_ylianttila.mthd,
                                    locale = my.locale)
plot(FEL.irrad.spct) +
  geom_line(data = FEL_spectrum(FEL.irrad.spct$w.length) * 1e4, color = "red")

# Compare calibrations ----------------------------------------------------

calib.lasse <- generic_spct(w.length = which_descriptor()$wavelengths,
                               irrad.mult = which_descriptor()$inst.calib$irrad.mult,
                               cal.idx = "lasse")

calib.ooacq <- generic_spct(w.length = new.descriptor$wavelengths,
                               irrad.mult = new.descriptor$inst.calib$irrad.mult,
                               cal.idx = "ooacq")

ggplot(calib.lasse, aes(w.length, irrad.mult)) +
  geom_line() +
  geom_line(data = calib.ooacq, color = "red")


# Alternative method ------------------------------------------------------

new.mthd <- MAYP11278_ylianttila.mthd
new.mthd$stray.light.method <- "full"

new.descriptor <- compute_irrad_calibration(FEL.raw.mspct,
                                            D2.raw.mspct,
                                            pix.wavelengths,
                                            FEL.k = FEL,
                                            D2.k = D2.UV653,
                                            method = new.mthd,
                                            verbose = TRUE)

# Compare D2 lamp ---------------------------------------------------------


D2.irrad.spct <- s_irrad_corrected(D2.lst,
                                   descriptor = new.descriptor,
                                   method = MAYP11278_ylianttila.mthd,
                                   locale = my.locale)
plot(D2.irrad.spct)

plot(D2.irrad.spct) +
  geom_line(data = D2_spectrum(D2.irrad.spct$w.length) * 1e4, color = "red")


# Compare FEL lamp --------------------------------------------------------


FEL.irrad.spct <- s_irrad_corrected(FEL.lst,
                                    descriptor = new.descriptor,
                                    method = MAYP11278_ylianttila.mthd,
                                    locale = my.locale)
plot(FEL.irrad.spct) +
  geom_line(data = FEL_spectrum(FEL.irrad.spct$w.length) * 1e4, color = "red")

# Compare calibrations ----------------------------------------------------

calib.ooacq <- generic_spct(w.length = new.descriptor$wavelengths,
                            irrad.mult = new.descriptor$inst.calib$irrad.mult,
                            cal.idx = "ooacq")

ggplot(calib.lasse, aes(w.length, irrad.mult)) +
  geom_line() +
  geom_line(data = calib.ooacq, color = "red")



