library(readxl)
library(readr)
library(ooacquire)

# create an object with the parameters for Lasse Ylianttila's original method
# for Maya, not vavailable.
JAZA3098_ch1_ylianttila.mthd <- list()

# create an object with the parameters for a method good only for sunlight,
# not vavailable.
JAZA3098_ch1_sun.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
JAZA3098_ch1_simple.mthd <- list(
  stray.light.method = "simple",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(199.3, 215.1),
  flt.ref.wl = c(360, 379.5),
  flt.Tfr = 1,
  inst.dark.pixs = 1:24,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 0.05
)

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
JAZA3098_ch1_none.mthd <- list(
  stray.light.method = "none",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(NA_real_, NA_real_),
  flt.ref.wl = c(NA_real_, NA_real_),
  flt.Tfr = 1,
  inst.dark.pixs = 1:24,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 0.05
)

# create an object with the parameters for Lasse Ylianttila's original method
# for Maya, not vavailable.
JAZA3098_ch2_ylianttila.mthd <- list()

# create an object with the parameters for a method good only for sunlight,
# not vavailable.
JAZA3098_ch2_sun.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
JAZA3098_ch2_simple.mthd <- list(
  stray.light.method = "simple",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(199.3, 215.1),
  flt.ref.wl = c(360, 379.5),
  flt.Tfr = 1,
  inst.dark.pixs = 1:24,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 0.05
)

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
JAZA3098_ch2_none.mthd <- list(
  stray.light.method = "none",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(NA_real_, NA_real_),
  flt.ref.wl = c(NA_real_, NA_real_),
  flt.Tfr = 1,
  inst.dark.pixs = 1:24,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 0.05
)

# load an instrument descriptor
load(file = "./data-raw/jaz-descriptor/JAZA3098.Rda")

# we make sure not to depend on Java code
descriptor_ch1$w <- NULL
descriptor_ch2$w <- NULL

# we do not have calibration files for this spectrometer

# create a new descriptor for each channel with 1 as calibration multipliers
descriptors <- list(JAZ3098_ch1 = descriptor_ch1, JAZ3098_ch2 = descriptor_ch2)
for (d in names(descriptors)) {
  # replace calibration in a copy of the descriptor
  descriptor.tmp <- descriptors[[d]]
  descriptor.tmp <-
    set_descriptor_irrad_mult(descriptor = descriptor.tmp,
                              irrad.mult = rep(1, length(descriptor.tmp[["wavelengths"]])),
                              wl.range = c(250, 890), # 190..250 used as reference
                              start.date = lubridate::ymd("2014-01-01"),
                              end.date = lubridate::ymd("2024-01-01"))
  descriptors[[d]] <- descriptor.tmp
}

print(names(descriptors))

rm(descriptor.tmp, d)

stopifnot(names(descriptors) == c("JAZ3098_ch1", "JAZ3098_ch2"))

JAZA3098_calib_dates.df <-
  read_csv("data-raw/calibrations-JAZA3098/calibration-dates.csv", skip = 1)

# Need to be lists of descriptors, even if of length 1.
JAZA3098_ch1_descriptors <- descriptors["JAZ3098_ch1"]
JAZA3098_ch2_descriptors <- descriptors["JAZ3098_ch2"]
save(JAZA3098_ch1_ylianttila.mthd,
     JAZA3098_ch1_sun.mthd,
     JAZA3098_ch1_simple.mthd,
     JAZA3098_ch1_none.mthd,
     JAZA3098_ch1_descriptors,
     JAZA3098_ch2_ylianttila.mthd,
     JAZA3098_ch2_sun.mthd,
     JAZA3098_ch2_simple.mthd,
     JAZA3098_ch2_none.mthd,JAZA3098_ch2_descriptors,
     JAZA3098_calib_dates.df,
     file = "data/calibs-JAZA3098.rda")


