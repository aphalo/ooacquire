# clear environment
rm(list = ls(pattern = "*"))

library(readr)
library(ooacquire)

# load an instrument descriptor
load(file = "./data-raw/flame-s-descriptor/FLMS00440.Rda")

# we make sure not to depend on Java code
descriptor$w <- NULL

descriptor <- set_descriptor_nl(descriptor,
                                nl.coeff = c(0.847773, 6.69476e-6, 2.72833e-11,
                                             -9.19219e-15, 3.13902e-19, -5.59827e-24,
                                             5.83296e-29, -2.88581e-34))

# create an object with the parameters for Lasse Ylianttila's original method
# for Maya, not vavailable.
FLMS00440_ylianttila.mthd <- list()

# create an object with the parameters for a method good only for sunlight,
# not vavailable.
FLMS00440_sun.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
FLMS00440_simple.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
FLMS00440_none.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "none",
  stray.light.wl = c(NA_real_, NA_real_),
  flt.dark.wl = c(340.17, 346.3),
  flt.ref.wl = c(NA_real_, NA_real_),
  flt.Tfr = 1,
  inst.dark.pixs = 1:16,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 0.05
)

# find calibration files
files <- list.files("data-raw/calibrations-FLMS00440",
                    pattern = "*cal.rda$",
                    full.names = TRUE)

FLMS00440_calib_dates.df <-
  read_csv("data-raw/calibrations-FLMS00440/calibration-dates.csv", col_types = "ccDDDc", skip = 1)

# create a new descriptor for each calibration file
descriptors <- list()
for (f in files) {
  print(f)
  date.row <- which(FLMS00440_calib_dates.df[["coeffs.file"]] == basename(f))
  name.f <- FLMS00440_calib_dates.df[["name"]][date.row]
  load(f)
  tmp <- cal.spct
#  names(tmp) <- c("w.length", "irrad.mult")
  # replace calibration in a copy of the descriptor
  descriptor.tmp <- descriptor
  descriptor.tmp <-
    set_descriptor_wl(descriptor = descriptor.tmp,
                      wl = tmp[["w.length"]])
  cal.wl.range <-  tmp[["w.length"]][range(which(tmp[["irrad.mult"]] != 0L))]
  cal.wl.range <-  c(ceiling(cal.wl.range[1]), floor(cal.wl.range[2]))
  cal.wl.range[1] <- max(cal.wl.range[1], 355)
  descriptor.tmp <-
    set_descriptor_irrad_mult(descriptor = descriptor.tmp,
                              irrad.mult = tmp[["irrad.mult"]],
                              wl.range = cal.wl.range,
                              start.date = FLMS00440_calib_dates.df[["start.date"]][date.row],
                              end.date = FLMS00440_calib_dates.df[["end.date"]][date.row],
                              cal.source = MAYP11278_calib_dates.df[["name"]][date.row])

  descriptor.tmp <- descriptors[[name.f]] <- descriptor.tmp
}

print(names(descriptors))

rm(tmp, f, name.f)

stopifnot(names(descriptors) == FLMS00440_calib_dates.df[["name"]])
stopifnot(basename(files) == FLMS00440_calib_dates.df[["coeffs.file"]])

FLMS00440_cal.spct <- cal.spct

FLMS00440_descriptors <- descriptors

save(FLMS00440_ylianttila.mthd,
     FLMS00440_sun.mthd,
     FLMS00440_simple.mthd,
     FLMS00440_none.mthd,
     FLMS00440_descriptors,
     FLMS00440_calib_dates.df,
     FLMS00440_cal.spct,
     file = "data/calibs-FLMS00440.rda")

