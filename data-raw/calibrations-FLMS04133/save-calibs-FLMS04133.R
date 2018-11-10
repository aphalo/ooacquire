library(readr)
library(ooacquire)

# create an object with the parameters for Lasse Ylianttila's original method
# for Maya, not vavailable.
FLMS04133_ylianttila.mthd <- list()

# create an object with the parameters for a method good only for sunlight,
# not vavailable.
FLMS04133_sun.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
FLMS04133_simple.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
FLMS04133_none.mthd <- list(
  stray.light.method = "none",
  stray.light.wl = c(189.91, 195.59),
  flt.dark.wl = c(NA_real_, NA_real_),
  flt.ref.wl = c(NA_real_, NA_real_),
  flt.Tfr = 1,
  inst.dark.pixs = 1:16,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 0.05
)

# load an instrument descriptor
load(file = "./data-raw/flame-s-descriptor/FLMS00440.Rda")

# we make sure not to depend on Java code
descriptor$w <- NULL

descriptor[["spectrometer.name"]] <- "Flame-S-RAD"
descriptor[["spectrometer.sn"]] <- "FLMS04133"
descriptor[["bench.grating"]] <- "02"
descriptor[["bench.filter"]] <- "000"
descriptor[["bench.slit"]] <- "050"
descriptor[["bench-lens"]] <- "L2-C"
descriptor[["detector"]] <- "DET2B-200-850"
descriptor[["num.pixs"]] <- 2048
descriptor[["num.dark.pixs"]] <- 16
descriptor[["min.integ.time"]] <- 1000
descriptor[["max.integ.time"]] <- 65535000
descriptor[["max.counts"]] <- 65535

descriptor[["time"]] <- NA
descriptor <- set_descriptor_nl(descriptor,
                                nl.coeff = c(0.893344, 7.20663e-6, -1.33919e-10,
                                            -2.26498e-15, -8.91163e-20, 4.24693e-25,
                                             -2.89509e-29, 1.55269e-34))

# find calibration files
files <- list.files("data-raw/calibrations-FLMS04133",
                    pattern = "*cal.rda$",
                    full.names = TRUE)

FLMS04133_calib_dates.df <-
  read_csv("data-raw/calibrations-FLMS04133/calibration-dates.csv", skip = 1)

# create a new descriptor for each calibration file
descriptors <- list()
for (f in files) {
  print(f)
  date.row <- which(FLMS04133_calib_dates.df[["coeffs.file"]] == basename(f))
  name.f <- FLMS04133_calib_dates.df[["name"]][date.row]
  load(f)
  tmp <- cal.spct
#  names(tmp) <- c("w.length", "irrad.mult")
  # replace calibration in a copy of the descriptor
  descriptor.tmp <- descriptor
  descriptor.tmp <-
    set_descriptor_wl(descriptor = descriptor.tmp,
                      wl = tmp[["w.length"]])
  descriptor.tmp <-
    set_descriptor_irrad_mult(descriptor = descriptor.tmp,
                              irrad.mult = tmp[["irrad.mult"]],
                              wl.range = c(230, 850),
                              start.date = FLMS04133_calib_dates.df[["start.date"]][date.row],
                              end.date = FLMS04133_calib_dates.df[["end.date"]][date.row])

  descriptor.tmp <- descriptors[[name.f]] <- descriptor.tmp
}

print(names(descriptors))

rm(tmp, f, name.f)

stopifnot(names(descriptors) == FLMS04133_calib_dates.df[["name"]])
stopifnot(basename(files) == FLMS04133_calib_dates.df[["coeffs.file"]])

FLMS04133_cal.spct <- cal.spct

FLMS04133_descriptors <- descriptors

save(FLMS04133_ylianttila.mthd,
     FLMS04133_sun.mthd,
     FLMS04133_simple.mthd,
     FLMS04133_none.mthd,
     FLMS04133_descriptors,
     FLMS04133_calib_dates.df,
     FLMS04133_cal.spct,
     file = "data/calibs-FLMS04133.rda")

