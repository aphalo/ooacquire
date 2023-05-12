library(readr)
library(ooacquire)

# load an instrument descriptor
load(file = "./data-raw/flame-s-descriptor/FLMS04133.Rda")

# we make sure not to depend on Java code
descriptor$w <- NULL

descriptor[["bench-lens"]] <- "L2-C"
descriptor[["detector"]] <- "DET2B-200-850"

descriptor[["time"]] <- NA

# create an object with the parameters for Lasse Ylianttila's original method
# for Maya, not vavailable.
FLMS04133_ylianttila.mthd <- list()

# create an object with the parameters for a method good only for sunlight,
# not vavailable.
FLMS04133_sun.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
FLMS04133_simple.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "simple",
  stray.light.wl = c(240.01, 249.89),
  flt.dark.wl = c(240.01, 249.89),
  flt.ref.wl = c(360.0815, 379.8590),
  flt.Tfr = 1,
  inst.dark.pixs = 1:16,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 1/8
)

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
FLMS04133_none.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "none",
  stray.light.wl = c(NA_real_, NA_real_),
  flt.dark.wl = c(240.01, 249.89),
  flt.ref.wl = c(NA_real_, NA_real_),
  flt.Tfr = 1,
  inst.dark.pixs = 1:16,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 1/8
)

# find calibration files
files <- list.files("data-raw/calibrations-FLMS04133",
                    pattern = "*cal.rda$",
                    full.names = TRUE)

FLMS04133_calib_dates.df <-
  read_csv("data-raw/calibrations-FLMS04133/calibration-dates.csv", col_types = "ccTTTc", skip = 1)

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
  cal.wl.range <-  tmp[["w.length"]][range(which(tmp[["irrad.mult"]] != 0L))]
  cal.wl.range <-  c(ceiling(cal.wl.range[1]), floor(cal.wl.range[2]))
  cal.wl.range[1] <- max(cal.wl.range[1], 255)
  descriptor.tmp <-
    set_descriptor_irrad_mult(descriptor = descriptor.tmp,
                              irrad.mult = tmp[["irrad.mult"]],
                              wl.range = cal.wl.range,
                              start.date = FLMS04133_calib_dates.df[["start.date"]][date.row],
                              end.date = FLMS04133_calib_dates.df[["end.date"]][date.row],
                              cal.source = MAYP11278_calib_dates.df[["name"]][date.row])

  descriptors[[name.f]] <- descriptor.tmp
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

