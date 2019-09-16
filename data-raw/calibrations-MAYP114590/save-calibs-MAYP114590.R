library(readr)
library(ooacquire)

# load an instrument descriptor
load(file = "./data-raw/calibrations-MAYP114590/MAYP114590.Rda")

# we make sure not to depend on Java code
descriptor$w <- NULL

# descriptor[["bench-lens"]] <- "L2-C"
# descriptor[["detector"]] <- "DET2B-200-850"

descriptor[["time"]] <- NA

# create an object with the parameters for Lasse Ylianttila's original method
# for Maya, not vavailable.
MAYP114590_ylianttila.mthd <- list()

# create an object with the parameters for a method good only for sunlight,
# not vavailable.
MAYP114590_sun.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
MAYP114590_simple.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "simple",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(204.6, 219.1),
  flt.ref.wl = c(368.3, 388.3),
  flt.Tfr = 1,
  inst.dark.pixs = 2:4,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 0.05
)

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
MAYP114590_none.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "none",
  stray.light.wl = c(NA_real_, NA_real_),
  flt.dark.wl = c(204.6, 219.1),
  flt.ref.wl = c(NA_real_, NA_real_),
  flt.Tfr = 1,
  inst.dark.pixs = 2:4,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 0.05
)

# find calibration files
files <- list.files("data-raw/calibrations-MAYP114590",
                    pattern = "*cal.rda$",
                    full.names = TRUE)

MAYP114590_calib_dates.df <-
  read_csv("data-raw/calibrations-MAYP114590/calibration-dates.csv", col_types = "ccDDDc", skip = 1)

# create a new descriptor for each calibration file
descriptors <- list()
for (f in files) {
  print(f)
  date.row <- which(MAYP114590_calib_dates.df[["coeffs.file"]] == basename(f))
  name.f <- MAYP114590_calib_dates.df[["name"]][date.row]
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
                              start.date = MAYP114590_calib_dates.df[["start.date"]][date.row],
                              end.date = MAYP114590_calib_dates.df[["end.date"]][date.row])

  descriptors[[name.f]] <- descriptor.tmp
}

print(names(descriptors))

rm(tmp, f, name.f)

stopifnot(names(descriptors) == MAYP114590_calib_dates.df[["name"]])
stopifnot(basename(files) == MAYP114590_calib_dates.df[["coeffs.file"]])

MAYP114590_cal.spct <- cal.spct

MAYP114590_descriptors <- descriptors

save(MAYP114590_ylianttila.mthd,
     MAYP114590_sun.mthd,
     MAYP114590_simple.mthd,
     MAYP114590_none.mthd,
     MAYP114590_descriptors,
     MAYP114590_calib_dates.df,
     MAYP114590_cal.spct,
     file = "data/calibs-MAYP114590.rda")

