library(readxl)
library(readr)
library(ooacquire)

# create an object with the parameters for Lasse Ylianttila's original method
# for Maya, not vavailable.
FLMS00440_ylianttila.mthd <- list()

# create an object with the parameters for a method good only for sunlight,
# not vavailable.
FLMS00440_sun.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
FLMS00440_simple.mthd <- list(
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
FLMS00440_none.mthd <- list(
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
load(file = "./data-raw/jaz-descriptor/FLMS00440.Rda")

# we make sure not to depend on Java code
descriptor$w <- NULL

# find calibration files
files <- list.files("data-raw/calibrations-FLMS00440",
                    pattern = "*.xlsx$",
                    full.names = TRUE)

FLMS00440_calib_dates.df <-
  read_csv("data-raw/calibrations-FLMS00440/calibration-dates.csv", skip = 1)

# create a new descriptor for each calibration file
descriptors <- list()
for (f in files) {
  print(f)
  date.row <- which(FLMS00440_calib_dates.df[["coeffs.file"]] == basename(f))
  name.f <- gsub("-coeffs-", "_", gsub(".xlsx", "", basename(f), fixed = TRUE), fixed = TRUE)
  tmp <- read_excel(f)
  names(tmp) <- c("w.length", "irrad.mult")
  # replace calibration in a copy of the descriptor
  descriptor.tmp <- descriptor
  descriptor.tmp <-
    set_descriptor_wl(descriptor = descriptor.tmp,
                      wl = tmp[["w.length"]])
  descriptor.tmp <-
    set_descriptor_irrad_mult(descriptor = descriptor.tmp,
                              irrad.mult = tmp[["irrad.mult"]] * 1e4,
                              wl.range = c(251, 899),
                              start.date = FLMS00440_calib_dates.df[["start.date"]][date.row],
                              end.date = FLMS00440_calib_dates.df[["end.date"]][date.row])

  descriptor.tmp <- descriptors[[name.f]] <- descriptor.tmp
}

print(names(descriptors))

rm(tmp, f, name.f)

stopifnot(names(descriptors) == FLMS00440_calib_dates.df[["name"]])
stopifnot(basename(files) == FLMS00440_calib_dates.df[["coeffs.file"]])

FLMS00440_descriptors <- descriptors
save(FLMS00440_ylianttila.mthd,
     FLMS00440_sun.mthd,
     FLMS00440_simple.mthd,
     FLMS00440_descriptors,
     FLMS00440_calib_dates.df,
     file = "data/calibs-FLMS00440.rda")

