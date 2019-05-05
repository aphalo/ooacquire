library(readxl)
library(readr)
library(ooacquire)
library(polynom)

# load an instrument descriptor
load(file = "./data-raw/maya-descriptor/MAYP112785.Rda")
descriptor$num.pixs <- 2068
descriptor$num.dark.pixs <- 20

descriptor$w <- NULL
descriptor$sr.index <- 0L
descriptor$bad.pixs <- numeric()

# create an object with the parameters for Lasse Ylianttila's method for Maya
MAYP112785_ylianttila.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "original",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(204.6, 219.1),
  flt.ref.wl = c(368.3, 388.3),
  flt.Tfr = 1,
  inst.dark.pixs = 2:4,
  tail.coeffs = c(-9.22574221, -0.035984385),
  worker.fun = "MAYP112785_tail_correction",
  trim = 0
)

# create an object with the parameters for a method good only for sunlight,
# based on Lasse Ylianttila's original method suitable ONLY for sunlight.
MAYP112785_sun.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "sun",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(204.6, 219.1),
  flt.ref.wl = c(368.3, 388.3),
  flt.Tfr = 1,
  inst.dark.pixs = 2:4,
  tail.coeffs = c(-9.22574221, -0.035984385),
  worker.fun = "MAYP112785_tail_correction",
  trim = 0.05
)

# create an object with the parameters for simple method for Maya
# suitable for any light source, but not as good for sunlight
MAYP112785_simple.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "simple",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(204.6, 219.1),
  flt.ref.wl = c(368.3, 388.3),
  flt.Tfr = 1,
  inst.dark.pixs = 2:4,
  tail.coeffs = c(-9.22574221, -0.035984385),
  worker.fun = "MAYP112785_tail_correction",
  trim = 0.05
)

# create an object with the parameters for simple method for Maya
# suitable for any light source, but not as good for sunlight
MAYP112785_simple.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "none",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(204.6, 219.1),
  flt.ref.wl = c(368.3, 388.3),
  flt.Tfr = 1,
  inst.dark.pixs = NA_integer_,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 0.05
)

# find calibration files
files <- list.files("data-raw/calibrations-MAYP112785",
                    pattern = "*.xlsx$",
                    full.names = TRUE)

MAYP112785_calib_dates.df <-
  read_csv("data-raw/calibrations-MAYP112785/calibration-dates.csv", skip = 1)

# create a new descriptor for each calibration file
descriptors <- list()
for (f in files) {
  print(f)
  date.row <- which(MAYP112785_calib_dates.df[["coeffs.file"]] == basename(f))
  name.f <- gsub("-coeffs-", "_", gsub(".xlsx", "", basename(f), fixed = TRUE), fixed = TRUE)
  tmp <- read_excel(f)
  names(tmp) <- c("w.length", "irrad.mult")
  # make a copy of the descriptor
  descriptor.tmp <- descriptor
  descriptor.tmp <-
    set_descriptor_wl(descriptor = descriptor.tmp,
                      wl = tmp[["w.length"]])
  descriptor.tmp <-
    set_descriptor_irrad_mult(descriptor = descriptor.tmp,
                              irrad.mult = tmp[["irrad.mult"]] * 1e4,
                              wl.range = c(251, 899),
                              start.date = MAYP112785_calib_dates.df[["start.date"]][date.row],
                              end.date = MAYP112785_calib_dates.df[["end.date"]][date.row])

    descriptors[[name.f]] <- descriptor.tmp
}

print(names(descriptors))

rm(tmp, f, name.f)

stopifnot(names(descriptors) == MAYP112785_calib_dates.df[["name"]])
stopifnot(basename(files) == MAYP112785_calib_dates.df[["coeffs.file"]])

MAYP112785_descriptors <- descriptors
save(MAYP112785_ylianttila.mthd,
     MAYP112785_sun.mthd,
     MAYP112785_simple.mthd,
     MAYP112785_descriptors,
     MAYP112785_calib_dates.df,
     file = "data/calibs-MAYP112785.rda")


