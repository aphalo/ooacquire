library(readr)
library(ooacquire)

# create an object with the parameters for Lasse Ylianttila's original method
# for Maya, not vavailable.
FLMS00416_ylianttila.mthd <- list()

# create an object with the parameters for a method good only for sunlight,
# not vavailable.
FLMS00416_sun.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
FLMS00416_simple.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
FLMS00416_none.mthd <- list(
  stray.light.method = "none",
  stray.light.wl = c(340.45, 346.16),
  flt.dark.wl = c(NA_real_, NA_real_),
  flt.ref.wl = c(NA_real_, NA_real_),
  flt.Tfr = 1,
  inst.dark.pixs = 1:16,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 0.05
)

# load an instrument descriptor
load(file = "./data-raw/flame-s-descriptor/FLMS00416.Rda")

# we make sure not to depend on Java code
descriptor$w <- NULL

descriptor$spectrometer.sn <- "FLMS00416"

descriptor <- set_descriptor_nl(descriptor,
                                nl.coeff = c(0.864123, 6.5517e-6, -1.06531e-10,
                                             4.70652e-16, 5.4788e-20, -3.1291e-24,
                                             6.61022e-29, -4.88409e-34))

# find calibration files
files <- list.files("data-raw/calibrations-FLMS00416",
                    pattern = "*cal.rda$",
                    full.names = TRUE)

FLMS00416_calib_dates.df <-
  read_csv("data-raw/calibrations-FLMS00416/calibration-dates.csv", skip = 1)

# create a new descriptor for each calibration file
descriptors <- list()
for (f in files) {
  print(f)
  date.row <- which(FLMS00416_calib_dates.df[["coeffs.file"]] == basename(f))
  name.f <- FLMS00416_calib_dates.df[["name"]][date.row]
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
                              wl.range = c(349.59, 1021.16),
                              start.date = FLMS00416_calib_dates.df[["start.date"]][date.row],
                              end.date = FLMS00416_calib_dates.df[["end.date"]][date.row])

  descriptor.tmp <- descriptors[[name.f]] <- descriptor.tmp
}

print(names(descriptors))

rm(tmp, f, name.f)

stopifnot(names(descriptors) == FLMS00416_calib_dates.df[["name"]])
stopifnot(basename(files) == FLMS00416_calib_dates.df[["coeffs.file"]])

FLMS00416_cal.spct <- cal.spct

FLMS00416_descriptors <- descriptors

save(FLMS00416_ylianttila.mthd,
     FLMS00416_sun.mthd,
     FLMS00416_simple.mthd,
     FLMS00416_none.mthd,
     FLMS00416_descriptors,
     FLMS00416_calib_dates.df,
     FLMS00416_cal.spct,
     file = "data/calibs-FLMS00416.rda")
