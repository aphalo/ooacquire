library(readxl)
library(readr)
library(ooacquire)

# load an instrument descriptor
load(file = "./data-raw/maya-descriptor/MAYP11278.Rda")

descriptor$num.pixs <- 2068
descriptor$num.dark.pixs <- 20

descriptor$w <- NULL

# inst.dark.pixs is set to 2:4 as reading from pixel 1 is not consistent.

# create an object with the parameters for Lasse Ylianttila's original method
# for Maya suitable for sunlight
MAYP11278_ylianttila.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "original",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(193, 209.5),        # used for $N$2 in Lasse's calc worksheet
  flt.ref.wl = c(360, 379.5),         # used for $N$3 in Lasse's calc worksheet
  flt.Tfr = 1,
  inst.dark.pixs = 2:4,
  tail.coeffs = c(-7.273130, -0.05688),
  worker.fun = "MAYP11278_tail_correction",
  trim = 0
)

# create an object with the parameters for Lasse Ylianttila's original method
# for Maya suitable for sunlight modified by shifting the flt reference -10nm
MAYP11278_short_flt_ref.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "original",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(193, 209.5),        # used for $N$2 in Lasse's calc worksheet
  flt.ref.wl = c(350, 369.5),         # used for $N$3 in Lasse's calc worksheet
  flt.Tfr = 1,
  inst.dark.pixs = 2:4,
  tail.coeffs = c(-7.273130, -0.05688),
  worker.fun = "MAYP11278_tail_correction",
  trim = 0
)

# create an object with the parameters for a method good only for sunlight,
# based on Lasse Ylianttila's original method suitable ONLY for sunlight.
MAYP11278_sun.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "sun",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(193, 209.5),
  flt.ref.wl = c(360, 379.5),
  flt.Tfr = 1,
  inst.dark.pixs = 2:4,
  tail.coeffs = c(-7.273130, -0.05688),
  worker.fun = "MAYP11278_tail_correction",
  trim = 0.05
)

# create an object with the parameters for simple method for Maya
# suitable for any light source, but not as good for sunlight
MAYP11278_simple.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "simple",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(193, 209.5),
  flt.ref.wl = c(360, 379.5),
  flt.Tfr = 1,
  inst.dark.pixs = 2:4,
  tail.coeffs = c(-7.273130, -0.05688),
  worker.fun = "MAYP11278_tail_correction",
  trim = 0.05
)

# create an object with the parameters for skipping correction method for Maya
# suitable for any light source, but not as good for sunlight
MAYP11278_none.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "none",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(193, 209.5),
  flt.ref.wl = c(360, 379.5),
  flt.Tfr = 1,
  inst.dark.pixs = NA_integer_,
  tail.coeffs =  c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 0
)

# find calibration files
files <- list.files("data-raw/calibrations-MAYP11278",
                    pattern = "*.xlsx$",
                    full.names = TRUE)

MAYP11278_calib_dates.df <-
  read_csv("data-raw/calibrations-MAYP11278/calibration-dates.csv",
           col_types = "cccTTTcc",
           skip = 1,
           locale = locale(tz = "UTC"))

# create a new descriptor for each calibration file
descriptors <- list()
for (f in files) {
  print(f)
  date.row <- which(MAYP11278_calib_dates.df[["coeffs.file"]] == basename(f))
  name.f <- gsub("-coeffs-", "_", gsub(".xlsx", "", basename(f), fixed = TRUE), fixed = TRUE)
  tmp <- read_excel(f)
  names(tmp) <- c("w.length", "irrad.mult")
  # replace calibration in a copy of the descriptor
  descriptor.tmp <- descriptor
  descriptor.tmp <-
    set_descriptor_wl(descriptor = descriptor.tmp,
                      wl = tmp[["w.length"]])
  cal.wl.range <-  tmp[["w.length"]][range(which(tmp[["irrad.mult"]] != 0L))]
  cal.wl.range <-  c(ceiling(cal.wl.range[1]), floor(cal.wl.range[2]))
  cal.wl.range[1] <- max(cal.wl.range[1], 250)
  cal.wl.range[2] <- min(cal.wl.range[2], 1050)
  descriptor.tmp <-
    set_descriptor_irrad_mult(descriptor = descriptor.tmp,
                              irrad.mult = tmp[["irrad.mult"]] * 1e4,
                              wl.range = cal.wl.range,
                              start.date = MAYP11278_calib_dates.df[["start.date"]][date.row],
                              end.date = MAYP11278_calib_dates.df[["end.date"]][date.row],
                              cal.source = MAYP11278_calib_dates.df[["name"]][date.row])

  diffuser.filename <- paste("data-raw/calibrations-MAYP11278/",
                             MAYP11278_calib_dates.df[["diffuser.file"]][date.row],
                             sep = "")
  descriptor.tmp$entrance.optics <- readRDS(diffuser.filename)

  descriptors[[name.f]] <- descriptor.tmp
}

print(names(descriptors))

rm(tmp, f, name.f)

stopifnot(names(descriptors) == MAYP11278_calib_dates.df[["name"]])
stopifnot(basename(files) == MAYP11278_calib_dates.df[["coeffs.file"]])

MAYP11278_descriptors <- descriptors
save(MAYP11278_ylianttila.mthd,
     MAYP11278_short_flt_ref.mthd,
     MAYP11278_sun.mthd,
     MAYP11278_simple.mthd,
     MAYP11278_descriptors,
     MAYP11278_calib_dates.df,
     file = "data/calibs-MAYP11278.rda")


