library(readxl)
library(readr)
library(ooacquire)

# create an object with the parameters for Lasse Ylianttila's method for Maya
MAYP11278_ylianttila.mthd <- list(
  method = "original",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(193, 209.5),
  flt.ref.wl = c(360, 379.5),
  worker_fun = ooacquire::maya_tail_correction,
  trim = 0
)

# load an instrument descriptor
load(file = "./data-raw/maya-descriptor/MAYP11278.Rda")

# find calibration files
files <- list.files("data-raw/calibrations-lasse",
                    pattern = "*.xlsx$",
                    full.names = TRUE)

MAYP11278_calib_dates.df <-
  read_csv("data-raw/calibrations-lasse/calibration-dates.csv", skip = 1)

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
  set_descriptor_wl(oo_descriptor = descriptor.tmp,
                    wl = tmp[["w.length"]])
  descriptor.tmp <-
    set_descriptor_irrad_mult(oo_descriptor = descriptor.tmp,
                              irrad.mult = tmp[["irrad.mult"]],
                              wl.range = c(251, 899),
                              start.date = MAYP11278_calib_dates.df[["start.date"]][date.row],
                              end.date = MAYP11278_calib_dates.df[["end.date"]][date.row])

  descriptor.tmp <- descriptors[[name.f]] <- descriptor.tmp
}

print(names(descriptors))

rm(tmp, f, name.f)

stopifnot(names(descriptors) == MAYP11278_calib_dates.df[["name"]])
stopifnot(basename(files) == MAYP11278_calib_dates.df[["coeffs.file"]])

MAYP11278_descriptors <- descriptors
save(MAYP11278_ylianttila.mthd,
     MAYP11278_descriptors,
     MAYP11278_calib_dates.df,
     file = "data/calibs-MAYP11278.rda")


