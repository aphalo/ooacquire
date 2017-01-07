library(readxl)
library(readr)
library(ooacquire)

# create an object with the parameters for Lasse Ylianttila's method for Maya
MAYP112785_ylianttila.mthd <- list(
  stray.light.method = "original",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(204.6, 219.1),
  flt.ref.wl = c(368.3, 388.3),
  tail.coeffs = c(-9.22574221, -0.035984385),
  worker.fun = ooacquire::MAYP112785_tail_correction,
  trim = 0
)

## temporarily we load the old descriptor!!
## this needs to be replaced so as to get correct values!!!!
# load an instrument descriptor
load(file = "./data-raw/maya-descriptor/MAYP11278.Rda")

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
  # replace calibration in a copy of the descriptor
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
  descriptor.tmp[["spectrometer.sn"]] <- "MAYP112785"
  descriptors[[name.f]] <- descriptor.tmp
}

print(names(descriptors))

rm(tmp, f, name.f)

stopifnot(names(descriptors) == MAYP112785_calib_dates.df[["name"]])
stopifnot(basename(files) == MAYP112785_calib_dates.df[["coeffs.file"]])

MAYP112785_descriptors <- descriptors
save(MAYP112785_ylianttila.mthd,
     MAYP112785_descriptors,
     MAYP112785_calib_dates.df,
     file = "data/calibs-MAYP112785.rda")


