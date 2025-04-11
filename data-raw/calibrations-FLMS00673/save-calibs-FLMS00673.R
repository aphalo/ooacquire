library(readr)
library(ooacquire)

# load an instrument descriptor
load(file = "./data-raw/flame-s-descriptor/FLMS00440.Rda")

# we make sure not to depend on Java code
descriptor$w <- NULL

descriptor[["spectrometer.name"]] <- "Flame-S-XR1-ES"
descriptor[["spectrometer.sn"]] <- "FLMS00673"
descriptor[["bench.grating"]] <- "XR1"
descriptor[["bench.filter"]] <- "000"
descriptor[["bench.slit"]] <- "025"
descriptor[["bench-lens"]] <- "L2"
descriptor[["detector"]] <- "DET2B-200-1100"
descriptor[["num.pixs"]] <- 2048
descriptor[["num.dark.pixs"]] <- 16
descriptor[["min.integ.time"]] <- 1000
descriptor[["max.integ.time"]] <- 65535000
descriptor[["max.counts"]] <- 65535

descriptor[["time"]] <- NA
descriptor <- set_descriptor_nl(descriptor,
                                nl.coeff = c(0.878133, 8.83404e-6, -5.07091e-10,
                                             3.80507e-14, -1.84551e-18, 4.64183e-23,
                                             -5.62435e-28, 2.56084e-33))

# create an object with the parameters for Lasse Ylianttila's original method
# for Maya, not vavailable.
FLMS00673_ylianttila.mthd <- list()

# create an object with the parameters for a method good only for sunlight,
# not vavailable.
FLMS00673_sun.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
FLMS00673_simple.mthd <- list()

# create an object with the parameters for simple method for Jaz
# suitable for any light source, but not as good for irradiance.
FLMS00673_none.mthd <- list(
  spectrometer.sn = descriptor$spectrometer.sn,
  stray.light.method = "none",
  stray.light.wl = c(203.34, 222.85),
  flt.dark.wl = c(202.88, 195.42),
  flt.ref.wl = c(359.99, 379.64),
  flt.Tfr = 1,
  inst.dark.pixs = 3:16,
  tail.coeffs = c(NA_real_, NA_real_),
  worker.fun = NULL,
  trim = 1/16
)

# load an instrument descriptor
load(file = "./data-raw/flame-s-descriptor/FLMS00440.Rda")

# we make sure not to depend on Java code
descriptor$w <- NULL

descriptor[["spectrometer.name"]] <- "Flame-S-XR1-ES"
descriptor[["spectrometer.sn"]] <- "FLMS00673"
descriptor[["bench.grating"]] <- "XR1"
descriptor[["bench.filter"]] <- "000"
descriptor[["bench.slit"]] <- "025"
descriptor[["bench-lens"]] <- "L2"
descriptor[["detector"]] <- "DET2B-200-1100"
descriptor[["num.pixs"]] <- 2048
descriptor[["num.dark.pixs"]] <- 16
descriptor[["min.integ.time"]] <- 1000
descriptor[["max.integ.time"]] <- 65535000
descriptor[["max.counts"]] <- 65535

descriptor[["time"]] <- NA
descriptor <- set_descriptor_nl(descriptor,
                                nl.coeff = c(0.878133, 8.83404e-6, -5.07091e-10,
                                             3.80507e-14, -1.84551e-18, 4.64183e-23,
                                             -5.62435e-28, 2.56084e-33))

# find calibration files
files <- list.files("data-raw/calibrations-FLMS00673",
                    pattern = "*cal.rda$",
                    full.names = TRUE)

FLMS00673_calib_dates.df <-
  read_csv("data-raw/calibrations-FLMS00673/calibration-dates.csv",
           col_types = "ccTTTc",
           skip = 1,
           locale = locale(tz = "UTC"))

# create a new descriptor for each calibration file
descriptors <- list()
for (f in files) {
  print(f)
  date.row <- which(FLMS00673_calib_dates.df[["coeffs.file"]] == basename(f))
  name.f <- FLMS00673_calib_dates.df[["name"]][date.row]
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
  cal.wl.range[1] <- max(cal.wl.range[1], 250)
  descriptor.tmp <-
    set_descriptor_irrad_mult(descriptor = descriptor.tmp,
                              irrad.mult = tmp[["irrad.mult"]],
                              wl.range = cal.wl.range,
                              start.date = FLMS00673_calib_dates.df[["start.date"]][date.row],
                              end.date = FLMS00673_calib_dates.df[["end.date"]][date.row],
                              cal.source = FLMS00673_calib_dates.df[["name"]][date.row])

  descriptor.tmp$entrance.optics <-
    list(make = "Ocean Optics",
         model = NA_character_,
         geometry = "cosine",
         serial.number = NA_character_,
         area = NA_real_,
         fibre = NA_character_)

  descriptor.tmp <- descriptors[[name.f]] <- descriptor.tmp
}

print(names(descriptors))

rm(tmp, f, name.f)

stopifnot(names(descriptors) == FLMS00673_calib_dates.df[["name"]])
stopifnot(basename(files) == FLMS00673_calib_dates.df[["coeffs.file"]])

FLMS00673_cal.spct <- cal.spct

FLMS00673_descriptors <- descriptors

save(FLMS00673_ylianttila.mthd,
     FLMS00673_sun.mthd,
     FLMS00673_simple.mthd,
     FLMS00673_none.mthd,
     FLMS00673_descriptors,
     FLMS00673_calib_dates.df,
     FLMS00673_cal.spct,
     file = "data/calibs-FLMS00673.rda")

