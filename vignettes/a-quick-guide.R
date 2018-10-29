## ---- eval=-2------------------------------------------------------------
folderpath <- system.file("extdata", package="ooacquire")
file.copy(from = folderpath, to = ".", recursive = TRUE)

## ----"setup", include=FALSE----------------------------------------------
require("knitr")
opts_knit$set(cache = FALSE, root.dir = system.file("extdata", package = "ooacquire"))

## ------------------------------------------------------------------------
library(photobiology)
library(photobiologyWavebands)
library(ggspectra)
library(ooacquire)

# print warnings at the time they are triggered
options(warn = 1)

# change this to TRUE to run acquisition examples
sr.online <- FALSE

## ---- eval=sr.online, echo=FALSE, message=FALSE, warning=FALSE-----------
#  w <- start_session(error.action = warning)
#  sr.online <- rOmniDriver::number_srs(w) > 0
#  # is_valid_wrapper(w)

## ---- eval=sr.online-----------------------------------------------------
#  acq_irrad_interactive(correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_irrad_interactive(HDR.mult = 1,
#                        tot.time.range = c(0, Inf),
#                        correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_irrad_interactive(HDR.mult = c(1, 10),
#                        tot.time.range = 10,
#                        correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_irrad_interactive(HDR.mult = c(1, 10),
#                        tot.time.range = c(10, 20),
#                        correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_irrad_interactive(target.margin = 0.25,
#                        correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_fluence_interactive(correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_fluence_interactive(integ.time = 4, # seconds
#                          correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                          descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_fraction_interactive(ref.value = 0.97,
#                           correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                           descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_fraction_interactive(qty.out = "Rfr", type = "specular",
#                           correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                           descriptors = ooacquire::MAYP11278_descriptors)

## ------------------------------------------------------------------------
file_names <- list(light = c("irrad-files/light-short.txt",
                             "irrad-files/light-long.txt"),
                   filter = "irrad-files/flt-long.txt",
                   dark = c("irrad-files/dark-short.txt",
                            "irrad-files/dark-long.txt"))

## ------------------------------------------------------------------------
irrad.spct <- 
  s_irrad_corrected(x = file_names,
                    descriptor = which_descriptor("2016-10-11", 
                                                  MAYP11278_descriptors),
                    correction.method = MAYP11278_ylianttila.mthd)

## ---- fig.height=5, fig.width=7------------------------------------------
plot(irrad.spct)

## ---- eval=FALSE---------------------------------------------------------
#  cal.spct <- read_oo_caldata("oo-calibration/xxxx")

## ---- eval=FALSE---------------------------------------------------------
#  cal.multipliers <- oo_calib2irrad_mult(cal.spct,
#                                         diff.type = "CC-3")

## ---- eval=FALSE---------------------------------------------------------
#  filepath <- system.file("example-scripts", "irrad-acq-interac.R", package="ooacquire")
#  file.copy(from = filepath, to = ".")

## ---- eval=FALSE---------------------------------------------------------
#  folderpath <- system.file("example-scripts", package="ooacquire")
#  list.files(path = folderpath, pattern = ".*[.]R")
#  file.copy(from = list.files(path = folderpath, pattern = ".*[.]R", full.names = TRUE), to = ".")

