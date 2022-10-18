## ----"setup", include=FALSE---------------------------------------------------
require("knitr")
opts_knit$set(cache = FALSE, root.dir = system.file("extdata", package = "ooacquire"))

## -----------------------------------------------------------------------------
# change this to TRUE to run acquisition examples
# these examples require user interaction to complete!!
sr.online <- FALSE

## -----------------------------------------------------------------------------
library(ooacquire)

# print warnings at the time they are triggered
options(warn = 1)

## ---- eval = FALSE------------------------------------------------------------
#  acq_irrad_interactive()

## ---- eval=sr.online----------------------------------------------------------
#  acq_irrad_interactive(correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors,
#                        interface.mode = "simple")

## ---- eval=sr.online----------------------------------------------------------
#  acq_irrad_interactive(HDR.mult = 1,
#                        tot.time.range = c(0, Inf),
#                        correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online----------------------------------------------------------
#  acq_irrad_interactive(HDR.mult = c(short = 1, long = 10), # the default
#                        tot.time.range = c(5, 15), # the default
#                        correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online----------------------------------------------------------
#  acq_irrad_interactive(HDR.mult = c(1, 10),  # the default
#                        tot.time.range = 10,
#                        correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online----------------------------------------------------------
#  acq_irrad_interactive(HDR.mult = c(1, 10),
#                        tot.time.range = c(10, 20),
#                        correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online----------------------------------------------------------
#  acq_irrad_interactive(target.margin = 0.25,
#                        correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online----------------------------------------------------------
#  acq_fluence_interactive(correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                          descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online----------------------------------------------------------
#  acq_fluence_interactive(integ.time = 4, # seconds
#                          correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                          descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online----------------------------------------------------------
#  acq_fraction_interactive(ref.value = 0.97,
#                           correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                           descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online----------------------------------------------------------
#  acq_fraction_interactive(qty.out = "Rfr", type = "specular",
#                           correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                           descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=FALSE--------------------------------------------------------------
#  filepath <- system.file("example-scripts", "irrad-acq-interac.R", package="ooacquire")
#  file.copy(from = filepath, to = ".")

## ---- eval=FALSE--------------------------------------------------------------
#  folderpath <- system.file("example-scripts", package="ooacquire")
#  list.files(path = folderpath, pattern = ".*[.]R")
#  file.copy(from = list.files(path = folderpath, pattern = ".*[.]R", full.names = TRUE), to = ".")

