## ---- echo=FALSE, message=FALSE------------------------------------------
require("knitr")
opts_knit$set(autodep = TRUE)

## ------------------------------------------------------------------------
library(ggspectra)
library(photobiology)
library(photobiologyWavebands)
library(ooacquire)

# print warnings at the time they are triggered
options(warn = 1)

sr.online <- FALSE

## ---- eval=sr.online, echo=FALSE, message=FALSE, warning=FALSE-----------
#  w <- start_session(error.action = warning)
#  sr.online <- rOmniDriver::number_srs(w) > 0
#  # is_valid_wrapper(w)

## ---- eval=sr.online-----------------------------------------------------
#  acq_irrad_interactive(method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_irrad_interactive(HDR.mult = 1,
#                        tot.time.range = c(0, Inf),
#                        method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_irrad_interactive(HDR.mult = c(1, 10),
#                        tot.time.range = 10,
#                        method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_irrad_interactive(HDR.mult = c(1, 10),
#                        tot.time.range = c(10, 20),
#                        method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_irrad_interactive(target.margin = 0.25,
#                        method = ooacquire::MAYP11278_ylianttila.mthd,
#                        descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_fraction_interactive(ref.value = 0.97,
#                           method = ooacquire::MAYP11278_ylianttila.mthd,
#                           descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=sr.online-----------------------------------------------------
#  acq_fraction_interactive(qty.out = "Rfr", type = "specular",
#                           method = ooacquire::MAYP11278_ylianttila.mthd,
#                           descriptors = ooacquire::MAYP11278_descriptors)

## ---- eval=-2------------------------------------------------------------
filepath <- system.file("example-scripts", "irrad-acq-interac.R", package="ooacquire")
file.copy(from = filepath, to = ".")

## ---- eval=-3------------------------------------------------------------
folderpath <- system.file("example-scripts", package="ooacquire")
list.files(path = folderpath, pattern = ".*[.]R")
file.copy(from = list.files(path = folderpath, pattern = ".*[.]R", full.names = TRUE), to = ".")

