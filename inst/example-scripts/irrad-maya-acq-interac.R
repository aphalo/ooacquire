library(ggspectra)
library(photobiology)
library(photobiologyWavebands)
library(ooacquire)

acq_irrad_interactive(HDR.mult = 1)

acq_irrad_interactive(HDR.mult = 1, tot.time.range = 10)

acq_irrad_interactive(HDR.mult = 1, tot.time.range = c(0.1, 10))

acq_irrad_interactive(HDR.mult = c(1, 10))

acq_irrad_interactive(HDR.mult = c(1, 10), tot.time.range = 10)

acq_irrad_interactive(HDR.mult = c(1, 10), tot.time.range = c(0.1, 10))

acq_irrad_interactive(HDR.mult = c(1, 7, 50), tot.time.range = 30)

acq_trans_interactive(HDR.mult = 1)

acq_trans_interactive(HDR.mult = 1, tot.time.range = 10)

acq_trans_interactive(HDR.mult = 1, tot.time.range = c(0.1, 10))

acq_trans_interactive(HDR.mult = c(1, 10))

acq_trans_interactive(HDR.mult = c(1, 10), tot.time.range = 10)

acq_trans_interactive(HDR.mult = c(1, 10), tot.time.range = c(0.1, 10))

acq_trans_interactive(HDR.mult = c(1, 7, 50), tot.time.range = 30)
