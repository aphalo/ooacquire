library(photobiology)
library(photobiologyWavebands)
library(ggspectra)
library(ooacquire)

options(warn = 1)
oldwd <- setwd("./not-inst/maya-tests")
on.exit(setwd(oldwd), add = TRUE)

# photon_as_default()
# energy_as_default()
# set_w.band_default("VIS_bands()")
# set_w.band_default("Plant_bands()")
# set_w.band_default("UV_bands()")

theme_set(theme_bw())
ggpp::ttheme_set(ggpp::ttheme_gtbw())

acq_irrad_interactive(qty.out = "irrad", # qty.out = "cps", qty.out = "raw"
                      save.pdfs = TRUE,
                      interface.mode = "auto")

