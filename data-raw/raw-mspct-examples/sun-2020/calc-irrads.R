library(photobiology)
library(photobiologyWavebands)
library(photobiologyPlants)
setwd("viikki-field-2020")
load("sun001.spct.Rda")
load("sun002.spct.Rda")
load("sun003.spct.Rda")
sun.mspct <- collect2mspct()
my_plant_bands <- list(UVB(), UVAsw(), UVAlw(),
                       waveband(c(400, 500), wb.name = "blue"),
                       Red("Smith10"), Far_red("Smith10"),
                       PAR())
q_irrad(sun.mspct, my_plant_bands, scale.factor = 1e6,
        attr2tb = c("when.measured")) -> sun_q_irrad.tb

R_FR(sun.mspct)

