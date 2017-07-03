# Using an spectrometer with a single channel
# Tested with a Maya 2000 Pro spectrometer

library(r4photobiology)
library(ggplot2)
library(ooacquire)

# print warnings when they are triggered
options(warn = 1)

# use default settings
acq_rfr_tfr_interactive(qty.out = "Tfr")

