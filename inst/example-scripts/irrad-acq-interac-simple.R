# this is a simple example, see the help page of acq_irrad_interactive() for
# a full description of parameters and the arguments they accept
library(ooacquire)
library(lubridate)

photon_as_default()
acq_irrad_interactive(qty.out = "irrad",
                      protocols = "ld",
                      save.pdfs = TRUE,
                      save.summaries = FALSE,
                      save.collections = FALSE,
                      interface.mode = "auto-attr",
                      folder.name = sprintf("./TEST-%s", lubridate::today()))
