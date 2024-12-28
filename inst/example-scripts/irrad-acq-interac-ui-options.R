library(ooacquire)
library(lubridate)

photon_as_default()
acq_irrad_interactive(qty.out = "irrad",
                      save.pdfs = TRUE,
                      save.summaries = TRUE,
                      save.collections = TRUE,
                      interface.mode = "auto-attr",
                      folder.name = sprintf("./inst-not/TEST-%s", lubridate::today()))

# example of tweaks to default user interface; not settable interactively!!
#
# qty.out decides what spectral values are returned in addition to raw counts
#   "irrad" -> energy or photon spectral irradiance
#   "cps" -> corrected counts per second
#   "raw" -> raw counts only
#
# save.pdfs decides if plots of the spectra are saved as pdf files [TRUE or FALSE]
#
# save.collections decides if the menu options to save collections of spectra are displayed
#
# save.summaries decides if summaries from collections are saved as .csv files [TRUE or FALSE]
#
# interface.mode decides which menu options are shown
#    "auto" -> full interface
#    "simple" -> simplified interface with auto-tuning but no setting of margin
#    "manual" -> no auto-tuning of integration time, set manually
#    "series" -> allows timed repeated "light" scans (under development)
#  ending "-attr" for any of the above, enable menu allowing user to enter
#  a character string to store in the comment and/or what.measured attributes
#
# folder.name can be used to provide a default folder name to save the output
# files from the session
#
#
