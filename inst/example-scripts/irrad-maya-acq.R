library(photobiology)
library(ggspectra)
library(ooacquire)

w <- start_session()

list_instruments(w)

# we use the descriptor stored in the package valid for "today"

descriptor <- which_descriptor()

descriptor[["w"]] <- w

# PROTOCOL 1
# set measurement settings for automatic adjustment
# there is no restriction for HDR settings but lengths between 1 and 3
# seem reasonable.
#
settings <- acq_settings(descriptor,
                         HDR.mult = c(1,10),
                         tot.time.range = c(10,10))

# automatic setting of integration time
settings <- tune_acq_settings(descriptor, settings)
settings

# acquire a single spectrum from spectrometer
spct_1 <- acq_raw_spct(descriptor, settings)
plot(spct_1)

# we use the high level function

spct_1 <- s_irrad_corrected(spct_1, method = MAYP11278_ylianttila.mthd)
plot(spct_1, unit.out = "photon")

## PROTOCOL 2
# acquire a measure in the light and a dark reference spectrum
# (The character vector for protocol can be of any length, and the strings
# can be anything you want. A prompt is issued if two consecutive strings
# are different. A protocol like c(rep("light", 20), "filter", "dark") is
# legal.)
#
# We first tune again the settings in case the light level has changed.
# using as starting value that stored in settings
settings <- tune_acq_settings(descriptor, settings)

# we acquire three pairs of bracketed spectra
mspct_2 <- acq_raw_mspct(descriptor, settings,
                         protocol = c("light", "filter", "dark"),
                         user.label = "my.test")

spct_2 <- s_irrad_corrected(mspct_2, method = MAYP11278_ylianttila.mthd, verbose = TRUE)

plot(spct_2, unit.out = "photon")

## PROTOCOL 3
# acquire a measure in the light and a dark reference spectrum
# (The character vector for protocol can be of any length, and the strings
# can be anything you want. A prompt is issued if two consecutive strings
# are different. A protocol like c(rep("light", 20), "filter", "dark") is
# legal.)
#
# We first tune again the settings in case the light level has changed.
# using as starting value that stored in settings
settings <- tune_acq_settings(descriptor, settings)

# we acquire three pairs of bracketed spectra
mspct_3 <- acq_raw_mspct(descriptor, settings,
                         protocol = c("light", "dark"),
                         user.label = "my.test")

spct_3 <- s_irrad_corrected(mspct_3, method = MAYP11278_ylianttila.mthd, verbose = TRUE)

plot(spct_3, unit.out = "photon")

## PROTOCOL 4
# acquire a measure in the light and a dark reference spectrum
# (The character vector for protocol can be of any length, and the strings
# can be anything you want. A prompt is issued if two consecutive strings
# are different. A protocol like c(rep("light", 20), "filter", "dark") is
# legal.)
#
# We first tune again the settings in case the light level has changed.

settings <- acq_settings(descriptor,
                         HDR.mult = 1,
                         tot.time.range = 5)

# using as starting value that stored in settings
settings <- tune_acq_settings(descriptor, settings)

# we acquire three pairs of bracketed spectra
mspct_4 <- acq_raw_mspct(descriptor, settings,
                         protocol = c("light", "dark"),
                         user.label = "my.test")

spct_4 <- s_irrad_corrected(mspct_4, method = MAYP11278_ylianttila.mthd)

plot(spct_4, unit.out = "photon")

end_session(w)

