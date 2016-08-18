library(photobiology)
library(ggplot2)
library(ggspectra)
library(ooacquire)

w <- start_session()

list_instruments(w)

# get a descriptor for the first channel of the first spectrometer
descriptor <- get_oo_descriptor(w)
# set Maya hot pixels and correct time limits
descriptor <- set_descriptor_bad_pixs(descriptor, c(123,380,1829,1994))
descriptor <- set_descriptor_integ_time(descriptor, NA, 7.2)

#

## PROTOCOL 0
# set measurement settings directly
settings <- acq_settings(descriptor,
                         HDR.mult = 1,
                         integ.time = 60,
                         num.scans = 1)

spct_0 <- acq_raw_spct(descriptor, settings)
plot(spct_0)

# we stop here because the settings most likely have been unsuitable
# if we had used correct values then processing could continue as below

# PROTOCOL 1
# set measurement settings for automatic adjustment
# there is no restriction for HDR settings but lengths between 1 and 3
# seem reasonable.
#
settings <- acq_settings(descriptor,
                         HDR.mult = c(1,10),
                         tot.time.range = c(5,10))

settings <- tune_acq_settings(descriptor, settings)
settings

spct_1.acq <- acq_raw_spct(descriptor, settings)
plot(spct_1.acq)

# processing of raw counts

spct_1 <- trim_counts(spct_1.acq)
plot(spct_1)

spct_1 <- linearize_counts(spct_1)
plot(spct_1)

# use an internal "dark" reference from 191 to 350 nm
spct_1 <- fshift(spct_1, range = c(191,290))
plot(spct_1)

# conversion to counts per second
spct_1a <- raw2cps(spct_1)
plot(spct_1a)

# merge data from long and short integration times
spct_1b <- merge_cps(spct_1a)
plot(spct_1b)

## PROTOCOL 2
# acquire a measure in the light and a dark reference spectrum
# (The character vector for protocol can be of any length, and the strings
# can be anything you want. A prompt is issued if two consecutive strings
# are different. A protocol like c(rep("light", 20), "filter", "dark") is
# legal.)
#
# We first tune again the settings in case the light level has changed.
settings <- tune_acq_settings(descriptor, settings)

# we acquire two pairs of bracketed spectra
mspct_1 <- acq_raw_mspct(descriptor, settings,
                         protocol = c("light", "filter", "dark"),
                         user.label = "window.sun")
mspct_1

## use new function
##

corrected.spct <- uvb_corrections(x = mspct_1[[1]],
                            flt = mspct_1[[2]],
                            dark = mspct_1[[3]],
                            method = "original",
                            stray.light.wl = c(218.5, 228.5),
                            flt.dark.wl = c(193, 209.5),
                            flt.ref.wl = c(360, 379.5),
                            worker_fun = maya_tail_correction,
                            trim = 0,
                            verbose = FALSE)
class(mspct_1)

mslply(mspct_1, getWhatMeasured)
mslply(mspct_1, getWhenMeasured)

plot(mspct_1[["spct_1"]], annotations = NULL) +
  ylim(0, max(mspct_1[["spct_1"]]$counts_1))
plot(mspct_1[["spct_2"]], annotations = NULL) +
  ylim(0, max(mspct_1[["spct_1"]]$counts_1))

# processing of raw counts
mspct_01 <- msmsply(mspct_1, trim_counts)
plot(mspct_01[["spct_1"]], annotations = NULL) +
  ylim(0, max(mspct_1[["spct_1"]]$counts_1))
plot(mspct_01[["spct_2"]], annotations = NULL) +
  ylim(0, max(mspct_1[["spct_1"]]$counts_1))

mspct_01 <- msmsply(mspct_01, linearize_counts)
plot(mspct_01[["spct_1"]], annotations = NULL) +
  ylim(0, max(mspct_1[["spct_1"]]$counts_1))
plot(mspct_01[["spct_2"]], annotations = NULL) +
  ylim(0, max(mspct_1[["spct_1"]]$counts_1))

mspct_filter <- msmsply(mspct_01, fshift, range = c(190,200))
mspct_filter_01 <- msmsply(mspct_filter, trim_wl, range = c(270,900))
mspct_filter_02 <- msmsply(mspct_filter_01, raw2cps)
mspct_filter_03 <- msmsply(mspct_filter_02, merge_cps)

plot(mspct_filter_03[[1]])
plot(mspct_filter_03[[2]])
plot(mspct_filter_03[[3]]) + ylim(NA, max(mspct_filter_03[[1]]$cps))

# subtract dark reference from corresponding light scans (short and long)
spct_01 <- ref_correction(mspct_filter_03$spct_1, mspct_filter_03$spct_2)
plot(spct_01)

getInstrDesc(spct_01)
getInstrSettings(spct_01)
plot(spct_01, annotations = c("color.guide", "labels", "segments"))

# just if we want to confirm the settings actualy in use in the spectrometer
current_settings <- get_oo_settings(descriptor)

end_session(w)
