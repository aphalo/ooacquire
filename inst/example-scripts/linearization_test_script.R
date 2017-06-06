# Using an spectrometer with two channels
# Tested with a Jaz spectrometer

library(r4photobiology)
library(ooacquire)
library(tidyverse)
library(polynom)

w <- start_session()

list_instruments(w)

descriptor_ch1 <- get_oo_descriptor(w, ch.index = 0L)
# descriptor_ch1$max.counts <- 50000
descriptor_ch2 <- get_oo_descriptor(w, ch.index = 1L)
# descriptor_ch2$max.counts <- 50000

# measure reflectance

descriptor <- descriptor_ch1

# We will measure with 30 different integration times, overexposing by
# about 30 % so as to have several puxels saturated at the longest integration
# time.
settings <- acq_settings(descriptor,
                         HDR.mult = c(30:1) / 30 * 1.3,
                         target.margin = 0.1,
                         tot.time.range = c(5, 15),
                         corr.elect.dark = 0L,
                         corr.sensor.nl = 0L,
                         boxcar.width = 10L)

settings <- tune_acq_settings(descriptor, settings)
# settings

raw_dose_mspct <- acq_raw_mspct(descriptor, settings, protocol = c("light", "dark"))
names(raw_dose_mspct)
plot(raw_dose_mspct[["light"]])
plot(raw_dose_mspct[["dark"]])
# We replace with NAs the clipped readings
raw_dose_mspct <- msmsply(raw_dose_mspct, trim_counts)
plot(raw_dose_mspct[["light"]])
plot(raw_dose_mspct[["dark"]])

# convert to counts per second
cps_dose_mspct <- raw2cps(raw_dose_mspct)

# if sensor response were linear, then the cps would be the same for all
# integration times. In practice this never the case, and we need to
# calculate a correction.
# Ocean Optics uses 10 pixels. Which pixels are suitable depends on the light
# source used.
all.w.lengths <- cps_dose_mspct[["light"]][["w.length"]]
# target.pixs <- 600 + 1:10 * 20
target.pixs <- 600

# print the integration times, total times, and possible saturation

getInstrSettings(cps_dose_mspct[["light"]])

col.names <- paste("pix", target.pixs, sep = "")

cps_dose_mspct[["light"]] %>%
  filter(w.length %in% all.w.lengths[target.pixs]) %>%
  select(-w.length) %>%
  t() -> cps.mat
names(cps.data) <- col.names
cps.tb <- gather(cps.data)

raw_dose_mspct[["light"]] %>%
  filter(w.length %in% all.w.lengths[target.pixs]) %>%
  select(-w.length) %>%
  t() -> counts.data
names(raw.data) <- col.names


all.w.lengths <- blue_filter.raw_mspct[[1]][["w.length"]]
target.pixs <- 600

blue_filter.raw_mspct[[1]] %>%
  filter(w.length %in% all.w.lengths[target.pixs]) %>%
  select(-w.length) %>%
  t() %>%
  as.vector() -> counts.data

blue_filter.raw_mspct[[1]] %>%
  raw2cps() %>%
  filter(w.length %in% all.w.lengths[target.pixs]) %>%
  select(-w.length) %>%
  t() %>%
  as.vector() -> cps.data

lin.poly.fit <- lm(counts.data~poly(cps.data, 1))
as.polynomial(coef(lin.poly.fit))

end_session(w)
