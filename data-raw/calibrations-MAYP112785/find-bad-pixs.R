library(photobiology)
library(ggspectra)
library(ooacquire)

load("data-raw/calibrations-MAYP112785/cosine_2026-07-14_badpixs.spct.Rda")

# very hot pixels in dark raw spct
autoplot(cosine_1350_001.raw_mspct[["dark"]]) +
  stat_spikes(height.threshold = 100) +
  ylim(0, 20000)

bad.pix.ids_4 <-
  which(find_spikes(cosine_1350_001.raw_mspct[["dark"]][["counts_4"]],
                    height.threshold = 100))

bad.pix.ids_3 <-
  which(find_spikes(cosine_1350_001.raw_mspct[["dark"]][["counts_3"]],
                    height.threshold = 100))

bad.pix.ids_2 <-
  which(find_spikes(cosine_1350_001.raw_mspct[["dark"]][["counts_2"]],
                    height.threshold = 100))

bad.pix.ids_1 <-
  which(find_spikes(cosine_1350_001.raw_mspct[["dark"]][["counts_1"]],
                    height.threshold = 100))

# hot pixel in source spct
autoplot(cosine_1350_001.spct) +
  stat_spikes(height.threshold = 50, spike.direction = "up", geom = "text")

bad.pix.ids_source <-
  which(min(abs(cosine_1350_001.raw_mspct[["filter"]][["w.length"]] - 409)) ==
          abs(cosine_1350_001.raw_mspct[["filter"]][["w.length"]] - 409))

bad.pix.ids_source <-
  which(cosine_1350_001.raw_mspct[["filter"]][["w.length"]] == 409.02)

bad.pix.ids <- sort(union(bad.pix.ids_4, bad.pix.ids_source))
bad.pix.wls <- cosine_1350_001.raw_mspct[["filter"]][["w.length"]][bad.pix.ids]
print(bad.pix.ids)
print(bad.pix.wls)

# check after updating calibration

y <-
  s_irrad_corrected(x = cosine_1350_001.raw_mspct,
                    spct.names = list(light = paste("light", "1":"3", sep = "."),
                                      filter = "filter", dark = "dark"))

z <-
  s_irrad_update(x = cosine_1350_001.raw_mspct,
                 y = cosine_1350_001.spct,
                 spct.names = list(light = paste("light", "1":"3", sep = "."),
                                   filter = "filter",
                                   dark = "dark"))

autoplot(z)
autoplot(y)
