library(photobiology)
library(photobiologyInOut)
library(ggpp)
library(ggspectra)
library(lubridate)
library(photobiologyFilters)

theme_set(theme_bw())

# disk

pc_disk.tb <-
  read.csv(file = "data-raw/calibrations-MAYP11278/MAYPC.CSV",
           sep = ";",
           skip = 1,
           header = FALSE,
           col.names = c("w.length", "Tpc", "SD"))

pc_disk.spct <- as.filter_spct(pc_disk.tb,
                               Tfr.type = "total",
                               thickness = 2e-3,
                               attenuation.mode = "absorption",
                               Rfr.constant = NA_real_)

when_measured(pc_disk.spct) <- ymd("2023-04-24")
what_measured(pc_disk.spct) <-
  "Polycarbonate disk used to measure stray light with MAYP11278. Tfr measured after several years of use."
comment(pc_disk.spct) <-
  "Disk material polycarbonate sheet from Arla plast (Sweden), bought from Foiltek (Finland)."
how_measured(pc_disk.spct) <-
  "Measured with an Agilent 8453 array spectrophotometer using deuterium + tungsten lamp. No integrating sphere."

autoplot(pc_disk.spct)

## dome

pc_dome.tb <-
  read.csv(file = "data-raw/calibrations-MAYP11278/DOME2.CSV",
           sep = ";",
           skip = 1,
           header = FALSE,
           col.names = c("w.length", "Tpc", "SD"))

pc_dome.spct <- as.filter_spct(pc_dome.tb,
                               Tfr.type = "total",
                               thickness = 1e-3,
                               attenuation.mode = "absorption",
                               Rfr.constant = NA_real_)
pc_dome.spct <- despike(pc_dome.spct, z.threshold = 4)

when_measured(pc_dome.spct) <- ymd("2023-04-24")
what_measured(pc_dome.spct) <-
  "Polycarbonate dome used with MAYP11278 (new)."
comment(pc_dome.spct) <-
  "Polycarbonate dome used to measure stray light with MAYP11278. Tfr measured when new. Dome as used in surveilance cameras described as \"51.4x26.57mm 2 Inch Small Acrylic PC Clear Dome Glass Protective Cover Security CCTV Camera Housing Anti-Aging Transparent Shell\", bought from AliExpress
seller YiChangWang".
how_measured(pc_dome.spct) <-
  "Measured with an Agilent 8453 array spectrophotometer using deuterium + tungsten lamp. No integrating sphere."

autoplot(pc_dome.spct,
         annotations = c("+", "title:what:when:how"))

PC_filters.mspt <- normalize(filter_mspct(list(dome = pc_dome.spct, disk = pc_disk.spct)), norm = 1050)

autoplot(PC_filters.mspt)

p0 <-
  autoplot(c(PC_filters.mspt,
             normalize(filters.mspct["Foiltek_Clear_PC"], norm = 1050)))

p0.insert <- p0 +
  labs(x = "", y = "") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(370, 430))

p1 <- p0 + annotate(geom = "plot", y = 0, x = 1100, label = p0.insert, vp.width = 0.45, vp.height = 0.65)

p2 <-
  autoplot(c(PC_filters.mspt["dome"],
             normalize(filters.mspct["Foiltek_Clear_PC"], norm = 1050)))

p2.insert <- p2 +
  labs(x = "", y = "") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(370, 430))

p2 <- p2 + annotate(geom = "plot", y = 0, x = 1100, label = p2.insert, vp.width = 0.45, vp.height = 0.65)

