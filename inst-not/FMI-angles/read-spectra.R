library(ooacquire)
library(readr)
library(tibble)
library(dplyr)
library(ggspectra)

# For the time being we need to override one function from my package
# 'photobiology' because of the missing pixels
# cps2irrad <- function(x.sample, pre.fun = NULL, ...) {
#   stopifnot(is.cps_spct(x.sample) &&
#               !is.null(getInstrDesc(x.sample)) &&
#               !is.null(getInstrSettings(x.sample)))
#   descriptor <- getInstrDesc(x.sample)
#   irrad.mult <- descriptor[["inst.calib"]][["irrad.mult"]]
#   # code added here to handle spectra missing the longest wavelengths
#   if (nrow(x.sample) < length(irrad.mult)) {
#     warning("Assuming ", length(irrad.mult) - nrow(x.sample),
#             " missing pixels at long end of spectrum!")
#     irrad.mult <- irrad.mult[1:nrow(x.sample)]
#   }
#   #
#   if (!is.null(pre.fun)) {
#     x.sample <- pre.fun(x.sample, ...)
#   }
#   cps.col.sample <- grep("^cps", names(x.sample), value = TRUE)
#   stopifnot(length(cps.col.sample) == 1)
#   z <- as.generic_spct(x.sample)
#   z[[cps.col.sample]] <- NULL
#   z[["s.e.irrad"]] <- x.sample[[cps.col.sample]] * irrad.mult
#   setSourceSpct(z, time.unit = getTimeUnit(x.sample))
#   z <- copy_attributes(x.sample, z)
#   if (length(descriptor[["inst.calib"]][["wl.range"]]) == 2) {
#     z <- clip_wl(z, descriptor[["inst.calib"]][["wl.range"]])
#   }
#   z
# }

oldwd <- setwd("~/RPackages/ooacquire/inst-not/FMI-angles")
on.exit(setwd(oldwd), add = TRUE)

data("calibs-FLMS00440")
folder.path <- "."
files <- list.files(folder.path, pattern = "*csv", full.names = TRUE)
descriptor <- ooacquire::FLMS00440_descriptors[[1]]
geocode <- na_geocode()

settings <- list()
# set NA (not available) as placeholder
settings[["integ.time"]] <- NA_real_
settings[["num.scans"]] <- NA_integer_
settings[["tot.time"]] <- NA_real_
settings[["rel.signal"]] <- NA_real_

# define function that reads one file with raw counts data
# returning a list with a collection of spectra and a tibble with
# some metadata to make later code simpler

read_spectra <- function(path, instr.desc, instr.settings) {
  file.name <- basename(path)
  data.locale <- locale(tz = "UTC")
  col.types <- cols(.default = col_double(),
                    Date = col_datetime(),
                    Time = col_time(),
                    Is_saturated = col_logical(),
                    Is_valid = col_logical(),
                    Is_timeout = col_logical(),
                    Dark_current_value = col_logical(),
                    Scans_to_avg = col_integer(),
                    Boxcar_width = col_integer())
  x <- read_delim(path,
                  skip = 21,
                  delim = ";",
                  col_types = col.types,
                  locale = data.locale,
                  progress = FALSE)
  header.names <- names(x)[1:12]
  wl <- as.numeric(names(x)[-(1:12)])
  date.times <- x[["Date"]] + x[["Time"]]
  # is force_tz doing anything??
  integr.time <- x[["Integration_time_(s)"]]
  scans <- x[["Scans_to_avg"]]
  is.valid <- x[["Is_valid"]]
  is.saturated <- x[["Is_saturated"]]
  max.sat.pc <- x[["Max_saturation_(%)"]]
  dark.ref <- x[["Dark_current_value"]]
  Zenith <- x[["Zenith"]]
  Azimuth <- x[["Azimuth"]]

  m <- t(as.matrix(x[ , -(1:12)]))
  dimnames(m) <- NULL
  tb <- as_tibble(m, .name_repair = "minimal")

  stopifnot(nrow(tb) == length(wl))
  stopifnot(ncol(tb) == length(date.times))
  stopifnot(ncol(tb) == length(integr.time))
  stopifnot(ncol(tb) == length(scans))
  stopifnot(ncol(tb) == length(is.valid))
  stopifnot(ncol(tb) == length(max.sat.pc))
  stopifnot(ncol(tb) == length(dark.ref))
  spectra <- raw_mspct()
  spectra.tb <- tibble()
  for (col in 1:ncol(tb)) {
    spct.name <- paste("no. ", col, sep = "")
    instr.settings[["spectrometer.sn"]] <- "FLMS00440"
    instr.settings[["integ.time"]] <- integr.time[col] * 1e6 # microseconds
    instr.settings[["num.scans"]] <- scans[col]
    instr.settings[["tot.time"]] <- integr.time[col] * scans[col] * 1e6
    instr.settings[["rel.signal"]] <- max.sat.pc[col] * 1e-2
    instr.desc[["time"]] <- date.times[col]
    tmp <- raw_spct(w.length = wl,
                    counts = tb[[col]],
                    instr.desc = instr.desc,
                    instr.settings = instr.settings)
    tmp <- setWhereMeasured(tmp, geocode)
    tmp <- setWhenMeasured(tmp, date.times[col])
    tmp <- setWhatMeasured(tmp, file.name)
    if (is.valid[col] && !is.saturated[col]) {
      if (dark.ref[col] || Azimuth[col] == -1L) {
        flag <- "dark"
      } else {
        flag <- "light"
      }
    } else {
      message("Invalid or saturated spectrum at ", date.times[col], "; marking it.")
      comment(tmp) <- paste("BAD SPECTRUM\n***\n",
                            comment(tmp), sep = "")
      flag <- "bad"
    }
    spectra[[spct.name]] <- tmp
    spectra.tb <- bind_rows(spectra.tb,
                           tibble(name = spct.name,
                                  rel.signal = max.sat.pc[col] * 1e-2,
                                  integ.time = integr.time[col] * 1e6,
                                  z = Zenith[col],
                                  a = Azimuth[col],
                                  flag = flag))
  }
  # after the loop ends add the collection of spectra as an additional column
  spectra.tb$spectra <- spectra
  spectra.tb
}

file.path <- files[1]
spectra.tb <- read_spectra(path = file.path,
                           instr.desc = descriptor,
                           instr.settings = settings)

nrow(spectra.tb) # number of spectra

# number of dark spectra
spectra.tb %>%
  filter(flag == "dark") %>% nrow()

# number of "light" spectra with enough signal
# you may want to change 2/3 to something else
spectra.tb %>%
  filter(flag != "dark" & rel.signal > 2/3) %>% nrow()

# group by integration time to see which integration times are used
# in the spectra we kept
spectra.tb %>%
  filter(flag != "dark") %>%
  group_by(integ.time) %>%
  summarize(n = n()) -> integ.time.tb
integ.time.tb

# keep dark spectra only integration time matches light spectra kept
spectra.tb %>%
  filter(integ.time %in% integ.time.tb$integ.time) -> spectra.tb

# see how many spectra we have for each z value
spectra.tb %>%
  group_by(integ.time, z, flag) %>%
  summarize(n = n())

# split the spectra by integration time, for convenience
# (Would not be needed if using a loop)
spectra.tb %>%
  filter((z == 0 | z == -1) & integ.time == 300000) -> spectra_00deg.tb

spectra.tb %>%
  filter((z == 30 | z == -1) & integ.time == 300000) -> spectra_30deg.tb

spectra.tb %>%
  filter((z == 60 | z == -1) & integ.time == 600000) -> spectra_60deg.tb

# process 00 degrees replicate spectra, individually to check if spectral irradiance looks o.k.
spectra_00deg <- spectra_00deg.tb$spectra

# first replicate
spectra_00deg_a <- spectra_00deg[c(1,2)]
names(spectra_00deg_a) <- c("dark", "light")

irrad_00deg_a_spct <-
  s_irrad_corrected(spectra_00deg_a,
                    correction.method = FLMS00440_none.mthd,
                    missing.pixs = c(2047, 2048))

autoplot(irrad_00deg_a_spct)

# second replicate
spectra_00deg_b <- spectra_00deg[c(1,3)]
names(spectra_00deg_b) <- c("dark", "light")

irrad_00deg_b_spct <-
  s_irrad_corrected(spectra_00deg_b,
                    correction.method = FLMS00440_none.mthd,
                    missing.pixs = c(2047, 2048))

autoplot(irrad_00deg_b_spct)

# third replicate
spectra_00deg_c <- spectra_00deg[c(1,4)]
names(spectra_00deg_c) <- c("dark", "light")

irrad_00deg_c_spct <-
  s_irrad_corrected(spectra_00deg_c,
                    correction.method = FLMS00440_none.mthd,
                    missing.pixs = c(2047, 2048))

autoplot(irrad_00deg_c_spct)

# second dark measurement as a "light measurement"
spectra_00deg_d <- spectra_00deg[c(1,5)]
names(spectra_00deg_d) <- c("dark", "light")

irrad_00deg_d_spct <-
  s_irrad_corrected(spectra_00deg_d,
                    correction.method = FLMS00440_none.mthd,
                    missing.pixs = c(2047, 2048))

# second dark measurement as a "light measurement", y scale limits other spectra
autoplot(irrad_00deg_d_spct)
ggplot(irrad_00deg_d_spct) +
  geom_line() +
  expand_limits(y = 0.06)

# some summaries
z00.source_mspct <- source_mspct(list(z00a = irrad_00deg_a_spct,
                                      z00b = irrad_00deg_b_spct,
                                      z00c = irrad_00deg_c_spct,
                                      z00dark = irrad_00deg_d_spct))

e_irrad(z00.source_mspct) # W m-2 whole spectrum

e_irrad(z00.source_mspct, w.band = split_bands(c(400, 500, 600, 700, 800, 900)))
