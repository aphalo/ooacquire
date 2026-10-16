library(ooacquire)

photon_as_default()

# RESET all files!

reset_files <- function() {
  file.remove(list.files("inst-not/problem-maya-files/",
                         "spct.rda$|spct.bak.rda", full.names = TRUE))

  file.copy(from = list.files("inst-not/problem-maya-files/original-files",
                              "spct.rda$", full.names = TRUE),
            to = "inst-not/problem-maya-files/")
}

# update files

reset_files()

files_s_irrad_update("inst-not/problem-maya-files/")

files_s_irrad_update("inst-not/problem-maya-files/", save.files = TRUE)

# same as above

reset_files()

files_s_irrad_update("inst-not/problem-maya-files/",
                     spct.names = list(dark = "dark", filter = "filter",
                                    light = paste("light", 1L:5L, sep = ".")))

files_s_irrad_update("inst-not/problem-maya-files/",
                     spct.names = list(dark = "dark", filter = "filter",
                                       light = paste("light", 1L:5L, sep = ".")),
                     save.files = TRUE)
#

reset_files()

files_s_irrad_update("inst-not/problem-maya-files/",
                     correction.method = MAYP11278_simple.mthd)

reset_files()

files_s_irrad_update("inst-not/problem-maya-files/",
                     correction.method = MAYP11278_none.mthd)

#

load("inst-not/problem-maya-files/original-files/hemis_field_A021.spct.rda")

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  correction.method = MAYP11278_none.mthd) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = "light.1"),
                  correction.method = MAYP11278_none.mthd) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark",
                                    light = "light.1"),
                  correction.method = MAYP11278_none.mthd) |>
  smooth_spct() |> autoplot(facets = 2)

s_irrad_corrected(hemis_field_A021.raw_mspct,
                     spct.names = list(dark = "dark", filter = "filter",
                                       light = paste("light", 1L:5L, sep = ".")),
                     correction.method = MAYP11278_simple.mthd) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = "light.1"),
                  correction.method = MAYP11278_simple.mthd) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_sun.mthd) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = "light.1"),
                  correction.method = MAYP11278_sun.mthd) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_sun.mthd,
                  filter.nir.adjust = TRUE) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = "light.1"),
                  correction.method = MAYP11278_sun.mthd,
                  filter.nir.adjust = TRUE) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_short_flt_ref.mthd) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = "light.1"),
                  correction.method = MAYP11278_short_flt_ref.mthd) |> autoplot()

## no filter

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_sun.mthd) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark",
                                    light = "light.1"),
                  correction.method = MAYP11278_sun.mthd) |> autoplot()

## CPS

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_sun.mthd,
                  return.cps = TRUE) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_simple.mthd,
                  return.cps = TRUE) |> autoplot(range = c(250, 350))

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_simple.mthd,
                  return.cps = TRUE) |> autoplot(range = c(250, 350))

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = "light.1"),
                  correction.method = MAYP11278_simple.mthd,
                  return.cps = TRUE) |> autoplot(range = c(250, 350))

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(light = "light.1"),
                  correction.method = MAYP11278_simple.mthd,
                  return.cps = TRUE) |> autoplot(range = c(188, 320))

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(light = "filter"),
                  correction.method = MAYP11278_simple.mthd,
                  return.cps = TRUE) |> autoplot(range = c(188, 320))

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(light = "dark"),
                  correction.method = MAYP11278_simple.mthd,
                  return.cps = TRUE) |> autoplot(range = c(188, 320))

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(light = "dark"),
                  correction.method = MAYP11278_simple.mthd,
                  return.cps = TRUE) |> autoplot(range = c(210, 320))

hemis_field_A021.raw_mspct$dark  |> autoplot(range = c(190, 320), ylim = c(NA, 10000))
