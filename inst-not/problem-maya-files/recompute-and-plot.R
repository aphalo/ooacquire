library(ooacquire)

files_s_irrad_update("inst-not/problem-maya-files/",
                     spct.names = list(dark = "dark", filter = "filter",
                                    light = paste("light", 1L:5L, sep = ".")))

files_s_irrad_update("inst-not/problem-maya-files/",
                     spct.names = list(dark = "dark", filter = "filter",
                                       light = paste("light", 1L:5L, sep = ".")),
                     save.files = TRUE)

files_s_irrad_update("inst-not/problem-maya-files/",
                     spct.names = list(dark = "dark", filter = "filter",
                                       light = paste("light", 1L:5L, sep = ".")),
                     correction.method = MAYP11278_simple.mthd)

load("inst-not/problem-maya-files/hemis_field_A021.spct.bak.rda")

s_irrad_corrected(hemis_field_A021.raw_mspct,
                     spct.names = list(dark = "dark", filter = "filter",
                                       light = paste("light", 1L:5L, sep = ".")),
                     correction.method = MAYP11278_simple.mthd) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_sun.mthd) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_sun.mthd,
                  filter.nir.adjust = TRUE) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_sun.mthd,
                  return.cps = TRUE) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark", filter = "filter",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_short_flt_ref.mthd) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_sun.mthd) |> autoplot()

s_irrad_corrected(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_simple.mthd) |>
  smooth_spct() |> autoplot(facets = 2)

s_irrad_update(hemis_field_A021.raw_mspct,
                  spct.names = list(dark = "dark",
                                    light = paste("light", 1L:5L, sep = ".")),
                  correction.method = MAYP11278_simple.mthd) |>
  smooth_spct() |> autoplot(facets = 2)

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
