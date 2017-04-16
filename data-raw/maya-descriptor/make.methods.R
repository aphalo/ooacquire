# create an object with the parameters for Lasse Ylianttila's method for Maya
maya_ylianttila <- list(
  stray.light.method = "original",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(193, 209.5),
  flt.ref.wl = c(360, 379.5),
  worker.fun = ooacquire::MAYP11278_tail_correction,
  trim = 0
)

maya_simple <- list(
  stray.light.method = "simple",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(193, 209.5),
  flt.ref.wl = c(360, 379.5),
  worker.fun = ooacquire::MAYP11278_tail_correction,
  trim = 0
)

maya_sun <- list(
  stray.light.method = "sun",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(193, 209.5),
  flt.ref.wl = c(360, 379.5),
  worker.fun = ooacquire::MAYP11278_tail_correction,
  trim = 0
)

load(file = "./data-raw/maya-descriptor/MAYP11278.Rda")

maya_descriptor <- descriptor
rm(descriptor)
maya_descriptor[["inst.calib"]][["wl.range"]] <- c(251, 899)

save(maya_ylianttila, maya_simple, maya_sun, maya_descriptor, file = "data/maya-ylianttila.rda")

