# create an object with the parameters for Lasse Ylianttila's method for Maya
maya_ylianttila <- list(
  method = "original",
  stray.light.wl = c(218.5, 228.5),
  flt.dark.wl = c(193, 209.5),
  flt.ref.wl = c(360, 379.5),
  worker_fun = ooacquire::maya_tail_correction,
  trim = 0
)

load(file = "./data-raw/maya-descriptor/MAYP11278.Rda")

maya_descriptor <- descriptor
# maya_descriptor$inst.calib$irrad.mult <-
#   ifelse(maya_descriptor$wavelengths < 250 |
#            maya_descriptor$wavelengths >= 900,
#          NA_real_,
#          maya_descriptor$wavelengths)
rm(descriptor)

save(maya_ylianttila, maya_descriptor, file = "data/maya-ylianttila.rda")

