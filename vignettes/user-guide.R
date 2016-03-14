## ---- eval=FALSE---------------------------------------------------------
#  acq_transmittance <- function(oo_escriptor, integ.time = 1e-3, HDR.mult = 1) {
#    settings <- acq_settings(oo_descriptor, integ.time = integ.time, HDR.mult = 1)
#    settings <- tune_settings(oo_descriptor, settings)
#    mspct <- acq_raw_mspct(oo_descriptor, protocol = c("clear", "black", "sample"))
#    mspct <- msmsply(mspct, trim_counts)
#    mspct <- msmsply(mspct, skip_bad_pixs)
#    mspct <- msmsply(mspct, linearize_counts)
#    mspct <- msmsply(mspct, raw2cps)
#    spct <- as.generic_spct((mspct[["sample"]] - mspct[["black"]]) /
#      (mspct[["white"]] - mspct[["black"]]))
#    names(spct)[2] <- "Tfr"
#    setFilterSpct(spct, )
#  }

