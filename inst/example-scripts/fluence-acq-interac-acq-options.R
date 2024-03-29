# Using a spectrometer with a single channel
# Tested with a Maya 2000 Pro spectrometer and a USB2000 spectrometer

library(ooacquire)

photon_as_default()

# use default settings choosing instrument automatically
acq_irrad_interactive(qty.out = "fluence")

# use default settings for old Maya
# acq_irrad_interactive(correction.method = ooacquire::MAYP11278_ylianttila.mthd,
#                       descriptors = ooacquire::MAYP11278_descriptors)

# use default settings for new Maya
# acq_irrad_interactive(correction.method = ooacquire::MAYP112785_ylianttila.mthd,
#                       descriptors = ooacquire::MAYP112785_descriptors)

# # change safety margin or "head space" when automatically tuning the
# # integration time, increasing its value is useful when irradiance changes fast
# acq_irrad_interactive(target.margin = 0.2)
#
# # no bracketing
# acq_irrad_interactive(HDR.mult = 1)
#
# # no bracketing and "overexposing" with respect to setting
# # could be occasionally useful when automatically tuning integration time
# acq_irrad_interactive(HDR.mult = 2)
#
# # no bracketing
# # adjust integration time and number of scans so that total time is 20 seconds
# acq_irrad_interactive(HDR.mult = 1, tot.time.range = 20)
#
# # no bracketing
# # adjust number of scans so that total time remains between 0.1 and 10 seconds
# acq_irrad_interactive(HDR.mult = 1, tot.time.range = c(0.1, 10))
#
# # bracketing, long integration 10 times longer than short one
# # (unless it goes over what the spectrometer accepts)
# acq_irrad_interactive(HDR.mult = c(1, 10))
#
# # bracketing, long integration 10 tines longer than short one
# # (unless it goes over what the spectrometer accepts)
# # adjust integration time and number of scans so that total time is 10 seconds
# # for both short and long integrations
# acq_irrad_interactive(HDR.mult = c(1, 10), tot.time.range = 10)
#
# # bracketing, long integration 10 tines longer than short one
# # (unless it goes over what the spectrometer accepts)
# # adjust number of scans so that total time remains between 0.1 and 10 seconds
# # total time for short and long integrations may be different
# acq_irrad_interactive(HDR.mult = c(1, 10), tot.time.range = c(0.1, 10))
#
# # bracketing with three different integration times, total time 30 seconds
# # (may be useful with LEDs or other situations when stray light is unusually
# # low)
# acq_irrad_interactive(HDR.mult = c(1, 7, 50), tot.time.range = 30)
#
