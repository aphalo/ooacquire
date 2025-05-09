# Using an spectrometer with a single channel
# Tested with a Maya 2000 Pro spectrometer

library(ooacquire)

# print warnings when they are triggered
options(warn = 1)

# use default settings
acq_fraction_interactive()

# # no bracketing
# acq_fraction_interactive(HDR.mult = 1)
#
# # no bracketing
# # adjust integration time and number of scans so that total time is 10 seconds
# acq_fraction_interactive(HDR.mult = 1, tot.time.range = 10)
#
# # no bracketing
# # adjust number of scans so that total time remins between 0.1 and 10 seconds
# acq_fraction_interactive(HDR.mult = 1, tot.time.range = c(0.1, 10))
#
# # bracketing, long integration 10 times longer than short one
# # (unless it goes over what the spectrometer accepts)
# acq_fraction_interactive(HDR.mult = c(1, 10))
#
# # bracketing, long integration 10 tines longer than short one
# # (unless it goes over what the spectrometer accepts)
# # adjust integration time and number of scans so that total time is 10 seconds
# # for both short and long integrations
# acq_fraction_interactive(HDR.mult = c(1, 10), tot.time.range = 10)
#
# # bracketing, long integration 10 tines longer than short one
# # (unless it goes over what the spectrometer accepts)
# # adjust number of scans so that total time remins between 0.1 and 10 seconds
# # total time for short and long integrations may be different
# acq_fraction_interactive(HDR.mult = c(1, 10), tot.time.range = c(0.1, 10))
#
# # bracketing with three different integration times, total time 30 seconds
# # (may be useful with LEDs or other situations when stray light is unusually
# # low)
# acq_fraction_interactive(HDR.mult = c(1, 7, 50), tot.time.range = 30)
