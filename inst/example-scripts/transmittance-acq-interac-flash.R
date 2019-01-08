# Acquire transmittance data using a Xenon flash as light source
# triggering the flash with a YoctoRelay USB module accessed
# through a Yoctopuce VirtualHub.
#
# The serial of the module needs to match the one in use.
#
# To use a remote hub, simply replace "localhost" by the hub's URL
#
# The delay value to use will depend on the delay of the flash to
# recharge fully, which may be longer than the reported readiness.
#

library(r4photobiology)
library(ooacquire)
library(httr)

options(warn = 1) # no delay when issuing warnings

yocto.flash.trigger <- function(n = 1L, delay = 0.5) {
  stopifnot(delay >= 0)
  stopifnot(n >= 0L)
  for (i in seq_len(length.out = n)) {
    Sys.sleep(delay)
    page <- GET("http://localhost:4444/bySerial/RELAYLO1-B263A/api?scr=&ctx=relay1&state=0")
  }
  message("Triggered ", n, ifelse(n == 1, " flash", " flashes"), ", with a delay of ", delay, " seconds.")
  return(TRUE)
}

# test function
yocto.flash.trigger()
yocto.flash.trigger(n = 2)
yocto.flash.trigger(n = 2, delay = 0)

acq_fraction_pulsed_interactive(f.trigger.pulses = yocto.flash.trigger)
