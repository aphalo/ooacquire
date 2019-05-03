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
#
# Flash used in tests: Godox AD200
# Flash trigger: Godox XPro-O
# Relay module: Yoctopuce YoctoRelay
# Spectrometer: Ocean Optics Maya 2000Pro
#
# As the spectrum of a flash may depend on power settings,
# bracketing should be done by varying the number of pulses.
#

library(r4photobiology)
library(ooacquire)
library(httr)

options(warn = 1) # no delay when issuing warnings

# By default, the function used simply prompts the operator to manually trigger
# the flash. We here define a simple function to use a USB relay module to
# automatically trigger a flash. This facilitates measurements, especially
# when using multiple flashes per spectrometer integration.
#
# The relay module is programmed beforehand to function as a momentary switch
# with a pulse duration of 0.01 seconds. The flash is set to manual mode and
# the XPro flash trigger set to SYNC = IN.

### We need to use here the delayed pulse command so that GET returns before
## the flash is triggered and we can so start the integration in the
## spectrtometer before the USB relay module triggers the flashes.

yocto.flash.trigger <- function(n = 1L, delay = 0.2) {
  stopifnot(delay >= 0)
  stopifnot(n >= 0L)
  for (i in seq_len(length.out = n)) {
#    Sys.sleep(delay)
    page <- GET("http://localhost:4444/bySerial/RELAYLO1-B263A/api?scr=&ctx=relay1&state=0")
    # If connection is refused a curl error is triggered and execution stopped.
    # If the module is not on-line an error is returned, that we can test for and handle.
    if (http_error(page)) {
      break()
    }
  }
  if (http_error(page)) {
    warning("Failed to trigger the flash: '", http_status(page)$message, "'", call. = FALSE)
    return(FALSE)
  } else {
    message("Triggered ", n, ifelse(n == 1, " flash", " flashes"), ", with a delay of ", delay, " seconds.")
    return(TRUE)
  }
}

# test that flash is triggered correctly
yocto.flash.trigger()
yocto.flash.trigger(n = 2)
yocto.flash.trigger(n = 10)
yocto.flash.trigger(n = 2, delay = 0.5)

acq_fraction_pulsed_interactive(f.trigger.pulses = yocto.flash.trigger)
acq_fraction_pulsed_interactive()
