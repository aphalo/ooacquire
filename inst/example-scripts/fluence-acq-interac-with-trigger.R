# Acquire irradiance data from a continuous light source triggering a camera or
# other device with a YoctoRelay USB module accessed through a local Yoctopuce
# VirtualHub or a remote YoctoPuce hub.
#
# In this use case the default configuration of the YoctoRelay needs to be
# changed to generate a switch closure after a small delay to ensure that the
# spectrometer sees the light flash during the spectral measurement.
# We send the command to the module, which returns immediately and
# switches on and then off after the programmed delay. We need to use the
# function delayedPulseTimer to program the relay module.
#
# with a maximum time in state B as we want a short switch closure
# to trigger a single flash or a single shutter trigger, and 10 ms seems to work
# fine.
# The module has two relays "Relay1" and "Relay2", we here use "Relay1". The
# module has a built-in webserver that we use to communicate without need
# of any driver or code library.
#
# Configuration used:
# logicalName: RELAY1
# stateAtPowerOn: A
# maxTimeOnStateA: 0.000 [s]
# maxTimeOnStateB: 0.010 [s]
# output: ON
# pulseTimer: 0.000 [s]
# delayedPulseTimer: none
# countdown: 0.000 [s]
#
# The serial number of the module in the code needs to match the one in use,
# and must be set as part of the URL.
#
# To use a remote hub, simply replace "localhost" by the hub's URL.
# The delay value to use will depend on the delay of the flash to
# recharge fully, which may be longer than the reported readiness.
#
# Flash used in tests: Godox AD200
# Flash trigger: Godox XPro-O
# Relay module: Yoctopuce YoctoRelay
# Spectrometer: Ocean Optics Maya 2000Pro
#
# As the spectrum of a flash may depend on power settings,
# bracketing should be done by varying the number of pulses.
#
# By default, the function used simply prompts the operator to manually trigger
# the flash. We here define a simple replacement function to use a USB relay
# module to automatically trigger a flash. This facilitates measurements,
# especially when using multiple flashes per spectrometer integration.
#
library(ooacquire)
library(yoctopuce)
# Load Python library modules
y_initialise("yocto_relay")
# Open connection to module function by name
Relay1 <- yocto_relay$YRelay$FindRelay("RELAY1")

yocto.flash.trigger <- function(n = 1L, delay = 0, duration = 0.01) {
  for (i in seq_len(length.out = n)) {
    # edit to match URL and module serial number (or check module docs. for alternatives)
    stopifnot("Delay must be > 0" = delay >= 0,
              "Duration must be > 0" = delay >= 0,
              "Only 'n = 1' supported" = n == 1L)
    for (i in seq_len(length.out = n)) {
      Relay1$delayedPulse(as.integer(delay * 1000), as.integer(duration * 1000))
    }
  }
}

# test that flash is triggered correctly
yocto.flash.trigger()
# yocto.flash.trigger(n = 2)
yocto.flash.trigger(delay = 2)
yocto.flash.trigger(delay = 0.5)
yocto.flash.trigger(duration = 1)

acq_irrad_interactive(qty.out = "fluence",
                      f.trigger.on = yocto.flash.trigger)
