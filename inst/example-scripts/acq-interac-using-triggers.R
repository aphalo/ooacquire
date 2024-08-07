# Acquire irradiance data from a continuous light source triggering a camera or
# other device with a YoctoRelay USB module accessed through a local Yoctopuce
# VirtualHub or a remote YoctoPuce hub.
#
# In this simple use case the default configuration of the YoctoRelay works.
# The module has two relays "Relay1" and "Relay2", we here use "Relay1". The
# module has a built-in webserver that we use to communicate without need
# of any driver or code library.
#
# Configuration used:
# logicalName: RELAY1
# stateAtPowerOn: A
# maxTimeOnStateA: 0.000 [s]
# maxTimeOnStateB: 0.000 [s]
# output: ON
# pulseTimer: 0.000 [s]
# delayedPulseTimer: none
# countdown: 0.000 [s]
#
# The serial number of the module in the code needs to match the one in use,
# and must be set as part of the URL, if it is used. Assigning a logical name
# to the module function, as I have done for this example, makes the code
# agnostic about module serial numbers.
#
# To use a remote hub, simply replace "localhost" by the hub's URL.
#
# The delay value to use will depend on the speed of the camera to arm the
# shutter, with an electronic shutter this is in the order of tens of
# milliseconds. Pulse durations as short as 1 ms seen to work reliably.
#
# Camera used in tests: Olympus E-M1, EM-1 II, OM-1 (digital)
# Camera trigger: direct cable
# Relay module: Yoctopuce YoctoRelay
# Spectrometer: Ocean Optics Maya 2000Pro
#
# Using a "stereo" 2.5mm connected as used by Canon, Pentax and recent
# Olympus/OM cameras seems to be also the most common approach with independent
# manufacturers of wired camera remotes. The expectation is a switch closure
# so no additional components are needed except for the connector. The wiring
# is as follows: base = common (ground for audio), middle = focus "half press",
# (audio R/right/"red" channel), tip = shutter release (audio L/left/"white"
# channel). In the Yocto-Relay I connected IN1 and IN2 to the base of the jack,
# B2 to the middle of the jack, and B1 to the tip. A1 and A2 not connected.
#
# Below I define three simple functions for using a YoctoRelay USB module to
# automatically trigger a camera each time a spectrum is acquired. This makes it
# possible to take photographs nearly synchronously with the spectral data
# acquisition. Alternatively, one can record a video overlaping the time during
# which a spectrum was acquired.
#
# Perfect synchronization is not possible as the spectrometer invisibles the most
# recent spectrum measured with the current settings, which will be in most
# cases one started before the computer requested it. In addition, because of
# what looks like competition for USB access between the spectrometer and the
# virtual hub, the relay module is not always on-line immediately after a
# spectrum has been acquired, so the functions defined below have a "sleep loop"
# that waits for at most approximately 20 ms for the relay module to be back on
# line.
#
# The argument passed to delay delays the switching of the relay but not the
# acquisition of spectra, making it possible to trigger the flash or camera
# while a spectrum is being acquired by the spectrometer on a different USB
# port.
#
# Further possibilities ---------------------------------------------------
#
# A Hähnell camera and flash trigger can be set to add a delay or issue multiple
# triggers when triggered through its "aux" input. It could be used to take
# multiple images per spectrum. Modern cameras can be programed for time-lapse
# photography and/or burst modes, adding possibilities. The OM-1 camera can, for
# example, take 50 photographs per second at full resolution and nearly 240
# photograps per second at lower resolution. Depending on the settings, manual
# or auto exposure settings can be used in the camera.
#
# Some electronic flashes have a stroboscope mode allowing a rapid sequence of
# light flashes on a single trigger event.
#
# The relay module could be used to trigger multiple flashes in the R trigger is
# run concurrently on a different R session using R package 'mirai'. I am not
# sure if this approach is able to allow accurate synchronization or stable
# running for a long time.
#

library(ooacquire)
library(yoctopuce)

y_initialise("yocto_relay")

yocto.trigger.init <- function() {
  Relay <<- yocto_relay$YRelay$FindRelay("RELAY1")
}

## ON and OFF functions
# too short a time between OFF and ON may not trigger a camera
yocto.trigger.on <- function(n = NA, delay = 0.01, duration = 3600) {
  if (duration > 3600) { # 1 h
    warning("Long duration of ", duration, "S reset to 3600S.")
    duration <- 3600
  }
  count.down <- 10
  while(count.down && !Relay$isOnline()) {
    count.down <- count.down - 1L
    Sys.sleep(0.002)
  }
  if (Relay$isOnline()) {
    Relay$delayedPulse(as.integer(delay * 1000), as.integer(duration * 1000))
    message("Relay ON")
    invisible(TRUE)
  } else {
    message("Relay is off-line!")
    invisible(FALSE)
  }
}

yocto.trigger.off <- function(n = NA, delay = 0) {
  stopifnot("Only 'delay = 0' supported" = delay == 0)
  count.down <- 10
  while(count.down && !Relay$isOnline()) {
    count.down <- count.down - 1L
    Sys.sleep(0.002)
  }
  if (Relay$isOnline()) {
    Relay$set_state(Relay$STATE_A)
    message("Relay OFF")
    invisible(TRUE)
  } else {
    message("Relay is off-line!")
    invisible(FALSE)
  }
}

## Delayed pulse function ensures pulse is long enough
# shutter release is reliable in fast succession
yocto.trigger.pulse <- function(n = NA, delay = 0.01, duration = 0.01) {
  stopifnot("Delay must be >= 0" = delay >= 0,
            "Duration must be >= 0" = duration >= 0)
  if (duration > 3600) { # 1 h
    warning("Long duration of ", duration, "S reset to 3600S.")
    duration <- 3600
  }
  count.down <- 50
  while(count.down && !Relay$isOnline()) {
    count.down <- count.down - 1L
    Sys.sleep(0.01)
  }
  if (Relay$isOnline()) {
    Relay$delayedPulse(as.integer(delay * 1000), as.integer(duration * 1000))
    message("Relay pulsed ON and OFF")
    invisible(TRUE)
  } else {
    message("Relay is off-line!")
    invisible(FALSE)
  }
}

# test that camera shutter is triggered correctly
yocto.trigger.init()
yocto.trigger.on()
yocto.trigger.on(delay = 2)
yocto.trigger.off()
yocto.trigger.pulse()
yocto.trigger.pulse(duration = 0.5)
yocto.trigger.pulse(delay = 0.5)

acq_irrad_interactive(folder.name = "./inst-not/yoct-relay-tests")

acq_irrad_interactive(folder.name = "./inst-not/yoct-relay-tests",
                      qty.out = "fluence")

acq_irrad_interactive(folder.name = "./inst-not/yoct-relay-tests",
                      qty.out = "irrad",
                      f.trigger.init = yocto.trigger.init,
                      f.trigger.on = yocto.trigger.on,
                      f.trigger.off = yocto.trigger.off,
                      triggers.enabled = "light")

acq_irrad_interactive(folder.name = "./inst-not/yoct-relay-tests",
                      qty.out = "irrad",
                      f.trigger.init = yocto.trigger.init,
                      f.trigger.on = yocto.trigger.pulse,
                      triggers.enabled = "light")

# half minute of measurements every 2 seconds
acq_irrad_interactive(folder.name = "./inst-not/yoct-relay-tests",
                      interface.mode = "series",
                      tot.time.range = 1,
                      target.margin = .1,
                      qty.out = "irrad",
                      HDR.mult = 1L,
                      seq.settings = list(start.boundary = "second",
                                          initial.delay = 0,
                                          step.delay = 2,
                                          num.steps = 15),
                      entrance.optics = "dome",
                      f.trigger.init = yocto.trigger.init,
                      f.trigger.on = yocto.trigger.pulse)


acq_irrad_interactive(folder.name = "./inst-not/yoct-relay-tests",
                      qty.out = "fluence",
                      f.trigger.on = yocto.trigger.pulse)
