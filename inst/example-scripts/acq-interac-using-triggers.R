# Acquire spectral irradiance or fluence from a continuous light source
# triggering a camera, a flash or other device with a YoctoRelay USB module
# accessed through a local Yoctopuce VirtualHub or a remote YoctoPuce hub.
#
# In these simple use case a simple (default?) configuration of the YoctoRelay
# works. The module has two relays with logical names "Relay1" and "Relay2"
# assigned, and we use "Relay1".
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
# The serial number of the module plus function name, can be used instead of the
# logical name, must be included as part of the URL. Assigning a logical name to
# the module function, as I have done for this example, makes the code agnostic
# about module serial numbers.
#
# To use a remote hub, simply replace "localhost" by the hub's URL.
#
# The delay value to use will depend on the speed of the camera to arm the
# shutter, with an electronic released shutter this is in the order of tens of
# milliseconds. Delays and pulse durations as short as 1 ms seen to work
# reliably.
#
# Cameras used in tests: Olympus E-M1, EM-1 II, OM-1 (digital)
# Camera trigger: direct cable
# Relay module: Yoctopuce YoctoRelay
# Spectrometer: Ocean Optics Maya 2000Pro
#
# Using a "stereo" 2.5mm connected as used by Canon, Pentax and recent
# Olympus/OM cameras seems to be also the most common approach with independent
# manufacturers of wired camera remotes. The expectation is a switch closure so
# no additional components are needed except for the connector. The wiring of
# the jack is as follows: base = common (ground for audio), middle = focus "half
# press", (audio R/right/"red" channel), tip = shutter release (audio
# L/left/"white" channel). In the Yocto-Relay I connected IN1 and IN2 to the
# base of the jack, B2 to the middle of the jack, and B1 to the tip. A1 and A2
# not connected.
#
# Below I define a simple pulse function for using a YoctoRelay USB module to
# automatically trigger a camera each time a spectrum is acquired. This makes it
# possible to take photographs nearly synchronously with the spectral data
# acquisition.
#
# Perfect synchronization is not possible as the spectrometer returns the most
# recent spectrum measured with the current settings, which will be in most
# cases am acquisition started before the R function requested it. In addition,
# because of what looks like competition for USB access between the spectrometer
# and the virtual hub, the relay module is not always on-line immediately after
# a spectrum has been acquired, so the functions defined below have a "sleep
# loop" that waits for at most approximately 20 ms for the relay module to be
# back on line.
#
# The argument passed to parameter delay delays the switching of the relay but
# not the acquisition of spectra, making it possible to trigger the flash or
# camera while a spectrum is being acquired by the spectrometer on a different
# USB port.
#
library(ooacquire)
library(yoctopuce)

init_yoctopuce("yocto_relay")

## ON and OFF functions
# too short a time between OFF and ON may not trigger a camera
yocto.trigger.on <- function(n = NA, delay = 0.01, duration = 3600) {
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
    message("Relay ON")
    invisible(TRUE)
  } else {
    message("Relay is off-line!")
    invisible(FALSE)
  }
}

yocto.trigger.off <- function(n = NA, delay = 0) {
  stopifnot("Only 'delay = 0' supported" = delay == 0)
  count.down <- 50
  while(count.down && !Relay$isOnline()) {
    count.down <- count.down - 1L
    Sys.sleep(0.01)
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
yocto.trigger.pulse <- function(n = NA, delay = 0.001, duration = 0.001) {
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

yocto.trigger.reset <- function() {
  Relay <<- yocto_relay$YRelay$FindRelay("RELAY1")
  # force relaay to off state
  yocto.trigger.off()
}

# test that camera shutter is triggered correctly
yocto.trigger.reset()
yocto.trigger.on()
yocto.trigger.on(delay = 2)
yocto.trigger.off()
yocto.trigger.pulse()
yocto.trigger.pulse(duration = 0.5)
yocto.trigger.pulse(delay = 0.5)

# acq_irrad_interactive(folder.name = "./inst-not/yocto-relay-tests")
#
# acq_irrad_interactive(folder.name = "./inst-not/yocto-relay-tests",
#                       qty.out = "fluence")

# Irradiance --------------------------------------------------------------
# This can be used to trigger a different measurement each time a spectrum is
# acquired, such as taking a photograph

acq_irrad_interactive(folder.name = "./inst-not/yocto-relay-tests",
                      qty.out = "irrad",
                      f.trigger.init = yocto.trigger.reset,
                      f.trigger.on = yocto.trigger.on,
                      f.trigger.off = yocto.trigger.off,
                      triggers.enabled = "light")

acq_irrad_interactive(folder.name = "./inst-not/yoct-relay-tests",
                      qty.out = "irrad",
                      f.trigger.init = yocto.trigger.reset,
                      f.trigger.on = yocto.trigger.pulse,
                      triggers.enabled = "light")

# half minute of measurements every 2 seconds
acq_irrad_interactive(folder.name = "./inst-not/yocto-relay-tests",
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


# Fluence -----------------------------------------------------------------
# This can be used to trigger a flash each time a spectrum is acquired

acq_irrad_interactive(folder.name = "./inst-not/yocto-relay-tests",
                      qty.out = "fluence",
                      f.trigger.init = yocto.trigger.init,
                      f.trigger.on = yocto.trigger.pulse)

# Further possibilities ---------------------------------------------------
#
# A camera and flash trigger, such as Hähnel CAPTUR Module Pro, can be set to add a
# delay or issue multiple triggers when triggered through its "aux" input. It
# could be used to take multiple images per spectrum. Modern cameras can be
# programmed for time-lapse photography and/or burst modes, adding
# possibilities. The OM-1 camera can, for example, take 50 photographs per
# second at full resolution and nearly 240 photographs per second at lower
# resolution. Depending on the settings, manual or auto exposure settings can be
# used in the camera.
#
# Some electronic flashes have a stroboscope mode allowing a rapid sequence of
# light flashes on a single trigger event.
#
# The relay module could be used to trigger multiple flashes if the R trigger
# function runs concurrently in a different R session using R package 'mirai'. I
# am not sure if this approach is able to allow accurate synchronization or
# stable running for a long time.
#

