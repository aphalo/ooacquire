.onAttach <- function(libname, pkgname) {
  if (getOption("ooacquire.offline", TRUE) || getOption("rOmniDriver.offline", TRUE) ) {
    packageStartupMessage("Package 'ooacquire' is Off-line: package 'rOmniDriver' needs to be installed and enabled to access spectrometers.")
  }
}

.onLoad <- function(libname, pkgname) {
  # on-line/off-line mode depends on availability of package rOmniDriver
  options(ooacquire.offline = !requireNamespace("rOmniDriver", quietly = TRUE))
}
