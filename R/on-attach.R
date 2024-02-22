.onAttach <- function(libname, pkgname) {
  if (getOption("ooacquire.offline", TRUE) || getOption("rOmniDriver.offline", TRUE) ) {
    packageStartupMessage("Package 'ooacquire' is Off-line: package 'rOmniDriver'",
                          "needs to be installed and enabled to access spectrometers.")
  }
  # 'colorSpec' has 'spacesXYZ' in suggests since version 1.5-0
  if (!requireNamespace("spacesXYZ", quietly = TRUE)) {
    packageStartupMessage("CRI and CCT in summary_table() are disabled, ",
                          "please install package 'spacesXYZ' to enable them.")
  }
}

.onLoad <- function(libname, pkgname) {
  # on-line/off-line mode depends on availability of package rOmniDriver
  options(ooacquire.offline = !requireNamespace("rOmniDriver", quietly = TRUE))
}
