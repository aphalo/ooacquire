---
editor_options: 
  markdown: 
    wrap: 72
---

## TODO

-   Add more test cases.
-   Finish writing algorithms vignette.
-   Document support for OO calibrations and add example scripts.
-   Move calibration data to separate packages.

# ooacquire 0.2.3 (2021-03-26)

-   Implement support for on-the-fly retrieval of instrument descriptor
    and calibration data from EEPROM when calibration is not available
    as R objects.
-   Retrieval of instrument descriptor and wavelength calibration from
    spectrometer EEPROM tested.
-   Bugs fixed in retrieval of irradiance calibration factors as they
    would have been used without proper conversion into the format used
    in the package. (Testing with a factory-calibrated spectrometer with
    data in EEPROM pending!!)
-   Updated only `acq_irrad_interactive()`. Similar tests and FIXES are
    needed in the other data acquisition functions.
-   Update calibration data for MAYP11278 and MAYP112785.
-   Test with old USB2000 spectrometer and fix the problems detected.

# ooacquire 0.2.2 (2021-01-16)

-   Revise `raw2corr_cps()` and `uvb_corrections()` so as to remove
    duplicated code, adding also support for despiking.
-   Revise `trim_counts()` so that it handles gracefully data acquired
    with "correct for electrical dark" enabled in OO software.
-   Revise `raw2cps()` so that it tolerates a bug in
    `photobiology::msmsply()`.
-   Revise `acq_irrad_interactive()` so that when a collection is
    created a plot with all the spectra in the collection and a summary
    table are also created. The plot is saved as a PDF file and the
    table is saved as a CSV file.
-   Revise menues in `acq_irrad_interactive()` to make the prompts
    shorter, and the defaults more obvious.
-   Attempting to use a calibration after its end-of-life issues a
    warning instead of an error.
-   Replace white LED data (old data was bad) and add data for sunlight,
    which is a better example for the algorithms.

# ooacquire 0.2.1-1 (2020-08-15)

-   Update for new versions of packages 'tibble' and 'photobiology'.
-   Bug fix in calibration date checks (dates before start of validity
    of oldest available calibration did not trigger an error).

# ooacquire 0.2.1 (2020-06-08)

-   Add pre-built calibration and methods for one Maya 2000 Pro
    instrument using calibration data from Ocean Optics.

# ooacquire 0.2.0 (2019-08-20)

-   Update `acq_irrad_interactive()` adding alternative user-interface
    modes, allowing users to set comment and what.measured attributes,
    and to constructing collections of spectra.

-   Add utility functions `collect_spct_files()` and `plot_spct_file()`.

-   Add pre-built calibrations and methods for three additional FLAME-S
    instruments.

-   Improve algorithm for automatic tuning of integration time.

-   Fix various minor bugs.

-   Update vignettes.

-   Test against **OmniDriver 2.56**

-   Update for 'readr' \>= 1.3.0 which is now required

## Measurement of pulsed light sources (*under development*)

The new features described below are under development, subject to
change and not yet well tested!! (double-check any results and please
raise issues if you find any bugs!)

Implement support for measuring output from xenon flashes, including use
of such flashes as light sources for measurement of spectral
transmittance and reflectance. Use of multiple exposures per integration
is also supported and the number of exposures stored as metadata is used
during later processing. In the case of pulsed light sources the first
conversion of RAW spectra is into counts per exposure, and instead of
spectral irradiance spectral fluence per exposure is computed. All
changes are backwards compatible with raw data acquired with earlier
versions of 'ooacquire'.

-   Add function `acq_fluence_interactive()` for measuring spectral
    fluence of pulsed light sources.
-   Add `acq_fraction_pulsed_interactive()` for measuring transmittance
    and reflectance using a pulsed light source.
-   Add example scripts for these new functions.

# ooacquire 0.1.5 (2017-11-13)

-   Fix serious design bug that triggered a fatal error with current
    version of 'Rcpp'.
-   Add functions `choose_sr_interactive()` and
    `choose_ch_interactive()` and edit `acq_irrad_interactive()` and
    `acq_fraction_interactive()` to make use of them.
-   Add fields to the instrument descriptor to store the numeric
    coefficients in addition to functions for non-linearity and
    wavelength calibrations.
-   Add pre-built calibrations and methods for our Jaz instrument.
-   Add support for irradiance calibrations done by Ocean Optics.
-   Add pre-built calibrations and methods for two FLAME-S instruments.

# ooacquire 0.1.4 (2017-05-11)

-   Test transmittance and reflectance acquisition functions against a
    different MayaPro spectrometer and a Jaz spectrometer with no
    irradiance calibration, and fix the bugs that became apparent.

# ooacquire 0.1.3

Test irradiance acquisition functions against a different MayaPro
spectrometer, and fix the bugs that became apparent. Tested against
latest OmniDriver 1.4.6.

# ooacquire 0.1.2

-   Add support for OceanView files, and a second set of calibration
    data.
-   Rewrite much of the `uv_corrections()` and `s_irrad_corrected()`
    code, and add support for remapping names of `raw_mspct` members' to
    `"light"`, `"filter"` and `"dark"`. Rewrite
    `s_fraction_corrected()`, and add support for remapping names of
    `raw_mspct` members' to `"sample"`, `"reference"` and `"dark"`. In
    the process fixed bug that was causing over-correction for stray
    light in the UVA region.
-   Add additional data to the descriptor. Changed test for too low
    signal for original filter correction to be more permissive, and
    instead of skipping the filter correction we use newly added method
    `"simple"`.
-   Rename parameter `"method"` to `"correction.method"` for clarity.
-   Add warning for cases when users supply spectra with negative values
    for raw-detector counts (electrical dark correction set to true).
-   Add two new vignettes: *Quick Start Guide*, and *Algorithms*.
-   Remove old sample scripts and rename the remaining ones.

# ooacquire 0.1.1

-   Add function `bleed_nas()` to be used to discard data from sensor
    pixels adjacent to saturated/clipped pixels.
-   Add function for reading data from *SpectraSuite* raw data files.
-   Add functions for reading metadata from *SpectraSuite* raw data
    files. Parsing is quite flexible, so hopefully robust across
    versions of the software and spectrometer models and their possible
    configurations.
-   Add generic `s_irrad_corrected()` with methods for `raw_mspct`
    objects and for lists of file names.
-   Add generic `s_fraction_corrected()` with methods for `raw_mspct`
    objects and for lists of file names.
-   Add automatic support for different protocols including bracketing
    based on the the raw spectral data passed as argument.
-   Add function `compute_irrad_calibration()` accepting measurements of
    both D2 and FEL lamps and the coefficients for reconstructing the
    emission of the lamps.
-   Add functions `acq_irrad_interactive()`, and
    `acq_trans_interactive()`.
-   Ensure that all metadata is copied and at all processing steps.
-   Add example data.

# ooacquire 0.1.0

First version.