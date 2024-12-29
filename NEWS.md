---
editor_options: 
  markdown: 
    wrap: 72
---

# ooacquire 0.5.2 (2024-12-xx)

_Two major bugs fixed, although neither of them affected the quality of the
spectral data._ The bugs were likely introduced in version 0.5.1 or 0.5.0. I
find surprising that the first one at least slipped through testing. However, I
do the testing of the interactive data-acquisition functions manually and I need
access to a spectrometer for this. The unit tests check the data conversion and
file reading functions. In addition to bug fixes the interactive menus and 
messages in function `acq_irrad_interactive()` are improved.

- Bug fixed: Error triggered when acquiring spectra interactively. The problem
was related to parameters in the interactive acquisition functions that expect
function definitions as arguments. Fixed by replacing `NULL` with `NA` as
default values and changing internal tests. This change does not affect user
code or the user interface of the affected functions.
- Bug fixed: menus for collections and summaries not displayed.
- Improved: entry of metadata and comments. Added possibility of replacing or
adding 'what.measured' and 'comment' metadata to the spectra when the plot is 
displayed. The plot is subsequently refreshed as for other settings in the
same menu, using the updated metadata.
- Improved: The currently set metadata are displayed above the menu where they
can be changed. As before, the values are reused for the next spectrum unless
modified.
- Improved: The wavelength range in plots obeys the value of R 
option `ggspectra.wlrange` if set. Newly, the R option setting can be modified
interactively in the menu visible when the plot is displayed. The plot is 
subsequently refreshed as for other settings in the
same menu.
- Fixed: Confusing sequence of messages when acquiring repeats.
- Changed: Time in plot subtitle now uses the local timezone. Fractions of 
seconds are rounded according to R option `digits.secs` if set and to the
nearest millisecond otherwise.

# ooacquire 0.5.1-1 (2024-09-02)

- Unit tests updated to 'photobiology' (>= 0.11.3) and this version is now a
requirement. No other code changes, just update to saved expected values
returned by `summary()` in tests.

# ooacquire 0.5.1 (2024-08-20)

Minor update to make it easier to synchronise acquisition of spectra and other
events using trigger functions.

- Update default arguments of `acq_irrad_interactive()`.
- Update example script `acq-interac-using-triggers.R`.

# ooacquire 0.5.0 (2024-08-02)

**Updates to interactive acquisition of spectra. Main improvement is in the
triggering of events synchronised with spectral data acquisition. The changes
to the triggers UI is code breaking except when using defaults. Code unrelated
to the use of triggers is not affected.**

-   Rename parameter `f.trigger.flashes` to `f.trigger.on` and add parameters
`f.trigger.off` and `f.trigger.init`, `triggers.enabled` to both
`acq_irrad_interactive()` and `acq_fraction_interactive()`.
-   Update functions `acq_raw_spct()`, `acq_raw_mspct()``and `hs_acq_raw_mspct()`
to support `f.trigger.on`, `f.trigger.off` and ``triggers.enabled`.
-   Update function `acq_fraction_interactive()` to be in sync with all updates
to `acq_irrad_interactive()`.
-   Add YoctoRelay example script.

# ooacquire 0.4.6 (2024-03-29)

-   Add function `update_bad_pixs()`.
-   Extend validity of 2023 calibrations of MAYP11278 to two years.

# ooacquire 0.4.5 (2024-02-27)

**Improved performance for the two Maya spectrometers with slit-function
characterization available, at the expense of very slightly different computed
spectral irradiances (differences are at most in the 5th significant place
of individual spectral irradiance values).**

-   Update the tail correction functions to use a narrower moving window from
    150 nm to 20 nm, with very little impact on the returned values but a 
    sizeable reduction in execution time from 26 ms to 8 ms per spectrum.
-   Consolidate documentation of instrument data into one page per instrument.

# ooacquire 0.4.4-4 (2024-02-22)

**The new features allowing measurement of time series of thousands of spectra
at a very fast rate are easily affected by glitches in the USB connection. If
the acquisition hangs from time to time, a high quality and short USB cable with
ferrite beads is likely to help.**

-   Edit menus used in `acq_irrad_interactive()` to be narrower and more 
    informative.
-   Improve interactive input of `numeric` and `period` values, so that bad 
    input is handled gracefully in `acq_irrad_interactive()`.
-   Add activity messages to keep the user informed about time-consuming 
    activities (several activity messages are shown only for long time series)
    in `acq_irrad_interactive()`.
-   Display percent progress in `s_irrad_interactive()` when used interactively.
-   Decrease the default maximum number of spectra per plot to speed up user
    interaction in `acq_irrad_interactive()`.
-   Revise function `QC_dark()` to use interpretable values as arguments to
    parameters. This is a potentially code-breaking change as the new defaults
    can alter the sensitivity of the quality control applied to dark spectra.
-   Revise function `merge_cps()` to accept a negative value as argument to
    `tolerance` to disable merging (= splicing) using only the primary scan.
-   Revise function `beekd_nas()` to improve performance (a lot).
-   Updates in 'photobiology' 0.11.0 and 'photobiology' 0.11.1 improve/fix 
    handling of metadata, especially in time series.
-   Update in 'photobiology' 0.11.1 improves the speed of plotting sampled
    time series.
-   Merge and expand the computation algorithms and calibration vignettes and
    move it [on-line](https://www.r4photobiology.info/pages/ooacquire-algorithms.html).
-   Track a small change in an indirect dependency on package 'spacesXYZ'.

# ooacquire 0.4.3 (2023-12-28)

-   Make real-time display of plots optional (no change to default).
-   Implement adjustment of the step delay time in time series to be a multiple
    of the integration time for buffered acquisition and normal acquisition 
    when not using HDR.
-   Implement multiple repeats of individual measurements including whole time 
    series. As in earlier versions, repeats reuse 'dark' and 'filter' 
    measurements from the most recent previous measurement.
-   Implement generation of sequentially "numbered" object names and file names. 
    Available in all interface modes.
-   Fix bug: crash in "slow" series acquisition introduced in 0.4.2.
-   Fix bug: wrong estimate of duration of individual measurements for "fast" 
    (buffered) series acquisition.
-   Fix bug in `no_filter_correction()`: stray light correction would be too
    easily skipped. This was causing problems only with the protocol that did not
    rely on a dark measurement to correct for stray light in those cases when
    the dark pixels in the spectrometer array returned unreliable readings. The
    new approach can be potentially disturbed by light sources strongly emitting at
    wavelengths shorter than about 240 nm.
-   Accept additional ways of inputting time periods in the menu for settings
    for acquisition of time series of spectra.
-   Minor fixes and tweaks to the user interface for selecting protocols.
-   Use R package 'mirai' to asynchronously save R data files and PDF files.
    Package 'mirai' is now in "suggests" and has to be installed to enable this
    feature. Experimental, by default not enabled, as it is unreliable 
    under Windows 11 and not yet tested under Unix or Linux..
 
# ooacquire 0.4.2 (2023-12-03)

-   Check and update bad (hot) pixels in MAP11278 calibration (add one pixel).
-   Depend on 'photobiology' (>= 0.11.0).

# ooacquire 0.4.1 (2023-06-11)

-   Add `QC_dark()` and implement its use in acquisition and in 
    `s_irrad_corrected()`. Aimed at detecting problems with raw-counts dark 
    baseline, specifically, pixel-position dependent enhanced dark counts 
    usually as a consequence of increased warming of the spectrometer.
-   In `merge_cps()` check consistency among data for different integration 
    times before merge and avoid merges within 250-400 nm region.
-   EXPERIMENTAL!! Scale filter reference spectrum based on NIR readings
    (not enabled by default).

# ooacquire 0.4.0 (2023-06-02)

-   Interface mode `"series"` of function `acq_irrad_interactive()` is fully 
    functional and quite well tested.
-   Tested working with OmniDriver 2.72 (2023-05-10), 2.71 and 2.56. Most other
    versions most likely also work.

# ooacquire 0.3.4 (2023-05-25)

-   Update `acq_irrad_interactive()` to measure spectral fluence in addition
    to spectral irradiance. Remove `acq_fluence_interactive()` and 
    `acq_fraction_pulsed_interactive()`.
    
# ooacquire 0.3.3-1 (2023-05-10)

-   Fix bug that made interface mode "series-attr" crash on attempts to change
    the step delay setting.

# ooacquire 0.3.3 (2023-05-09)

-   Improved estimate of measurement duration, and display message.
-   When possible, if series step delay is too short, set it to zero, and
    otherwise to the estimated duration.
-   Fix bug in high speed acquisition of series with protocols ld and lfd.
    
# ooacquire 0.3.2-1 (2023-04-27)

-   Tweaked menu texts and made it possible to select among types of collection
    summaries.
-   Moved UI for saving of collections to its own menu separate from that for
    quit/repeat/NEXT.
    
# ooacquire 0.3.2 (2023-04-19)

-   Export `irrad_summary_table()`, previously, an internal function. Also 
    enhance this function.
-   Enhance support for different types of summaries in `irrad_summary_table()`.
-   Update `acq_irrad_interactive()` adding menu option to repeat last
    measurement using same protocol, integration settings and "dark" and
    "filter" reference spectra.
-   Fix intermittent failure to initialize high-resolution-time API used for
    high speed time series acquisition of spectra.

# ooacquire 0.3.1 (2023-04-11)

-   Implement high speed acquisition of time series using special API functions
    through new wrappers in 'rOmniDriver' (>= 0.1.18).
-   Add new calibrations for MAYP11278 with cosine and hemispherical diffusers.
-   Add new calibration for MAYP112785 with cosine diffuser.
-   Calibration data missing in 'ooacquire' for NIR region even when available, 
    fixed. For a couple of calibrations the UV end was slightly truncated.
-   Add support for multiple descriptors per instrument and optionally include
    a description of the entrance optics in the instrument descriptor.
-   Add a slot to the calibration field in the descriptor to store the _source_
    or identifier of the calibration to allow traceability.
-   Add "how.measured" attribute to acquired spectra.
-   Update code used for plot title, subtitle and caption. Date times were
    no longer formatted correctly.
-   Photsyntheticaly active radiation can be defined as a range of wavelengths
    (PhR) or as an effective or weighted quantitiy (PAR, as defined by McCree).
    When expressed as photons they are identical, but when expressed as energy 
    they differ and the difference depends on the shape of the light spectrum.
    Previously, PhR as energy was incorrectly labelled PAR, as it frequently is.
    In 'photobiologyWavebands' (0.5.1), this has been corrected. This version
    uses the correct labeling in plots and quantities and makes it possible to
    choose between PAR and PhR, with PhR as default, so that the numbers
    returned remain the same, but are correctly labeled.
-   Tested working with OmniDriver 2.71, 2.56 and 2.46, under current R 4.2.3 
    and coming R 4.3.0.

# ooacquire 0.3.0 (2023-04-01)
    
-   Tested working with OmniDriver 2.71 and 2.56, under current R 4.2.3 and 
    coming R 4.3.0. (Maya and Flame recognized by 2.71 and 2.56. USB2000
    supported in 2.56 but not in 2.71. USB2000 requires update to firmware 2.41
    for recent MS-Windows to recognize it.)
-   Tolerate missing values returned by functions removed from recent
    versions of OmniDriver. (API changes in OmniDriver 2.71 and 2.70!)
-   Automatic acquisition of time series of spectra: implementation functional.
-   New `interface.mode` `"full"` combines "manual" and "auto".
-   Several messages updated to better accommodate time series acquisition.
-   Add to default protocols "dl" and "dfl", better suited to measurement of
    time series.
-   Fix bugs related to dates and times caused by changes in 'lubridate'.
-   Function `s_irrad_corrected()` updated to work with multiple "light"
    measurements as created by measurement of time series.
-   Versions of packages imported from are updated.

# ooacquire 0.2.6 (2022-10-18)

-   Track changes in 'photobiology' >= 0.10.14.
-   Fix for deprecated functions in 'dplyr' >= 1.0.0.
-   Revise UI of `acq_irrad_interactive()` to avoid accidental overwrite of
    data files, and really do not save any data for discarded spectra.
-   Tested working with OmniDriver 2.56. Fails with OmniDriver 2.70. 

# ooacquire 0.2.5 (2022-09-30)

-   Implement an **off-line** mode in which data acquisition is disabled but
    computations are still possible. The **off-line** mode is automatically
    entered if package 'rOmniDriver' is not installed or if its initialization
    fails. This change removes the need to install Java, drivers from Ocean 
    Insight and R packages 'rOmniDriver' and 'rJava' when data to be processed
    have been acquired using another computer or with software from Ocean 
    Insight. 
-   Fix bug in the generation of the .CSV file with summaries from collections 
    of spectra.

# ooacquire 0.2.4 (2022-06-24)

-   Function `acquire_continuous_interactive()` gains three new
    parameters that make it possible to override defaults and gets
    several small tweaks to the user interface including dynamic default
    for protocol.
-   Function `acquire_continuous_interactive()` now sets defaults for
    user/operator name, session name and folder used for output based on
    logged-in user's name, current day and time and spectral quantity
    returned.
-   Revise the user interaction in `set_folder_interactive()` and add
    check for success of new folder creation.
-   Support in `acq_raw_mspct()` the acquisition of a timed sequence of
    spectra.
-   Improve summary for collection of spectra in
    `acq_continuous_interactive()`.
-   Fix bug in reading of raw data from files possibly caused by changes
    in 'readr'.
-   Fix bug in `which_descriptor()` that resulted in wrong text in
    warnings.
-   Fix bug in `which_descriptor()` that resulted in failure to handle
    well gaps between the validity of calibrations or dates before the
    earliest calibration.
-   Expand description of algorithms.
-   Test with OmniDriver 2.56 and 2.68.

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
