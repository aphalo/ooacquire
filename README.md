# R package 'ooacquire' for _Ocean Optics_ spectrometers

## Abstract

Package **'ooacquire'** makes it possible to control, modify settings and acquire spectral data directly from within R. It also supports the conversion of raw-counts data into physical quantities. It supports most types of spectrometers available from _Ocean Optics_ (https://oceanoptics.com/).
The free runtime of the _OmniDriver SDP_ and _Java_ need both to be installed before data acquisition is possible. The runtime itself and its documentation can be downloaded at https://oceanoptics.com/product/omnidriver/.

## Details

Package **'ooacquire'** provides high level functions for spectral data acquisition built using lower level functions from package **'rOmniDriver'** as a base. Acquisition is very flexible with respect to measuring protocols. It caters for all steps involved in the acquisition of spectral data from connecting to the instrument(s) and retrieving information from non-volatile memory, setting and adjusting acquisition parameters, acquiring raw counts and converting them into counts per second. It supports bracketing of the integration time for high dynamic range (HDR) protocols, both with respect to data acquisition and merging/splicing of spectra. It also supports protocols in which the total measuring time is kept constant by adjusting in coordination integration time and number of scans averaged. It works seamlessly together with package **photobiology** on which it also depends.

In addition to directly acquiring RAW counts data, raw-counts data can be read from files with automatic decoding of the corresponding acquisition metadata from file headers. Files saved by _OceanView_ or _SpectraSuite_ software from Ocean Optics, or directly by Jaz spectrometers can be read. Raw data read from files and acquired directly is stored in the same format. Consequently, data from either origin can be used as the starting point for the computation of spectra expressed as corrected counts-per-second with the same flexibility and code.

High level functions in this package and in package **photobiology** allow the easy conversion of counts-per-second into the physical quantities of interest such as spectral irradiance, spectral transmittance, spectral reflectance, spectral absorptance and spectral absorbance. 

Our package's functions related to direct data acquisition use the free _OmniDriver_ run-time which in turn requires _Java_. There is no other set up needed, just plug your spectrometer to an USB port. The first time you connect an instrument the operating system will install the drivers as they are made available by the _OmniDriver_ installation.

Direct acquisition has been tested with our _Maya2000Pro_, _Flame_ and _Jaz_ instruments under MS-windows 7 and MS-Windows 10, but can be expected also to work with any other modern spectrometer from Ocean Optics, and under OS X, and Linux. Package **'ooacquire'** manages acquisition settings semi-automatically storing all the settings needed for acquisition into a single data object. Functions for automatic tuning of integration time are also provided. Settings used for acquisition of spectra and a descriptor of the instrument are stored at the time of acquisition as attributes of the object where the raw counts are stored. These metadata are preserved through all processing steps.

## Technical aspects

Package **'rOmniDriver'** makes available in R the API functions from the _OmniDriver SDP_ by wrapping the Java calls in R functions of the same name and doing argument type conversions when needed. _OmniDriver_ allows to change settings and acquire spectra using any Ocean Optics USB-connected spectrometer. 

## Non-commercial status

Packages **'rOmniDriver'** and  **'ooacquire'** are both open source and released under a GPL licence. Neither **'rOmniDriver'** nor **'ooacquire'** require the commercial software _OceanView_ or _SpectraSuite_ to be installed, but should be able to coexist with either of them. They do not require the purchase of any software from Ocean Optics, but the use of these packages or the free _OmniDriver_ runtime is not supported in any way by Ocean Optics, unless you acquire a licence to the _OmniDriver SDP_. The _OmniDriver SDP_ is not open source and is proprietary software copyrighted by Ocean Optics and supporting only the use of hardware sold by this company (https://oceanoptics.com/).

