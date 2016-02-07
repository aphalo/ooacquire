#' Extract integration time and instrument serial number.
#'
#' Reads and parses the header of the raw data file to extract the integration
#' time used and the serial number of the instrument. It is designed to work
#' with the output files from SpectraSuite, the software from Ocean Optics.
#'
#' @param file_name The path to the text file with raw data from the
#'   spectrometer.
#' @return Returns a list with two items: "integration_time" and "serial_no".
#'   \describe{ \item{integration_time}{ A numeric value with integration time
#'   in microseconds.} \item{searial_no}{A character string with the serial
#'   number of the instrument}}
#' @export
#' @author Pedro J. Aphalo
#' @references \url{http://www.r4photobiology.info}
#' @keywords misc
#'
read_file_header <-
function(file_name)
{
#  parsed_line_timedate <- scan(file=file_name, nlines=1, skip=2,
#  what=list("character","character",month="character",day="numeric","
#  character","character",year="numeric") )
  line03 <- scan(file=file_name, nlines=1, what=list("character"), skip=2, sep="\n", quiet=TRUE)[[1]]
  line03 <- sub("Date: [[:alpha:]]{3} ", "", line03)
  time_zone <- sub("^(.{16})([[:upper:]]{3,4})(.{5})$", "\\2", line03)
  if (nchar(time_zone) == 4) {time_zone <- sub("S", "", time_zone)} # the S is for summer time but it is not needed
  line03 <- sub("[[:upper:]]{3,4}[[:space:]]", "", line03)
  line09 <- scan(file=file_name, nlines=1, skip=8, what=list("character"), quiet=TRUE)
  line10 <- scan(file=file_name, nlines=1, skip=9, what=list("character"), quiet=TRUE)
  line18 <- scan(file=file_name, nlines=1, skip=17, what=list("character"), quiet=TRUE)[1]
  return(list(meas_time = as.POSIXct(line03, format="%B %d %H:%M:%S %Y", tz="EET"),
              integration_time = as.numeric(line09[[1]][4]),
              num_scans = as.numeric(line10[[1]][3]),
              serial_no = gsub("[()]", "", line09[[1]][5]),
              decimal=substr(line18[[1]][1], 4, 4)))
}
# @note Example head of a file showing the expected format:\\
# SpectraSuite Data File
# ++++++++++++++++++++++++++++++++++++
#   Date: Wed May 16 14:30:04 EET 2012
# User: OO Maya
# Dark Spectrum Present: No
# Reference Spectrum Present: No
# Number of Sampled Component Spectra: 1
# Spectrometers: MAYP11278
# Integration Time (usec): 1000000 (MAYP11278)
# Spectra Averaged: 4 (MAYP11278)
# Boxcar Smoothing: 0 (MAYP11278)
# Correct for Electrical Dark: No (MAYP11278)
# Strobe/Lamp Enabled: No (MAYP11278)
# Correct for Detector Non-linearity: No (MAYP11278)
# Correct for Stray Light: No (MAYP11278)
# Number of Pixels in Processed Spectrum: 2068
# >>>>>Begin Processed Spectral Data<<<<<


