#' Recompute spectral irradiance in objects in files
#'
#' Recompute spectral irradiance in \code{source_spct} objects stored in
#' \code{.rda} files created with \code{\link{acq_irrad_interactive}()} from
#' earlier versions of 'ooacquire' using the calibrations and methods in the
#' current version of 'ooacquire'.
#'
#' @inheritParams files_rm_jwrapper
#' @inheritParams s_irrad_corrected_updt
#' @param trim.descriptor logical Passed to
#'   \code{\link{s_irrad_corrected_updt}()}.  If TRUE the spectrometer
#'   calibration constants, pixel wavelengths, slit-function "tail-correction"
#'   function code and other calibration-related information is deleted.
#'
#' @details
#' Spectral data objects acquired directly from a connected spectrometer
#' with 'ooacquire' contain embedded calibration data and correction algorithms
#' that by default are used when spectral irradiance is computed. Function
#' \code{\link{s_irrad_corrected_updt}()} uses the current matching
#' descriptor.
#'
#' This function, searches folders for files with names matching \code{pattern},
#' loads them one by one if modified before the \code{date.limit}, removes the
#' Java wrapper if present and recomputes spectral irradiance with
#' \code{\link{s_irrad_corrected_updt}()} and saves the updated objects into a
#' file with the original name after renaming the original file.
#'
#' The defaults match the defaults of \code{s_irrad_corrected()} and
#' \code{acq_irrad_interactive()} in the current version of 'ooacquire'.
#' Field \code{w} containing the \code{jwrapper} is deleted from all
#' spectral objects, and for objects containing spectral data expressed
#' in physical units, also fields related to instrument calibration are also
#' deleted by default. If \code{trim.descriptor = FALSE} is passed in the call,
#' deleting the \code{w} is the action applied to all objects.
#'
#' @inheritSection files_rm_jwrapper Date related uncertainties
#' @inheritSection files_rm_jwrapper Warning!
#'
#' @return A \code{character} vector with the paths to the updated files,
#' returned silently. As a side effect files can be saved containing updated
#' objects while keeping original files renamed as backups. By default no
#' files are saved.
#'
#' @seealso \code{\link{ls}()}, \code{\link{list.files}()},
#'   \code{\link{s_irrad_corrected_updt}()} which are used to list
#'   objects and files, and to update spectral irradiance, respectively.
#'
#' @family Functions and methods related to instrument descriptors.
#' @seealso Use function \code{\link{s_irrad_corrected_updt}()} for individual
#'   objects in the R workspace.
#'
#' @export
#'
files_s_irrad_updt <-
  function(path = ".",
           pattern = "\\.spct\\.[Rr]da",
           recursive = TRUE,
           date.limit = NULL,
           hdr.tolerance = getOption("ooacquire.hdr.tolerance", default = 0.05),
           return.cps = NULL,
           trim.descriptor = TRUE,
           which = NULL,
           which.not = NULL,
           save.files = FALSE,
           verbose = getOption("photobiology.verbose",
                               default = FALSE)) {
    if (!save.files) {
      message("TEST RUN: No files will be saved or modified!")
    }
    # find files
    all.files <- list.files(path = path,
                            pattern = pattern,
                            full.names = TRUE,
                            recursive = recursive)
    # subset based on creation time
    if (!is.null(date.limit)) {
      selector <- file.mtime(all.files) < as.POSIXct(date.limit)
      files <- all.files[selector]
      message("Found ", length(files),
              " files. Skipped ", length(all.files) - length(files), " files")
    } else {
      files <- all.files
      message("Found ", length(files), " files")
    }
    # make sure we do not mess other files
    existing.objects <- ls(pattern = "\\.raw_mspct$|\\.spct")
    updated.files <- character()
    for (f in files) {
      message("---\n", basename(f))
      load(f)
      loaded.raw.objects <- setdiff(ls(pattern = "\\.raw_mspct$"),
                                existing.objects)
      updated.objs <- 0L
      for (obj in loaded.raw.objects) {
        temp <- get(obj, inherits = FALSE)
        if (!(photobiology::is.raw_mspct(temp))) {
          warning("Skipping unsupported raw-counts object '", obj,
                  " of class \"", class(temp)[1], "\"")
          next()
        }
        source.obj <- gsub("\\.raw_mspct$", ".spct")
        if (!exists(source.obj) ||
            !photobiology::is.source_spct(get(source.obj))) {
          warning("Skipping unsupported irradiance object '", obj,
                  " of class \"", class(temp)[1], "\"")
          next()
        }
        temp <- rm_jwrapper(temp, verbose = verbose)
        z <- s_irrad_corrected_updt(x = temp,
                                    y = get(source.obj),
                                    spct.names = spct.names,
                                    correction.method = correction.method,
                                    hdr.tolerance = hdr.tolerance,
                                    which = NULL,
                                    which.not = NULL,
                                    verbose = verbose)

        assign(obj, temp)
        assign(source.obj, z)
        updated.objs <- updated.objs + 1L

      }

      if (save.files) {
        backup.file <- gsub("\\.spct.[Rr]da$", ".spct.bak.rda", f)
        file.rename(from = f, to = backup.file)
        stopifnot(file.exists(backup.file))
        save(list = loaded.objects, file = f)
        updated.files <- c(updated.files, f)
        message("Updated ", updated.objs, " objects. Saved!")
      } else {
        message("Updated ", updated.objs, " objects. Not saved!")
      }
      rm(list = loaded.objects)
    }
    invisible(updated.files)
  }
