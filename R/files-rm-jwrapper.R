#' Remove Java wrapper from objects in files
#'
#' Remove Java wrappers stored in field \code{"w"} from the
#' \code{instr.descriptor} metadata attribute of spectral objects and of members
#' of collections of spectra loaded from R data files, saving them using their
#' original name and location, after renaming the original files to serve as
#' backups.
#'
#' @param path character Vector of full path names; the default corresponds to
#'   the working directory. Tilde expansion (see \code{\link{path.expand}()}) is
#'   performed. See \code{\link{list.files}()} for details.
#' @param pattern character An optional regular expression. Only file names
#'   which match the regular expression will be returned. See
#'   \code{\link{list.files}()} for details.
#' @param recursive logical Should the listing recurse into directories?
#' @param date.limit POSIXct Files with more recent creation dates are skipped
#'   assuming they have been created with 'ooacquire' (>= 0.2.5 2022-09-30).
#' @param trim.descriptor logical If \code{TRUE} apply
#'   \code{trimInstrDesc()} instead of \code{rm_jwrapper()} to spectral objects
#'   containing data in physical units. \code{rm_jwrapper()} is always used for
#'   objects containing sensor counts, either raw of cps.
#' @param save.files logical If \code{FALSE} skip saving of files. Useful to
#'   test what files would be modified.
#' @param verbose logical Issue messages when unsupported objects or spectral
#'   objects already free of the problematic Java wrapper are encountered.
#'
#' @details
#' Spectral data objects acquired directly from a connected spectrometer
#' with 'ooacquire' (< 0.2.5) contain a leftover Java wrapper used to connect to
#' the spectrometer. This is of no use after disconnection, but if it remains
#' in the objects saved to an \code{.rda} file loading these files requires
#' 'rJava' and the Java JDK to be available. Some old \code{source_spct} and
#' \code{filter_spct} objects can also contain the full instrument descriptor
#' including calibration data, which makes the objects unnecessarily large.
#'
#' This function, searches folders for files with names matching \code{pattern},
#' loads them one by one if modified before the \code{date.limit}, removes the
#' spurious Java wrapper and saves the updated objects into a file with the
#' original name after renaming the original file.
#'
#' The defaults match the defaults of \code{s_irrad_corected()} and
#' \code{acq_irrad_interactive()} in the current version of 'ooacquire'.
#' Field \code{w} containing the \code{jwrapper} is deleted from all
#' spectral objects, and for objects containing spectral data expressed
#' in physical units, also fields related to instrument calibration are
#' deleted. If \code{trim.descriptor = FALSE} is passed in the call,
#' deleting the \code{w} is the action applied to all objects.
#' Any objects in the loaded files that are not spectral objects or
#' collections of spectral objects are kept unchanged.
#'
#' **The date-based file selection uses file modification time as Windows has
#' an unusual logic for file creation time, resulting in creation times being
#' later than modification time when a file is copied. When attached files are
#' downloaded or when files are downloaded they may get the creation and
#' modification times set to the time of downloading. This is only a speed
#' up for avoiding loading of files. Files containing objects that do not need
#' updating will not be modified even if they are loaded.**
#'
#' @section Warning!: It is strongly recommended that before committing
#' changes a test is first done to check which files will be updated.
#'
#' @return A \code{character} vector with the paths to the updated files,
#' returned silently.
#'
#' @seealso \code{\link{ls}()}, \code{\link{list.files}()},
#' \code{\link[ooacquire]{rm_jwrapper}()} which are used to list objects
#' and files, and to update remove the Java wrapper from 'rjava', respectively.
#'
#' @family Functions and methods related to instrument descriptors.
#' @seealso Use function \code{\link{rm_jwrapper}()} from individual
#'   objects in the R workspace.
#'
#' @export
#'
files_rm_jwrapper <-
  function(path = ".",
           pattern = "\\.spct\\.[Rr]da",
           recursive = TRUE,
           date.limit = NULL,
           trim.descriptor = TRUE,
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
      loaded.objects <- setdiff(ls(pattern = "\\.raw_mspct$|\\.spct"),
                                existing.objects)
      updated.objs <- 0L
      for (obj in loaded.objects) {
        temp <- get(obj, inherits = FALSE)
        if (!(photobiology::is.generic_spct(temp) ||
              photobiology::is.generic_mspct(temp))) {
          warning("Skipping unsupported object '", obj,
                  " of class \"", class(temp)[1], "\"")
          next()
        }
        if (trim.descriptor &&
            !is.raw_mspct(temp) && !is.raw_spct(temp) &&
            !is.cps_mspct(temp) && !is.cps_spct(temp)) {
          z <- photobiology::trimInstrDesc(temp)
        } else {
          z <- rm_jwrapper(temp, verbose = verbose)
        }

        if (!identical(temp, z)) {
          assign(obj, z)
          updated.objs <- updated.objs + 1L
        }
      }
      # files containing only objects with no Java wrappers are skipped
      if (save.files && updated.objs) {
        backup.file <- gsub("\\.spct.[Rr]da$", ".spct.rjava.rda", f)
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
