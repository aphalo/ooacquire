#' Read a file and plot spectrum
#'
#' Read an .Rda file as saved during data acquisition and plot it, possibly
#' first trimming the range of wavelengths and/or smoothing the spectral data.
#'
#' @param file a (readable binary-mode) connection or a character string giving
#'   the name of the file to load (when tilde expansion is done).
#' @param range a numeric vector of length two, or any other object for which
#'   function range() will return two.
#' @param method a character string "custom", "lowess", "supsmu".
#' @param strength numeric value to adjust the degree of smoothing.
#' @param na.rm a logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#'
#' @export
#'
#' @note If the argument passed to \code{method} is of length 2, the first
#'   member will apply to \code{source_spct} objects and the second one to
#'   \code{filter_spct} and \code{reflector_spct} objects. A numeric vector
#'   of length 2 passed to \code{strength} is treated in the same way. This
#'   is only relevant when we are collecting spectra belonging to different
#'   classes and need to treat them differently with respect to smoohting. It
#'   also allows different defaults, as transmittance spectra tend to lack
#'   the fine structure of some emission spectra.
#'
#' @seealso Function \code{\link[photobiology]{trim_wl}} is used to trim the
#'   range of the data to plot, and function
#'   \code{\link[photobiology]{smooth_spct}} is used for smoothing, and
#'   prameters \code{range}, and \code{strength} and \code{method} are passed to
#'   them, respectively.
#'
plot_spct_file <- function(file, range = NULL, method = NULL, strength = 0, na.rm = FALSE) {
  if (is.null(method)) {
    method <- c("supsmu", "custom")
  } else if (length(method) == 1L) {
    method <- rep(method, 2)
  }
  if (file.exists(file)) {
    load(file = file)
  } else {
    warning("File does not exist or is not accesible")
    return(ggplot2::ggplot())
  }
  object.name <- gsub(".Rda|.rda", "", x = basename(path = file))
  tmp.spct <- get(x = object.name)
  # if an object with the expected name was not loaded, we return an empty plot
  if (!exists(object.name, inherits = FALSE)) {
    warning("No object named '", object.name, "' was loaded.")
    return(ggplot2::ggplot())
  }
  # if the loaded object is not a spectrum, we return an empty plot
  if (!is.any_spct(tmp.spct)) {
    warning("Object '", object.name, "' is not a spectrum, it is a '", class(tmp.spct)[1], "'.")
    return(ggplot2::ggplot())
  }
  tmp.spct <- photobiology::trim_wl(tmp.spct, range = range)
  if (strength >= 0 && class(tmp.spct)[1] %in% c("filter_spct", "reflector_spct", "source_spct")) {
    tmp.spct <- photobiology::smooth_spct(tmp.spct,
                                          strength = strength,
                                          method = ifelse(is.source_spct(tmp.spct),
                                                          method[2],
                                                          method[1]),
                                          na.rm = na.rm)
  }
  ggplot2::autoplot(tmp.spct, annotations = c("+", "boundaries")) +
    ggspectra::stat_find_wls(target = 0.5) +
    ggspectra::stat_find_wls(geom = "text", target = 0.5, hjust = -0.5, size = 3) +
    ggplot2::ggtitle(object.name)
}

#' Collect spectra into a collection
#'
#' Read .Rda files as saved during data acquisition and build a collection with
#' the spectra read, possibly first trimming the range of wavelengths and/or
#' smoothing the spectral data.
#'
#' @param path a character string giving
#'   the name of the folder from wich to load .Rda files containing spectra.
#' @param range a numeric vector of length two, or any other object for which
#'   function range() will return two.
#' @param method a character vector of length 1 or 2, containing the smoothing
#'   method to use, one of "custom",  "lowess", or "supsmu".
#' @param strength numeric value to adjust the degree of smoothing.
#' @param name.root character string to prepend to the name of the spectra in
#'   the collection.
#' @param na.rm a logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#'
#' @export
#'
#' @note If the argument passed to \code{method} is of length 2, the first
#'   member will apply to \code{source_spct} objects and the second one to
#'   \code{filter_spct} and \code{reflector_spct} objects. A numeric vector
#'   of length 2 passed to \code{strength} is treated in the same way. This
#'   is only relevant when we are collecting spectra belonging to different
#'   classes and need to treat them differently with respect to smoohting. It
#'   also allows different defaults, as transmittance spectra tend to lack
#'   the fine structure of some emission spectra.
#'
#' @seealso Function \code{\link[photobiology]{trim_wl}} is used to trim the
#'   range of the data to plot, and function
#'   \code{\link[photobiology]{smooth_spct}} is used for smoothing, and
#'   prameters \code{range}, and \code{strength} and \code{method} are passed to
#'   them, respectively.
#'
collect_spct_files <- function(path = ".", range = NULL, method = NULL, strength = 0, name.root = "", na.rm = FALSE) {
  if (length(name.root) > 0L && name.root != "") {
    name.root <- paste(name.root, "_", sep = "")
  }
  if (is.null(method)) {
    method <- c("supsmu", "custom")
  } else if (length(method) == 1L) {
    method <- rep(method, 2)
  }
  spectra <- list()
  for (file in list.files(path, "spct.rda", full.names = TRUE, ignore.case = TRUE)) {
    load(file = file)
    object.name <- gsub(".Rda|.rda", "", x = basename(path = file))
    # if an object with the expected name was not loaded we jump to the next iteration
    if (!exists(object.name, inherits = FALSE)) {
      next()
    }
    tmp.spct <- get(x = object.name)
    # if the object with the expected name is not a spectrum we jump to the next iteration
    if (!is.any_spct(tmp.spct)) {
      warning("Object '", object.name, "' is not a spectrum, it is a '", class(tmp.spct)[1], "'.")
      next()
    }
    # as tmp.spct may contain bad data, we do trimming before smoothing
    tmp.spct <- photobiology::trim_wl(tmp.spct, range = range)
    # we always skip smoothing for object_spct, raw_spct and cps_spct objects
    if (strength >= 0 && class(tmp.spct)[1] %in% c("filter_spct", "reflector_spct", "source_spct")) {
      tmp.spct <- photobiology::smooth_spct(tmp.spct,
                                            strength = strength,
                                            method = ifelse(is.source_spct(tmp.spct),
                                                            method[2],
                                                            method[1]),
                                            na.rm = na.rm)
    }
    # valid file names may be invalid as R object names, or if valid, still require quoting
    # make.names() ensures that names are "nice" in R
    spectra[[make.names(paste(name.root, gsub(".spct", "", object.name), sep = ""))]] <- tmp.spct
  }
  # The class of the container object for the collection is inferred from the
  # class of the individual spectra.
  photobiology::generic_mspct(spectra,
                              class = photobiology::shared_member_class(spectra)[1])
}

# collect_spct_files("Zomei", range = c(350, 1050), strength = 2)
#
# plot_spct_file("Zomei/IR950.spct.Rda", range = c(350, 1050), strength = 2)
