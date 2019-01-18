#' Read a file and plot spectrum
#'
#' Read an .Rda file as saved during data acquisition and plot it, possibly
#' first trimming the range of wavelengths and/or smoothing the spectral data.
#'
#' @param file a (readable binary-mode) connection or a character string giving
#'   the name of the file to load (when tilde expansion is done).
#' @param range a numeric vector of length two, or any other object for which
#'   function range() will return two.
#' @param strength numeric value to adjust the degree of smoothing.
#'
#' @export
#'
#' @seealso Function \code{\link[photobiology]{trim_wl}} is used to trim the
#'   range of the data to plot, and function
#'   \code{\link[photobiology]{smooth_spct}} with \code{method = "supsmu"} are
#'   used in the implementation, and prameters \code{range} and \code{strength}
#'   are passed to them, respectively.
#'
plot_spct_file <- function(file, range = NULL, strength = 0) {
  load(file = file)
  object.name <- gsub(".Rda|.rda", "", x = basename(path = file))
  tmp.spct <- get(x = object.name)
  if (!is.any_spct(tmp.spct)) {
    return(ggplot2::ggplot())
  }
  tmp.spct <- photobiology::trim_wl(tmp.spct, range = range)
  tmp.spct <- photobiology::smooth_spct(tmp.spct, strength = strength, method = "supsmu")
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
#' @param strength numeric value to adjust the degree of smoothing.
#' @param name.root character string to prepend to the name of the spectra in
#'   the collection.
#'
#' @export
#'
#' @seealso Function \code{\link[photobiology]{trim_wl}} is used to trim the
#'   range of the data to plot, and function
#'   \code{\link[photobiology]{smooth_spct}} with \code{method = "supsmu"} are
#'   used in the implementation, and prameters \code{range} and \code{strength}
#'   are passed to them, respectively.
#'
collect_spct_files <- function(path = ".", range = NULL, strength = 0, name.root = path) {
  spectra <- list()
  for (file in list.files(path, "spct.rda", full.names = TRUE, ignore.case = TRUE)) {
    load(file = file)
    object.name <- gsub(".Rda|.rda", "", x = basename(path = file))
    tmp.spct <- get(x = object.name)
    if (!is.any_spct(tmp.spct)) {
      next()
    }
    tmp.spct <- photobiology::trim_wl(tmp.spct, range = range)
    tmp.spct <- photobiology::smooth_spct(tmp.spct,
                                          strength = strength,
                                          method = "supsmu")
    spectra[[make.names(paste(name.root, gsub(".spct", "", object.name)))]] <- tmp.spct
  }
  photobiology::generic_mspct(spectra,
                              class = photobiology::shared_member_class(spectra)[1])
}

# collect_spct_files("Zomei", range = c(350, 1050), strength = 2)
#
# plot_spct_file("Zomei/IR950.spct.Rda", range = c(350, 1050), strength = 2)
