#' Summarize spectral irradiance or fluence
#'
#' Compute irradiance or fluence by waveband and energy or photon ratios
#' between wavebands of interest to plants' and human visual responses to light.
#'
#' @param mspct A source_mspct, or a source_spct object containing spectral
#'    irradiance for one or more sources.
#' @param unit.out character One of "photon" or "energy".
#' @param q.scale.factor,e.scale.factor numeric Multiplicative factors used to rescale
#'   photon and energy irradiances.
#' @param attr2tb character Vector with one or more of "when.measured",
#'    "what.measured", "where.measured", "how.measured" and "comment".
#' @param summary.type character One of "plant", "plant-pbio" or "VIS".
#' @param digits integer The number of significant digits in the output in the
#'    range [1..22].
#'
#' @details Function \code{irrad_summary_table()} wraps functions from packages
#'   'photobiology', 'photobiologyWavebands', 'photobiologyPlants' and
#'   'photobiologyInOut' and returns a set of summaries with quantities
#'   frequently used in specific research and technical fields.
#'
#'    \code{summary.type = "plant"} returns the most commonly used irradiances
#'    and waveband ratios used in agricultutre and related research. Most of
#'    them expressed as fluxes of photons or photon ratios.
#'
#'    \code{summary.type = "plant-pbio"} adds some estimates related to
#'    photobiology of plants.
#'    .
#'    \code{summary.type = "VIS"} returns the some commonly used quantities
#'    related to illumination. Fluxes are expressed on an energy basis.
#'
#' @return A tibble with one row per spectrum and one column per summary
#'   quantity or attribute and a column with the names of the spectra.
#'
#' @export
#'
#' @seealso See the documentation for functions
#'   \code{\link[photobiology]{irrad}},
#'   \code{\link[photobiology]{q_ratio}}, \code{\link[photobiology]{e_ratio}},
#'   \code{\link[photobiology]{add_attr2tb}},
#'   \code{\link[photobiologyInOut]{spct_CRI}},
#'   \code{\link[photobiologyInOut]{spct_CCT}}
#'   and \code{\link[base]{signif}} which are called to build
#'   the summary table.
#'
#' @examples
#'
#' irrad_summary_table(sun.spct)
#' irrad_summary_table(sun.spct, attr2tb = c("what.measured", "where.measured"))
#' irrad_summary_table(sun.spct, summary.type = "plant", unit.out = "photon")
#' irrad_summary_table(sun.spct, summary.type = "PAR", unit.out = "photon")
#' # temporary kludge until fixed in photobiologyInOut
#' irrad_summary_table(sun.spct, summary.type = "VIS", unit.out = "energy")
#'
irrad_summary_table <-
  function(mspct,
           unit.out = getOption("photobiology.radiation.unit",
                                default = "energy"),
           q.scale.factor = 1e6,
           e.scale.factor = 1,
           attr2tb = "when.measured",
           summary.type = "plant",
           digits = 4L) {

    if (summary.type == "PAR") {
      message("'summary.type = \"PAR\"' is no longer supported. Using \"plant\"!")
      summary.type == "plant"
    }
    # handle also single spectra
    if (is.generic_spct(mspct)) {
      mspct <- generic_mspct(list(mspct), class = class(mspct)[1])
    }
    if (any(unname(sapply(mspct, getMultipleWl)) > 1)) {
      mspct <- subset2mspct(mspct)
    }

    if (grepl("plant|PAR", summary.type)) {
        q_irrad.tb <-
          photobiology::q_irrad(mspct,
                                scale.factor = q.scale.factor,
                                w.band = photobiologyWavebands::Plant_bands())
        PhR.tb <-
          photobiology::e_irrad(mspct,
                                scale.factor = e.scale.factor,
                                w.band = photobiologyWavebands::PhR())
        qe_ratio.tb <-
          photobiology::qe_ratio(mspct,
                                 w.band = photobiologyWavebands::PAR(),
                                 scale.factor = q.scale.factor/e.scale.factor)
        uv_ratios.tb <-
          q_ratio(mspct,
                  w.band.num = photobiologyWavebands::UV_bands(),
                  w.band.denom = photobiologyWavebands::PAR())
        vis_ratios.tb <-
          q_ratio(mspct,
                  w.band.num =
                    list(blue = photobiologyWavebands::Blue("Sellaro"),
                         red = photobiologyWavebands::Red("Smith20"),
                         red_slr = photobiologyWavebands::Red("Sellaro")),
                  w.band.denom =
                    list(green = photobiologyWavebands::Green("Sellaro"),
                         "far-red" = photobiologyWavebands::Far_red("Smith20"),
                         "far-red_slr" = photobiologyWavebands::Far_red("Sellaro")))

        summary.tb <-
          dplyr::full_join(q_irrad(mspct,
                                   w.band = NULL,
                                   scale.factor = q.scale.factor),
                           photobiologyPlants::xPAR_irrad(mspct,
                                                          scale.factor = q.scale.factor),
                           by = "spct.idx")
        summary.tb <-
          dplyr::full_join(summary.tb,
                           PhR.tb,
                           by = "spct.idx")
        summary.tb <-
          dplyr::full_join(summary.tb, qe_ratio.tb, by = "spct.idx")
        summary.tb <-
          dplyr::full_join(summary.tb, q_irrad.tb, by = "spct.idx")
        summary.tb <-
          dplyr::full_join(summary.tb, uv_ratios.tb, by = "spct.idx")
        summary.tb <-
          dplyr::full_join(summary.tb, vis_ratios.tb, by = "spct.idx")
        if (grepl("-pbio$", summary.type)) {
          summary.tb <-
            dplyr::full_join(summary.tb,
                             photobiologyPlants::Pfr_Ptot(mspct),
                             by = "spct.idx")
          summary.tb <-
            dplyr::full_join(summary.tb,
                             photobiologyPlants::Phy_photoconversion(mspct, target = NULL),
                             by = "spct.idx") |>
            dplyr::rename(nu_Qreal = "nu")
          summary.tb <-
            dplyr::full_join(summary.tb,
                             photobiologyPlants::Phy_photoconversion(mspct, target = 100e-6),
                             by = "spct.idx") |>
            dplyr::rename(nu_Q100 = "nu")
          summary.tb <-
            dplyr::full_join(summary.tb,
                             photobiologyPlants::PQ_redox_state(mspct),
                             by = "spct.idx")
        }

    } else if (summary.type == "VIS") {
      summary.tb <-
        dplyr::full_join(photobiology::illuminance(mspct,
                                                   allow.scaled = TRUE,
                                                   scale.factor = 1e-3), # klux
                         photobiology::irrad(mspct,
                            unit.out = unit.out,
                            scale.factor = ifelse(unit.out == "energy",
                                                  e.scale.factor, q.scale.factor),
                            w.band = c(list(photobiologyWavebands::VIS()),
                                       photobiologyWavebands::VIS_bands())),
                         by = "spct.idx")
      summary.tb[["CRI"]] <- sapply(mspct,
                                    photobiologyInOut::spct_CRI,
                                    tol = 0.0054,
                                    named = FALSE)
      summary.tb[["CCT"]] <- sapply(mspct,
                                    photobiologyInOut::spct_CCT,
                                    strict = TRUE,
                                    named = FALSE)

    } else { # total
      summary.tb <-
        photobiology::irrad(mspct,
                            unit.out = unit.out,
                            scale.factor = ifelse(unit.out == "energy",
                                                  e.scale.factor, q.scale.factor),
                            w.band = NULL)
      summary.tb <-
        dplyr::full_join(summary.tb,
                         irrad(mspct,
                               unit.out = unit.out,
                               scale.factor = ifelse(unit.out == "energy",
                                                     e.scale.factor, q.scale.factor),
                               w.band = split_bands(x = c(300, 400, 500, 600, 700, 800),
                                                    list.names = paste((3:7)*100, (4:8)*100,
                                                                       sep = "."))),
                         by = "spct.idx")
    }

    # add attributes
    summary.tb <- photobiology::add_attr2tb(tb = summary.tb,
                                            mspct = mspct,
                                            col.names = attr2tb,
                                            idx = "spct.idx",
                                            unnest = FALSE)

    if (!is.null(digits)) {
      selector <- unname(sapply(summary.tb, is.numeric))
      summary.tb[ , selector] <- signif(summary.tb[ , selector], digits = digits)
    }

    summary.tb
  }
