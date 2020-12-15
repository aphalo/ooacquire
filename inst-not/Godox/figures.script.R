library(r4photobiology)
library(ggplot2)
library(scales)
library(ggspectra)
library(magrittr)

my.files <- list.files(pattern = ".*Hoya.25A.*Rda")
for (f in my.files) load(f)

filter_mspct(l = list(Fire.CPL.spct,
                      Fire.CPL.rep2.spct,
                      Fire.CPL.rep3.spct,
                      Fire.CPL.rep4.spct)) %>%
  s_median() %>%
  na.omit() %>%
  clip_wl(range = c(350, 1000)) %>%
  smooth_spct(method = "supsmu", strength = 2) %>%
  ggplot() +
  geom_spct() +
  geom_hline(yintercept = c(1, 0), linetype = "dashed") +
  ylim(0,1) +
  labs(x = "Wavelength (nm)", y = "Total transmittance (/1)",
       title = "Firecrest CPL filter") -> Firecrest.CPL.fig

filter_mspct(l = list(Fire.ND.1.2.spct,
                      Fire.ND.1.2.rep1.spct,
                      Fire.ND.1.2.rep3.spct,
                      Fire.ND.1.2.rep4.spct)) %>%
  s_median() %>%
  na.omit() %>%
  clip_wl(range = c(350, 1000)) %>%
  smooth_spct(method = "supsmu", strength = 10) %>%
  ggplot() +
  geom_spct() +
  geom_hline(yintercept = c(1, 0), linetype = "dashed") +
  geom_hline(yintercept = 1/(2^4), linetype = "dashed", colour = "red") +
  ylim(0,0.2) +
  labs(x = "Wavelength (nm)", y = "Total transmittance (/1)",
       title = "Firecrest ND 1.2 filter") -> Firecrest.ND.1.2.detail.fig

filter_mspct(l = list(Fire.ND.1.2.spct,
                      Fire.ND.1.2.rep1.spct,
                      Fire.ND.1.2.rep3.spct,
                      Fire.ND.1.2.rep4.spct)) %>%
  s_median() %>%
  na.omit() %>%
  clip_wl(range = c(350, 1000)) %>%
  smooth_spct(method = "supsmu", strength = 2) %>%
  ggplot() +
  geom_hline(yintercept = c(1, 0), linetype = "dashed") +
  geom_hline(yintercept = 1/(2^4), linetype = "dashed", colour = "red") +
  geom_line() +
  ylim(0,1) +
  labs(x = "Wavelength (nm)", y = "Total transmittance (/1)",
       title = "Firecrest ND 1.2 filter") -> Firecrest.ND.1.2.fig

filter_mspct(l = list(Fire.UVIR.spct,
                      Fire.UVIR.rep2.spct,
                      Fire.UVIR.rep3.spct)) %>%
  s_median() %>%
  na.omit() %>%
  clip_wl(range = c(350, 1000)) %>%
  smooth_spct(method = "supsmu", strength = 2) %>%
  normalize() %>%
  ggplot() +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  geom_line() +
  stat_find_wls() +
  stat_find_wls(geom = "text", hjust = -0.2) +
  ylim(0,1) +
  labs(x = "Wavelength (nm)", y = "Total transmittance (/maximum)",
       title = "Firecrest UVIR filter") -> Firecrest.UVIR.fig

filter_mspct(l = list(Fire.UV400.spct,
                      Fire.UV400.rep2.spct,
                      Fire.UV400.rep3.spct,
                      Fire.UV400.rep4.spct)) %>%
  s_median() %>%
  na.omit() %>%
  clip_wl(range = c(350, 1000)) %>%
  smooth_spct(method = "supsmu", strength = 2) %>%
  normalize() %>%
  ggplot() +
  geom_hline(yintercept = c(0, 1), linetype = "dashed") +
  geom_line() +
  stat_find_wls() +
  stat_find_wls(geom = "text", hjust = -0.2) +
  ylim(0,1) +
  labs(x = "Wavelength (nm)", y = "Total transmittance (/maximum)",
       title = "Firecrest UV400 filter") -> Firecrest.UV400.fig

filter_mspct(l = list(Hoya.25A.spct,
                      Hoya.25A.bare.xenon.2.spct,
                      Hoya.25A.bare.xenon.spct)) %>%
  s_median() %>%
  na.omit() %>%
  clip_wl(range = c(350, 1000)) %>%
  smooth_spct(method = "supsmu", strength = 2) %>%
  normalize() %>%
  ggplot() +
  geom_hline(yintercept = c(1, 0), linetype = "dashed") +
  geom_line() +
  stat_find_wls() +
  stat_find_wls(geom = "text", hjust = -0.2) +
  ylim(0,1) +
  labs(x = "Wavelength (nm)", y = "Total transmittance (/maximum)",
       title = "Hoya 25A HMC filter") -> Hoya.25A.fig

filters.mspct$ND1.20_299 %>%
  trim_wl(range = c(350, 1000)) %>%
  ggplot() +
  geom_hline(yintercept = c(1, 0), linetype = "dashed") +
  geom_hline(yintercept = 1/(2^4), linetype = "dashed", colour = "red") +
  geom_line() +
  ylim(0,1) + expand_limits(x = c(350, 1000)) +
  labs(x = "Wavelength (nm)", y = "Internal transmittance",
       title = "Lee Filters ND1.20, \"gel\" No. 299") -> Lee.ND1.20.fig

filters.mspct$NG9 %>%
  trim_wl(range = c(350, 1000)) %>%
  ggplot() +
  geom_hline(yintercept = c(1, 0), linetype = "dashed") +
  geom_hline(yintercept = 1/(2^4), linetype = "dashed", colour = "red") +
  geom_line() +
  ylim(0,1) +
  labs(x = "Wavelength (nm)", y = "Internal transmittance",
       title = "Schott NG9 1 mm") -> Schott.NG9.1mm.fig

pdf(file = "ND.filters.figs.pdf", onefile = TRUE, width = 6, height = 4)
print(Schott.NG9.1mm.fig)
print(Lee.ND1.20.fig)
print(Firecrest.ND.1.2.fig)
dev.off()

svg(filename = "firecrest-nd120-fig.svg")
print(Firecrest.ND.1.2.fig)
dev.off()


