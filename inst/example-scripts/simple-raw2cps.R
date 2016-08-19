library(photobiology)
library(photobiologyWavebands)
library(ooacquire)
library(ggspectra)
library(ggplot2)

load(file = "test_data.rda")
# replace values from saturated pixels with NA
spct_1.a <- trim_counts(spct_1)
spct_1.b <- bleed_nas(spct_1.a)
spct_1.c <- skip_bad_pixs(spct_1.b)
spct_1.d <- linearize_counts(spct_1.c)
# as we do not have a dark reading we use the average of the first 100 pixels as
# dark reference
test_1 <- fshift(spct_1.d, range = unlist(spct_1.d[c(1,100), "w.length"]))
plot(test_1)
# we convert the raw counts to counts per second
test_cps <- raw2cps(test_1)
plot(test_cps)
plot(test_cps, range = c(605, 620)) + geom_point()
summary(test_cps)

ggplot(test_cps, aes(w.length, cps_1)) + geom_line() + ylim(0, 6.3e+5)
ggplot(test_cps, aes(w.length, cps_2)) + geom_line() + ylim(0, 6.3e+5)

fig_splice <- ggplot(test_cps, aes(x = w.length)) +
  geom_line(aes(y = cps_1), color = "blue") +
  geom_line(aes(y = cps_2), color = "red")

fig_splice

fig_splice + scale_y_log10()

fig_splice + scale_y_log10(limits = c(1e3, 1e6)) + xlim(500, 700)

# we merge the scans taken with different integration times
test_merge <- merge_cps(test_cps)
plot(test_merge)
plot(test_merge, range = c(605, 620)) + geom_point()
summary(test_merge)

# we apply the "tail" correction for the slit function
corr_cps <- slit_function_correction(test_merge)
plot(corr_cps)
ggplot(test_merge, range = c(605, 620)) + geom_line() +
  geom_line(data = corr_cps, color = "red")

average_spct(test_cps)
average_spct(test_merge)
average_spct(corr_cps)

average_spct(clip_wl(test_cps, UVB()))
average_spct(clip_wl(test_merge, UVB()))
average_spct(clip_wl(corr_cps, UVB()))
