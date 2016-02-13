library(photobiology)
library(ooacquire)
library(ggspectra)
library(ggplot2)

load(file = "test_data.rda")
# replace values from saturated pixels with NA
spct_1.a <- trim_counts(spct_1)
spct_1.b <- skip_bad_pixs(spct_1.a)
# as we do not have a dark reading we use the filter 100 pixels as dark
# reference
test_1 <- fshift(spct_1.b, range = unlist(spct_1[c(1,100), "w.length"]))
plot(test_1)
# we convert the raw counts to counts per second
test_cps <- raw2cps(test_1)
plot(test_cps)
summary(test_cps)

ggplot(test_cps, aes(w.length, cps_1)) + geom_line() + ylim(0, 6.3e+5)
ggplot(test_cps, aes(w.length, cps_2)) + geom_line() + ylim(0, 6.3e+5)

ggplot(test_cps, aes(x = w.length)) +
  geom_line(aes(y = cps_1), color = "blue") +
  geom_line(aes(y = cps_2), color = "red")

# we merge the scans taken with different integration times
test_merge <- merge_cps(test_cps)
plot(test_merge)
summary(test_merge)

integrate_spct(test_cps)
integrate_spct(test_merge)
