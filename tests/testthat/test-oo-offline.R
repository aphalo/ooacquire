context("read raw Ocean Optics")

library(photobiology)
library(lubridate)
library(readr)
library(tibble)

test_that("SpectraSuite", {

  ss.spct <- read_oo_ssdata(file = "data-test/pheno1normal.txt",
                            tz = "EET")

  expect_equal(nrow(ss.spct), 2068)
  expect_equal(ncol(ss.spct), 2)
  expect_equal(ss.spct[1, 1], 187.82)
  expect_equal(ss.spct[2068, 1], 1117.14)
  expect_is(ss.spct[[1]], "numeric")
  expect_equal(sum(is.na(ss.spct[[1]])), 0)
  expect_true(all(sign(ss.spct[[1]]) > 0))
  expect_is(ss.spct[[2]], "numeric")
  expect_equal(sum(is.na(ss.spct[[2]])), 0)
  expect_is(ss.spct, "raw_spct")
  expect_named(ss.spct, c("w.length", "counts"))
  expect_equal(as.numeric(getWhenMeasured(ss.spct), tz = "EET"),
               as.numeric(ymd_hms("2016-10-11 11:23:05", tz = "UTC"), tz = "EET"))
  expect_equal(getWhereMeasured(ss.spct),
               tibble(lon = NA_real_, lat = NA_real_, address = NA_character_))
  expect_equal(getWhatMeasured(ss.spct), "File: pheno1normal.txt")
  expect_equal(getTimeUnit(ss.spct), "unknown")
  expect_gt(length(comment(ss.spct)), 0)
})


test_that("SpectraSuite comma", {

  my.locale <- readr::locale("en", decimal_mark = ",", tz = "EET")

  ss.spct <- read_oo_ssdata(file = "data-test/pheno1normal.txt",
                            locale = my.locale)

  expect_equal(nrow(ss.spct), 2068)
  expect_equal(ncol(ss.spct), 2)
  expect_equal(ss.spct[1, 1], 187.82)
  expect_equal(ss.spct[2068, 1], 1117.14)
  expect_is(ss.spct[[1]], "numeric")
  expect_equal(sum(is.na(ss.spct[[1]])), 0)
  expect_true(all(sign(ss.spct[[1]]) > 0))
  expect_is(ss.spct[[2]], "numeric")
  expect_equal(sum(is.na(ss.spct[[2]])), 0)
  expect_is(ss.spct, "raw_spct")
  expect_named(ss.spct, c("w.length", "counts"))
  expect_equivalent(getWhenMeasured(ss.spct),
                    ymd_hms("2016-10-11 11:23:05", tz = "UTC"))
  expect_equal(getWhereMeasured(ss.spct),
               tibble(lon = NA_real_, lat = NA_real_, address = NA_character_))
  expect_equal(getWhatMeasured(ss.spct), "File: pheno1normal.txt")
  expect_equal(getTimeUnit(ss.spct), "unknown")
  expect_gt(length(comment(ss.spct)), 0)
})

test_that("SpectraSuite comma multiple", {

  my.locale <- readr::locale("en", decimal_mark = ",", tz = "EET")

  ss.mspct <- read_files2mspct(files = list(
    light = "data-test/pheno1normal.txt",
    dark = "data-test/pheno1normaldark.txt"
  ),
  locale = my.locale
  )

  expect_equal(length(ss.mspct), 2)
  expect_equivalent(names(ss.mspct), c("light", "dark"))
  expect_equal(ss.mspct[["light"]][1, 1], 187.82)
  expect_equal(ss.mspct[["dark"]][1, 1], 187.82)
  expect_equal(ss.mspct[["light"]][2068, 1], 1117.14)
  expect_equal(ss.mspct[["dark"]][2068, 1], 1117.14)
  expect_is(ss.mspct[["light"]][[1]], "numeric")
  expect_equal(sum(is.na(ss.mspct[["light"]][[1]])), 0)
  expect_true(all(sign(ss.mspct[["light"]][[1]]) > 0))
  expect_is(ss.mspct[["light"]][[2]], "numeric")
  expect_equal(sum(is.na(ss.mspct[["light"]][[2]])), 0)
  expect_is(ss.mspct, "raw_mspct")
  expect_is(ss.mspct[["light"]], "raw_spct")
  expect_is(ss.mspct[["dark"]], "raw_spct")
  expect_named(ss.mspct[["light"]], c("w.length", "counts"))
  expect_equivalent(getWhenMeasured(ss.mspct[["light"]]),
                    ymd_hms("2016-10-11 11:23:05", tz = "UTC"))
  expect_equivalent(getWhenMeasured(ss.mspct[["dark"]]),
                    ymd_hms("2016-10-11 11:23:53", tz = "UTC"))
  expect_equal(getWhereMeasured(ss.mspct[["light"]]),
               tibble(lon = NA_real_, lat = NA_real_, address = NA_character_))
  expect_equal(getWhatMeasured(ss.mspct[["light"]]), "File: pheno1normal.txt")
  expect_equal(getTimeUnit(ss.mspct[["light"]]),  "unknown")
  expect_gt(length(comment(ss.mspct[["light"]])), 0)
})
