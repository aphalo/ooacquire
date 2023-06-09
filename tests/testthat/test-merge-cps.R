context("read raw Ocean Optics")

test_that("merge_cps works with good data", {

  load("./test-irrad-mspct-maya-data/sun_hdr4.spct.rda")

  expect_no_error(raw2corr_cps(good_hdr.raw_spct, ref.pixs.range = 2:4))
  expect_no_warning(raw2corr_cps(good_hdr.raw_spct, ref.pixs.range = 2:4))
  expect_no_message(raw2corr_cps(good_hdr.raw_spct, ref.pixs.range = 2:4))
  good.cps_spct <- raw2corr_cps(good_hdr.raw_spct, ref.pixs.range = 2:4)
  expect_equal(max(good.cps_spct$cps), 356812.435993)

})


test_that("merge_cps works with bad data", {

  load("./test-irrad-mspct-maya-data/sun_hdr4.spct.rda")

  expect_no_error(raw2corr_cps(bad_hdr.raw_spct, ref.pixs.range = 2:4))
  expect_no_warning(raw2corr_cps(bad_hdr.raw_spct, ref.pixs.range = 2:4))
  expect_message(raw2corr_cps(bad_hdr.raw_spct, ref.pixs.range = 2:4))
  bad.cps_spct <- raw2corr_cps(bad_hdr.raw_spct, ref.pixs.range = 2:4)

  expect_equal(max(bad.cps_spct$cps), 331080.293495)

})
