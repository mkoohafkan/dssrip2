skip_if_no_dss()

test_that("timeseries read works", {
  on.exit(f$done(), add = TRUE)
  f = dss_open(system.file("extdata/example.dss", package = "dssrip2"))
  path = "/BRANDYWINE CREEK/WILMINGTON, DE/FLOW/01JAN2012/1DAY/USGS/"
  # default behavior
  ts1 = dss_read(f, path)
  expect_s3_class(ts1, "data.frame")
  expect_identical(nrow(ts1), 24107L)
  expect_identical(names(ts1), c("datetime", "flow"))
  expect_identical(names(ts1), c("datetime", "flow"))
  expect_identical(attr(ts1, "dss.units"), "cfs")
  expect_identical(attr(ts1, "dss.type"), "PER-AVER")
  # cheeck full vs. not
  ts2 = dss_read(f, path, FALSE)
  expect_s3_class(ts2, "data.frame")
  expect_identical(nrow(ts2), 274L)
  expect_identical(names(ts1), names(ts2))
  expect_identical(attr(ts2, "dss.units"), attr(ts1, "dss.units"))
  expect_identical(attr(ts2, "dss.type"), attr(ts1, "dss.type"))

  path2 = "/BRANDYWINE CREEK/WILMINGTON, DE/GAGE HEIGHT/01AUG2012/15MIN/USGS/"
  ts3 = dss_read(f, path2, TRUE)
  



})


test_that("paired data read works", {
  on.exit(f$done(), add = TRUE)
  f = dss_open(system.file("extdata/example.dss", package = "dssrip2"))
  path = "/BRANDYWINE CREEK/WILMINGTON, DE/FLOW-STAGE///GENERATED DATA PAIRS/"
  pd = dss_read(f, path)
  expect_s3_class(pd, "data.frame")

})


test_that("grid data read works", {
  skip("need to create grid example")
  on.exit(f$done(), add = TRUE)
  f = dss_open(system.file("extdata/example.dss", package = "dssrip2"))
#  path = "/SHG/LCOLORADO/PRECIP/02JAN2020:1500/02JAN2020:1600/Ex15/"

 # gd = dss_read(f, path)
 # expect_s4_class(gd, "SpatRaster")

})
