skip_if_no_dss()

test_that("timeseries read works", {
  on.exit(dss_close_all(), add = TRUE)
  exfile = system.file("extdata/example.dss", package = "dssrip2")
  path = "/BRANDYWINE CREEK/WILMINGTON, DE/FLOW/01JAN2012/1DAY/USGS/"
  # default behavior
  ts1 = dss_read(exfile, path)
  expect_s3_class(ts1, "data.frame")
  expect_identical(nrow(ts1), 24107L)
  expect_identical(names(ts1), c("datetime", "flow"))
  expect_identical(names(ts1), c("datetime", "flow"))
  expect_identical(attr(ts1, "dss_attributes")$units, "cfs")
  expect_identical(attr(ts1, "dss_attributes")$type, "PER-AVER")
  # cheeck full vs. not
  ts1_full = dss_read(exfile, path, FALSE)
  expect_s3_class(ts1_full, "data.frame")
  expect_identical(nrow(ts1_full), 274L)
  expect_identical(names(ts1), names(ts1_full))
  expect_identical(attr(ts1_full, "dss_attributes")$units,
    attr(ts1, "dss_attributes")$units)
  expect_identical(attr(ts1_full, "dss_attributes")$type,
    attr(ts1, "dss_attributes")$type)

  # test missing values and timestamps
  path2 = "/BRANDYWINE CREEK/WILMINGTON, DE/GAGE HEIGHT/01AUG2012/15MIN/USGS/"
  ts2 = dss_read(exfile, path2, TRUE)
  expect_s3_class(ts2[[1]], "POSIXct")
  expect_type(ts2[[2]], "double")
  expect_identical(nrow(ts2), 2881L)
  expect_identical(sum(is.na(ts2[[2]])), 9L)

  # irregular time series
  path3 = "/BRANDYWINE CREEK/WILMINGTON, DE/FLOW-ANNUAL PEAK//IR-CENTURY/USGS/"
  ts3 = dss_read(exfile, path3)
  expect_identical(nrow(ts3), 67L)
  expect_s3_class(ts3[[1]], "POSIXct")
  expect_identical(names(ts3), c("datetime", "flow-annual peak"))

})


test_that("paired data read works", {
  on.exit(dss_close_all(), add = TRUE)
  exfile = system.file("extdata/example.dss", package = "dssrip2")
  path = "/BRANDYWINE CREEK/WILMINGTON, DE/FLOW-STAGE///GENERATED DATA PAIRS/"
  pd = dss_read(exfile, path)
  expect_s3_class(pd, "data.frame")
  expect_type(pd[[1]], "double")
  expect_type(pd[[2]], "double")
  expect_identical(nrow(pd), 67L)
  expect_identical(names(pd), c("flow", "stage"))
  expect_identical(attr(pd, "dss_attributes"),
    list(xparameter = "FLOW", yparameter = "STAGE", xtype = "UNT",
      ytype = "UNT", xunits = "cfs", yunits = "feet"))

})


test_that("grid data read works", {
  skip("need to create grid example")
  on.exit(dss_close_all(), add = TRUE)
  exfile = system.file("extdata/example.dss", package = "dssrip2")
#  path = "/SHG/LCOLORADO/PRECIP/02JAN2020:1500/02JAN2020:1600/Ex15/"

 # gd = dss_read(f, path)
 # expect_s4_class(gd, "SpatRaster")

})
