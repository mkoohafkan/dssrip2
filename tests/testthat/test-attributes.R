test_that("attribute setting works", {

  # time series
  d = data.frame(
    datetime = seq(as.POSIXct("2021-01-01", tz = "Etc/GMT+0"),
      as.POSIXct("2021-01-05", tz = "Etc/GMT+0"), by = "1 day"),
    flow = c(10, 12, NA, 13, 10) * 1000
  )
  
  expect_error(dss_add_attributes(d, list()))

  dattr = list(type = "PER-AVER", units = "cfs")
  d2 = dss_add_attributes(d, dattr)
  expect_equal(dss_attributes(d2), dattr)
  expect_error(dss_add_attributes(d2, list(foo = "bar")))

  # paired data
  d3 = data.frame(flow = c(10, 12, 14, 13, 10) * 1000,
    stage = c(17.1, 17.3, 17.6, 17.4, 17.2))
  dattr3 = list(xtype = "UNT", ytype = "UNT",
    xunits = "cfs", yunits = "stage")

  expect_error(dss_add_attributes(d, list()))

  d4 = dss_add_attributes(d, dattr)
  expect_equal(dss_attributes(d2), dattr)
  expect_error(dss_add_attributes(d2, list(foo = "bar")))

})
