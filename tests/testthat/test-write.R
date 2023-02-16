skip_if_no_dss()

test_that("time series writing works", {
  d = data.frame(
    datetime = seq(as.POSIXct("2021-01-01", tz = "etc/GMT+0"),
      as.POSIXct("2021-01-05", tz = "etc/GMT+0"), by = "1 day"),
    flow = c(10, 12, NA, 13, 10) * 1000
  )
  attributes = list("type" = "PER-AVER", units = "cfs")

  outpath = tempfile(fileext = ".dss")
  out_file = dss_create(outpath)
  on.exit(out_file$done(), add = TRUE)
  out_pathname = "/Fake Creek/Fake Town/FLOW//1DAY/FAKE/"

  dss_write(d, out_file, out_pathname, attributes)

  expect_identical(d, dss_read(out_file, out_pathname),
    ignore_attr = c("dss.type", "dss.units"))

})


test_that("paired data writing works", {
  d = data.frame(flow = c(10, 12, 14, 13, 10) * 1000,
    stage = c(17.1, 17.3, 17.6, 17.4, 17.2))
  attributes = list(xunits = "cfs", yunits = "stage",
    xtype = "UNT", ytype = "UNT")

  outpath = tempfile(fileext = ".dss")
  out_file = dss_create(outpath)
  on.exit(out_file$done(), add = TRUE)
  out_pathname = "/Fake Creek/Fake Town/FLOW-STAGE///FAKE/"

  dss_write(d, out_file, out_pathname, attributes)

  expect_equal(d, dss_read(out_file, out_pathname),
    tolerance = 1e-5, ignore_attr = c("dss.xtype", "dss.ytype",
      "dss.xunits", "dss.yunits"))

})


test_that("grid data writing works", {
  skip("not implemented")
})
