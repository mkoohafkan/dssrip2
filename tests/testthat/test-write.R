test_that("time series writing works", {
  d = data.frame(
    datetime = seq(as.POSIXct("2021-01-01", tz = "etc/GMT+0"),
      as.POSIXct("2021-01-05", tz = "etc/GMT+0"), by = "1 day"),
    flow = c(10, 12, 14, 13, 10) * 1000
  )
  attributes = list("type" = "PER-AVER", units = "cfs")

  outpath = tempfile(fileext = ".dss")
  out_file = dss_file(outpath)
  out_pathname = "/Fake Creek/Fake Town/FLOW//1DAY/FAKE/"

  dss_write(d, out_file, out_pathname, attributes)

  expect_identical(d, dss_read(out_file, out_pathname),
    ignore_attr = c("dss.type", "dss.units"))
})
