skip_if_no_dss()

test_that("time series writing works", {
  d = data.frame(
    datetime = seq(as.POSIXct("2021-01-01", tz = "etc/GMT+0"),
      as.POSIXct("2021-01-05", tz = "etc/GMT+0"), by = "1 day"),
    flow = c(10, 12, NA, 13, 10) * 1000
  )
  attributes = list("type" = "PER-AVER", units = "cfs")

  tf = tempfile(fileext = ".dss")
  conn = dss_create(tf)
  on.exit(conn$done(), add = TRUE)
  path = "/Fake Creek/Fake Town/FLOW//1DAY/FAKE/"

  dss_write(d, conn, path, attributes)

  expect_identical(d, dss_read(conn, path),
    ignore_attr = c("dss.type", "dss.units"))

})


test_that("paired data writing works", {
  d = data.frame(flow = c(10, 12, 14, 13, 10) * 1000,
    stage = c(17.1, 17.3, 17.6, 17.4, 17.2))
  attributes = list(xunits = "cfs", yunits = "stage",
    xtype = "UNT", ytype = "UNT")

  tf = tempfile(fileext = ".dss")
  conn = dss_create(tf)
  on.exit(conn$done(), add = TRUE)
  path = "/Fake Creek/Fake Town/FLOW-STAGE///FAKE/"
  dss_write(d, conn, path, attributes)

  expect_equal(d, dss_read(conn, path),
    tolerance = 1e-5, ignore_attr = c("dss.xtype", "dss.ytype",
      "dss.xunits", "dss.yunits"))

  # test labels and NA values
  d2 = data.frame(flow = c(10, 12, NA, 13, 15) * 1000,
    foo = c(17.1, 17.2, 17.3, 17.4, 17.6),
    bar = c(13.7, 13.8, 13.8, 13.9, 14.1),
    baz = c(10.36, 10.3, 10.4, 10.4, 10.))
  attributes2 = list(xunits = "cfs", yunits = "feet",
    xtype = "UNT", ytype = "UNT", labels = names(d2[2:4]))
  path2 = "/Fake Creek/Fake Town/FLOW-STAGE///FAKE2/"
  dss_write(d2, conn, path2, attributes2)

})


test_that("grid data writing works", {
  skip("not implemented")
})


test_that("deleting records works", {

  skip("not implemented")

  d = data.frame(
    datetime = seq(as.POSIXct("2021-01-01", tz = "etc/GMT+0"),
      as.POSIXct("2021-01-05", tz = "etc/GMT+0"), by = "1 day"),
    flow = c(10, 12, NA, 13, 10) * 1000
  )
  attributes = list("type" = "PER-AVER", units = "cfs")

  tf = tempfile(fileext = ".dss")
  conn = dss_create(tf)
  on.exit(conn$done(), add = TRUE)
  path = "/Fake Creek/Fake Town/FLOW//1DAY/FAKE/"

  dss_write(d, conn, path, attributes)
  dss_delete(conn, path)

})
