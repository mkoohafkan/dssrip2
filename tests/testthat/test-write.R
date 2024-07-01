skip_if_no_dss()

test_that("time series writing works", {
  on.exit(dss_close_all(), add = TRUE)
  tf = tempfile(fileext = ".dss")
  dss_create(tf)

  d = data.frame(
    datetime = seq(as.POSIXct("2021-01-01", tz = "Etc/GMT+0"),
      as.POSIXct("2021-01-05", tz = "Etc/GMT+0"), by = "1 day"),
    flow = c(10, 12, NA, 13, 10) * 1000
  )
  attr(d, "dss_attributes") = list(type = "PER-AVER", units = "cfs")
  path = "/Fake Creek/Fake Town/FLOW//1DAY/FAKE/"

  # error when file does not exist
  expect_error(dss_write(d, tempfile(fileext = ".dss"), path1))
  
  expect_true(dss_write(d, tf, path))
  expect_identical(dss_read(tf, path), d)

  d2 = d
  attr(d2, "dss_attributes") = NULL
  expect_error(dss_write(d2, tf, path))

  # test that writing tibbles works
  if (requireNamespace("tibble")) {
    path2 = "/Fake Creek/Fake Town/FLOW//1DAY/FAKE2/"
    expect_true(dss_write(tibble::as_tibble(d), tf, path2))
    expect_identical(dss_read(tf, path2), d)
  }


  # unit test: catch unordered timeseries (submitted by Rachael Marzion)
  d3 = data.frame(datetime = as.POSIXct(c(1230811200, 1230813000,
        1230771600, 1230773400, 1230775200, 1230777000), tz = "GMT"),
    elev = c(106.9, 106.95, 107.05, 107.1, 107.15, 107.25))
  attr(d3, "dss_attributes") = list(type = "INST-VAL", units = "ft")
  path3 = "/DCR Ops Data/Boston, MA/ELEV//30MIN/Basin Elev (ft MDC)/"
  expect_error(dss_write(d3, tf, path3))

})


test_that("paired data writing works", {
  on.exit(dss_close_all(), add = TRUE)
  tf = tempfile(fileext = ".dss")
  dss_create(tf)

  d = data.frame(flow = c(10, 12, 14, 13, 10) * 1000,
    stage = c(17.1, 17.3, 17.6, 17.4, 17.2))
  attr(d, "dss_attributes") = list(xtype = "UNT", ytype = "UNT",
    xunits = "cfs", yunits = "stage")
  path = "/Fake Creek/Fake Town/FLOW-STAGE///FAKE/"

  expect_true(dss_write(d, tf, path))
  expect_equal(dss_read(tf, path), d, tolerance = 1e-5,
    ignore_attr = "dss_attributes")
  expect_equal(attr(dss_read(tf, path), "dss_attributes")[-c(1, 2)],
    attr(d, "dss_attributes"))


  # test labels and NA values
  d2 = data.frame(flow = c(10, 12, NA, 13, 15) * 1000,
    foo = c(17.1, 17.2, 17.3, 17.4, 17.6),
    bar = c(13.7, 13.8, 13.8, 13.9, 14.1),
    baz = c(10.36, 10.3, 10.4, 10.4, 10.0))
  attr(d2, "dss_attributes") = list(xunits = "cfs", yunits = "feet",
    xtype = "UNT", ytype = "UNT")
  path2 = "/Fake Creek/Fake Town/FLOW-STAGE///FAKE2/"

  expect_true(dss_write(d2, tf, path2))
  expect_equal(dss_read(tf, path2), d2, tolerance = 1e-5,
    ignore_attr = "dss_attributes")

  d3 = d2
  attr(d3, "dss_attributes") = NULL
  expect_error(dss_write(d3, tf, path2))

  # test that writing tibbles works
  if (requireNamespace("tibble")) {
    path3 = "/Fake Creek/Fake Town/FLOW-STAGE///FAKE3/"
    expect_true(dss_write(tibble::as_tibble(d), tf, path))
    expect_equal(dss_read(tf, path), d, tolerance = 1e-5,
      ignore_attr = "dss_attributes")
    expect_equal(attr(dss_read(tf, path), "dss_attributes")[-c(1, 2)],
      attr(d, "dss_attributes"))
  }

})


test_that("grid data writing works", {
  skip("not implemented")
})


test_that("deleting records works", {
  on.exit(dss_close_all(), add = TRUE)
  d = data.frame(
    datetime = seq(as.POSIXct("2021-01-01", tz = "Etc/GMT+0"),
      as.POSIXct("2021-01-05", tz = "Etc/GMT+0"), by = "1 day"),
    flow = c(10, 12, NA, 13, 10) * 1000
  )
  attr(d, "dss_attributes") = list("type" = "PER-AVER", units = "cfs")

  tf = tempfile(fileext = ".dss")
  dss_create(tf)

  path1 = "/Fake Creek/Fake Town/FLOW//1DAY/FAKE/"
  path2 = "/Another Fake Creek/Fake Town/FLOW//1DAY/FAKE/"

  dss_write(d, tf, path1)
  dss_write(d, tf, path2)

  # error when supplied path does not exist
  expect_error(dss_delete(tf, "/Fake Creek/Fake Town/FLOW//1DAY/NOT EXIST/",
    full = TRUE))

  delete_path2 = "/ANOTHER FAKE CREEK/FAKE TOWN/FLOW/01JAN2020/1DAY/FAKE/"
  expect_true(dss_delete(tf, path1, full = TRUE))
  expect_true(dss_delete(tf, delete_path2, full = FALSE))

  expect_identical(dss_catalog(tf, path1, condensed = TRUE),
    character(0))
  expect_identical(dss_catalog(tf, delete_path2, condensed = FALSE),
    character(0))
  expect_identical(dss_catalog(tf, dss_parts_replace(path2,
    list(D = ".*")), condensed = FALSE),
    "/ANOTHER FAKE CREEK/FAKE TOWN/FLOW/01JAN2021/1DAY/FAKE/")

})
