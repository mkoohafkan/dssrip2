skip_if_no_dss()

test_that("DSS file open works", {
  on.exit(conn$close(), add = TRUE)
  conn = dss_open(system.file("extdata/example.dss", package = "dssrip2"))
  expect_s4_class(conn, "jobjRef")
})


test_that("DSS file creation works", {
  on.exit(conn$close(), add = TRUE)
  tf1 = tempfile(fileext = ".dss")
  tf2 = tempfile(fileext = ".dss")
  tf3 = tempfile(fileext = ".dss")
  tf4 = tempfile(fileext = ".dss")
  tf5 = tempfile(fileext = ".dss")
  on.exit(unlink(c(tf1, tf2, tf3, tf4, tf5)), add = TRUE)

  f1 = dss_create(tf1)
  f2 = dss_create(tf2, version = 6L)
  f3 = dss_create(tf3, version = 7L)

  expect_s4_class(f1, "jobjRef")
  expect_s4_class(f2, "jobjRef")
  expect_identical(dss_version(f1), 7L)
  expect_identical(dss_version(f2), 6L)
  expect_identical(dss_version(f3), 7L)
  expect_error(dss_create(tf, version = 5L))

  dss_convert(f1, tf4)
  dss_convert(f2, tf5)
  f4 = dss_open(tf4)
  f5 = dss_open(tf5)

  expect_identical(dss_version(f4), 6L)
  expect_identical(dss_version(f5), 7L)

  dss_close(f1)
  dss_close(f2)
  dss_close(f3)
  dss_close(f4)
  dss_close(f5)

})
