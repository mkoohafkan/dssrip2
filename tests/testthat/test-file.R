skip_if_no_dss()

test_that("DSS file creation works", {
  tf1 = tempfile(fileext = ".dss")
  tf2 = tempfile(fileext = ".dss")
  tf3 = tempfile(fileext = ".dss")
  on.exit(unlink(c(tf1, tf2, tf3)), add = TRUE)
  on.exit(dss_close_all(), add = TRUE)

  expect_true(dss_create(tf1))
  expect_true(dss_create(tf2, version = 6L))
  expect_error(dss_create(tf3, version = 5L))
  expect_true(dss_create(tf3, version = 7L))

  expect_setequal(normalizePath(c(tf1, tf2, tf3)),
    normalizePath(.store$list()))

})


test_that("DSS file close works", {
  tf1 = tempfile(fileext = ".dss")
  tf2 = tempfile(fileext = ".dss")
  tf3 = tempfile(fileext = ".dss")
  tf4 = tempfile(fileext = ".dss")
  on.exit(unlink(c(tf1, tf2, tf3, tf4)), add = TRUE)

  dss_create(tf1)
  dss_create(tf2, version = 6L)
  dss_create(tf3, version = 7L)
  dss_create(tf4, version = 7L)

  expect_true(dss_close(tf1))
  expect_setequal(.store$list(), normalize_path(c(tf2, tf3, tf4),
    TRUE))

  expect_true(dss_close_all())
  expect_identical(.store$list(), character())

})



test_that("DSS file version detection works", {
  tf1 = tempfile(fileext = ".dss")
  tf2 = tempfile(fileext = ".dss")
  tf3 = tempfile(fileext = ".dss")
  on.exit(unlink(c(tf1, tf2, tf3)), add = TRUE)
  on.exit(dss_close_all(), add = TRUE)

  dss_create(tf1)
  dss_create(tf2, version = 6L)
  dss_create(tf3, version = 7L)

  expect_identical(dss_version(tf1), 7L)
  expect_identical(dss_version(tf2), 6L)
  expect_identical(dss_version(tf3), 7L)

})


test_that("DSS file version conversion works", {
  tf1 = tempfile(fileext = ".dss")
  tf2 = tempfile(fileext = ".dss")
  tf3 = tempfile(fileext = ".dss")
  tf4 = tempfile(fileext = ".dss")
  on.exit(unlink(c(tf1, tf2, tf3, tf4)), add = TRUE)
  on.exit(dss_close_all(), add = TRUE)

  expect_true(dss_create(tf1, version = 7L))
  expect_true(dss_create(tf2, version = 6L))

  dss_convert(tf1, tf3)
  dss_convert(tf2, tf4)

  expect_identical(dss_version(tf3), 6L)
  expect_identical(dss_version(tf4), 7L)

})
