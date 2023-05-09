skip_if_no_dss()


test_that("File path normalization works", {

  td = normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  cwd = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  
  tf1 = paste0(td, "\\file.txt")
  tf2 = paste0(toupper(td), "/file.txt")
  tf3 = "file.txt"
  tf4 = "./file.txt"
  tf5 = "/file.txt"

  vf1 = file.path(td, "file.txt", fsep = "/")
  vf2 = file.path(cwd, "file.txt", fsep = "/")
  vf3 = normalizePath(file.path(normalizePath("/", mustWork = TRUE),
    "file.txt"), winslash = "/", mustWork = FALSE)

  expect_error(normalize_path(tf1, TRUE))

  expect_identical(normalize_path(tf1, FALSE), vf1)
  expect_identical(normalize_path(tf2, FALSE), vf1)
  expect_identical(normalize_path(tf3, FALSE), vf2)
  expect_identical(normalize_path(tf4, FALSE), vf2)
  expect_identical(normalize_path(tf5, FALSE), vf3)

  file.create(tf2)
  on.exit(unlink(c(tf2)))
  expect_identical(normalize_path(tf2, TRUE), vf1)

  tf6 = "Q:\\foo/bar\\baz.txt"
  vf4 = "Q:/foo/bar/baz.txt"

  expect_identical(normalize_path(tf6, FALSE), vf4)
  expect_identical(normalize_path(toupper(tf6), FALSE), toupper(vf4))

})


test_that("DSS file creation works", {
  tf1 = tempfile(fileext = ".dss")
  tf2 = tempfile(fileext = ".dss")
  tf3 = tempfile(fileext = ".dss")
  on.exit(unlink(c(tf1, tf2, tf3)), add = TRUE)
  on.exit(dss_close_all(), add = TRUE)

  expect_true(dss_create(tf1))
  expect_setequal(.store$list(), normalize_path(tf1, TRUE))

  expect_true(dss_create(tf2, version = 6L))

  expect_error(dss_create(tf3, version = 5L))
  expect_setequal(.store$list(), normalize_path(c(tf1, tf2), TRUE))

  expect_true(dss_create(tf3, version = 7L))
  expect_setequal(.store$list(), normalize_path(c(tf1, tf2, tf3),
    TRUE))

  expect_s4_class(dss_file(tf1), "jobjRef")
  expect_setequal(.store$list(), normalize_path(c(tf1, tf2, tf3),
    TRUE))

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

  expect_setequal(.store$list(), normalize_path(c(tf1, tf2, tf3, tf4),
    TRUE))
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
