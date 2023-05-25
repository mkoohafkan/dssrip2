test_that("DSS catalog cache works", {

  skip_if_no_dss()

  tf1 = tempfile(fileext = ".dss")
  tf2 = tempfile(fileext = ".dss")
  tf3 = tempfile(fileext = ".dss")
  tf4 = tempfile(fileext = ".dss")
  on.exit(unlink(c(tf1, tf2, tf3, tf4)), add = TRUE)

  exfile = system.file("extdata/example.dss", package = "dssrip2")
  file.copy(exfile, c(tf1, tf2, tf3, tf4))


  dss_catalog(tf1, condensed = FALSE)
  dss_catalog(tf2)
  dss_catalog(tf3, condensed = FALSE)
  dss_catalog(tf3, condensed = TRUE)
  dss_catalog(tf4, condensed = TRUE)

  expect_setequal(.store$list_catalog(TRUE),
    normalize_path(c(tf2, tf3, tf4), TRUE))
  expect_setequal(.store$list_catalog(FALSE),
    normalize_path(c(tf1, tf3), TRUE))
  
  dss_close(tf3)
  expect_setequal(.store$list_catalog(TRUE),
    normalize_path(c(tf2, tf4), TRUE))
  expect_setequal(.store$list_catalog(FALSE),
    normalize_path(c(tf1), TRUE))
  dss_close(tf4)
  expect_setequal(.store$list_catalog(TRUE),
    normalize_path(c(tf2), TRUE))
  expect_setequal(.store$list_catalog(FALSE),
    normalize_path(c(tf1), TRUE))
  dss_close_all()
  expect_identical(.store$list_catalog(TRUE), character())
  expect_identical(.store$list_catalog(FALSE), character())

})


test_that("path list works", {
  
  skip_if_no_dss()

  on.exit(dss_close_all(), add = TRUE)
  
  exfile = system.file("extdata/example.dss", package = "dssrip2")

  all_paths = dss_catalog(exfile, pattern = "*", condensed = FALSE)
  all_paths_c = dss_catalog(exfile, pattern = "*", condensed = TRUE)
  paths1 = dss_catalog(exfile, pattern = "^/BRANDYWINE CREEK/.*/$",
    condensed = FALSE)
  paths1_c = dss_catalog(exfile, pattern = "^/BRANDYWINE CREEK/.*/$",
    condensed = TRUE)
  paths2 = dss_catalog(exfile, pattern = "^/.*/.*/.*/.*/IR-CENTURY/.*$",
    condensed = FALSE)
  paths2_c = dss_catalog(exfile, pattern = "^/.*/.*/.*/.*/IR-CENTURY/.*$",
    condensed = TRUE)

  expect_identical(length(all_paths), 74L)
  expect_identical(length(all_paths_c), 5L)

  expect_identical(length(paths1), 74L)
  expect_identical(length(paths1_c), 5L)
  expect_identical(all(paths1 %in% all_paths), TRUE)
  expect_identical(all(paths1 %in% all_paths_c), FALSE)
  expect_identical(all(paths1_c %in% all_paths_c), TRUE)

  expect_identical(length(paths2), 4L)
  expect_identical(length(paths2_c), 2L)
  expect_identical(all(paths2 %in% all_paths), TRUE)
  expect_identical(all(paths2 %in% all_paths_c), FALSE)
  expect_identical(all(paths2_c %in% all_paths_c), TRUE)

})


test_that("path parts works", {

  path = "/BRANDYWINE CREEK/WILMINGTON, DE/FLOW/01JAN2012/1DAY/USGS/"
  parts = dss_parts_split(path)
  parts_all = dss_parts_split(path, keep = TRUE)

  expect_identical(names(parts), LETTERS[1:6])
  expect_identical(names(parts_all), c("path", LETTERS[1:6]))
  expect_identical(parts, parts_all[LETTERS[1:6]])
  expect_identical(parts_all[["path"]], path)
  expect_identical(parts[["A"]], "BRANDYWINE CREEK")
  expect_identical(parts[["B"]], "WILMINGTON, DE")
  expect_identical(parts[["C"]], "FLOW")
  expect_identical(parts[["D"]], "01JAN2012")
  expect_identical(parts[["E"]], "1DAY")
  expect_identical(parts[["F"]], "USGS")
  expect_error(dss_parts_split("/hg/d"))

  new_path = dss_parts_replace(path, list(B = "DE", D = ""))
  expect_identical(new_path, "/BRANDYWINE CREEK/DE/FLOW//1DAY/USGS/")
  expect_error(dss_parts_replace(path, list(A = NULL)))
  expect_error(dss_parts_replace(path, list(G = "foo")))
  expect_error(dss_parts_combine("/hg/d"))

  expect_identical(path,  dss_parts_combine(parts))

})
