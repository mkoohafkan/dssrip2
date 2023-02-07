skip_if_not(dss_require())

test_that("path list works", {
  on.exit(f$close(), add = TRUE)
  f = dss_open(system.file("extdata/test.dss", package = "dssrip2"))
  all_paths = dss_paths(f, pattern = "*", condensed = FALSE)
  all_paths_c = dss_paths(f, pattern = "*", condensed = TRUE)
  paths1 = dss_paths(f, pattern = "^/RACCOON CREEK/.*/$",
    condensed = FALSE)
  paths1_c = dss_paths(f, pattern = "^/RACCOON CREEK/.*/$",
    condensed = TRUE)
  paths2 = dss_paths(f, pattern = "^/.*/.*/.*/.*/IR-CENTURY/.*$",
    condensed = FALSE)
  paths2_c = dss_paths(f, pattern = "^/.*/.*/.*/.*/IR-CENTURY/.*$",
    condensed = TRUE)

  expect_identical(length(all_paths), 550L)
  expect_identical(length(all_paths_c), 26L)

  expect_identical(length(paths1), 84L)
  expect_identical(length(paths1_c), 6L)
  expect_identical(all(paths1 %in% all_paths), TRUE)
  expect_identical(all(paths1 %in% all_paths_c), FALSE)
  expect_identical(all(paths1_c %in% all_paths_c), TRUE)

  expect_identical(length(paths2), 22L)
  expect_identical(length(paths2_c), 11L)
  expect_identical(all(paths2 %in% all_paths), TRUE)
  expect_identical(all(paths2 %in% all_paths_c), FALSE)
  expect_identical(all(paths2_c %in% all_paths_c), TRUE)

})

test_that("path parts works", {

  path = "/BRANDYWINE CREEK/WILMINGTON, DE/FLOW/01JAN2012/1DAY/USGS/"
  parts = dss_parts_split(path)

  expect_identical(names(parts), c("path", LETTERS[1:6]))
  expect_identical(parts[["path"]], path)
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