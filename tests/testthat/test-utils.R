test_that("NA fill works", {

  v1 = c(1L, 2L, NA, NA, 5L)
  v2 = c(NA, NA, 3L, 4L, NA)

  expect_identical(na_fill(v1), c(1L, 2L, 2L, 2L, 5L))
  expect_identical(na_fill(v1, FALSE), c(1L, 2L, 5L, 5L, 5L))
  expect_identical(na_fill(v2), c(NA, NA, 3L, 4L, 4L))
  expect_identical(na_fill(v2, FALSE), c(3L, 3L, 3L, 4L, NA))
  expect_identical(na_fill(rep(NA, 5)), na_fill(rep(NA, 5), FALSE))
  expect_identical(na_fill(numeric(0)), numeric(0))
})


test_that("DSS timezone works", {

  expect_equal(dss_timezone(""), "etc/GMT+0")
  expect_equal(dss_timezone(NULL), "etc/GMT+0")
  expect_equal(dss_timezone(0), "etc/GMT+0")
  expect_warning(expect_equal(dss_timezone(3600), "etc/GMT+0"))
  expect_equal(dss_timezone(3600e3), "etc/GMT-1")
  expect_equal(dss_timezone(-3600e3), "etc/GMT+1")
  expect_error(dss_timezone(Inf))
  expect_error(dss_timezone(NA))

})


test_that("DSS timestamp extraction works", {

  

  expect_equal(dss_timezone(""), "etc/GMT+0")
  expect_equal(dss_timezone(NULL), "etc/GMT+0")
  expect_equal(dss_timezone(0), "etc/GMT+0")
  expect_warning(expect_equal(dss_timezone(3600), "etc/GMT+0"))
  expect_equal(dss_timezone(3600e3), "etc/GMT-1")
  expect_equal(dss_timezone(-3600e3), "etc/GMT+1")
  expect_error(dss_timezone(Inf))
  expect_error(dss_timezone(NA))

})
