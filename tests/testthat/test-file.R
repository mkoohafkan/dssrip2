skip_if_not(dss_require())

test_that("DSS File open works", {
  on.exit(f$close(), add = TRUE)
  f = dss_open(system.file("extdata/test.dss", package = "dssrip2"))
  expect_s4_class(f, "jobjRef")
})
