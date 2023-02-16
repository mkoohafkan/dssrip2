skip_if_no_dss()

test_that("DSS File open works", {
  on.exit(conn$close(), add = TRUE)
  conn = dss_open(system.file("extdata/example.dss", package = "dssrip2"))
  expect_s4_class(conn, "jobjRef")
})
