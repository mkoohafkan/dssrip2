skip_if_offline()

test_that("URLs work", {
  desc_url = "https://www.hec.usace.army.mil/confluence/dssdocs/dssjavaprogrammer"
  expect_identical(attr(curlGetHeaders(desc_url), "status"), 200L)
})
