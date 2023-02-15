skip_if_offline()

test_that("URLs work", {
  desc_url = "https://www.hec.usace.army.mil/confluence/dssdocs/dssjavaprogrammer/"
  hec_url = "https://www.hec.usace.army.mil/software/hec-dssvue/"

  expect_identical(attr(curlGetHeaders(desc_url), "status"), 200L)
  expect_identical(attr(curlGetHeaders(hec_url), "status"), 200L)

})
