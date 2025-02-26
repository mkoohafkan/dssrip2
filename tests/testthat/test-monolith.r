test_that("requirements file parsing works", {

  req_file = system.file("requirements.yaml", package = "dssrip2")

  expect_s3_class(monolith_requirements(req_file), "data.frame")

})


test_that("asset query works", {
  skip_if_offline(host = "repo.maven.apache.org")
  skip_if_offline(host = "search.maven.org")
  skip_if_offline(host = "www.hec.usace.army.mil")

  req_file = system.file("requirements.yaml", package = "dssrip2")

  expect_s3_class(monolith_assets(req_file), "data.frame")

})


test_that("asset download works", {
  skip_if_offline(host = "repo.maven.apache.org")
  skip_if_offline(host = "search.maven.org")
  skip_if_offline(host = "www.hec.usace.army.mil")

  install_dir = tempfile()
  on.exit(unlink(install_dir, recursive = TRUE), add = TRUE)

  req_file = system.file("requirements.yaml", package = "dssrip2")
  req_artifacts = monolith_requirements(req_file)[["artifactId"]]

  expect_null(dss_install_monolith(install_dir, req_file))
  expect_setequal(list.files(install_dir), c("jar", "lib"))

  expect_setequal(
    lapply(req_artifacts, function(x) {
      any(grepl(x, list.files(install_dir, recursive = TRUE)))
    }),
    TRUE
  )

  expect_error(
    dss_install_monolith(install_dir, req_file, overwrite = FALSE)
  )
  expect_null(
    dss_install_monolith(install_dir, req_file, overwrite = TRUE)
  )

})
