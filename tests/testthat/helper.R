skip_if_no_dss = function() {
  skip_on_cran()
  if (dss_require()) {
      return(invisible(TRUE))
  }
  skip("Could not connect to DSS")
}
