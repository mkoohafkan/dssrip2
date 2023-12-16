skip_if_no_dss = function() {
  skip_on_cran()

  if (isTRUE(as.logical(Sys.getenv("CI", "false")))) {
      # look for .dssrip2 folder in temp directory
      dss_home_ci = Sys.getenv("DSS_HOME_CI")
      if (nzchar(dss_home_ci)) {
        if (dss_require(dss_home_ci, monolith = TRUE)) {
          return(invisible(TRUE))
        }
      }
  } else {
    if (dss_require()) {
      return(invisible(TRUE))
    }
  }
  skip("Could not connect to DSS")
}
