#' dssrip
#'
#' A wrapper for the
#' [HEC-DSS Java API](https://www.hec.usace.army.mil/confluence/dssjavaprogrammer)
#' allowing an interface between R and many common hydrologic models.
#' This is a rewrite of the original "dssrip" package.
#' 
#' @docType package
#' @name dssrip
NULL


#' @importFrom rJava .jpackage
#' @importFrom utils packageName
.onLoad = function(libname, pkgname) {
  .jpackage(packageName(), nativeLibrary = FALSE, own.loader = TRUE)
}


#' @importFrom rJava .jengine
.onAttach = function(libname, pkgname) {
  # detect DSS directory
  use_monolith = isTRUE(getOption("dss.monolith"))
  install_dir = getOption("dss.home")
  if (use_monolith) {
    app = "HEC-Monolith"
    if (is.null(install_dir)) {
    install_dir = monolith_default_dir()
  }
  } else {
    app = "HEC-DSSVue"
  }
  if (is.null(install_dir)) {
    dir_msg = "No default HEC-DSSVue directory specified."
  } else if (!dir.exists(install_dir)) {
    dir_msg = paste("Specified", app, "directory",
      shQuote(install_dir), "not found.")
  } else {
    dir_msg = sprintf("Defaulting to %s in %s.", app, shQuote(install_dir))
  }
  # detect java options
  if (is.null(rJava::.jengine(silent=TRUE))) {
    if (!is.null(getOption("java.parameters"))) {
      param_msg = paste("Java VM will be initialized with parameters",
        shQuote(getOption("java.parameters")))
    } else {
      param_msg = "No Java VM parameters specified."
    }
  } else {
    param_msg
  }
  if (isTRUE(getOption("dss.autostart"))) {
    if(is.null(install_dir)) {
      start_msg = paste("Could not connect to", app,
        "automatically since no directory is specified.")
    } else if  (!dir.exists(install_dir)) {
      start_msg = paste("Could not connect to", app,
        "automatically since specified directory was not found.")
    } else {
      dss_connect(install_dir, monolith = use_monolith)
      start_msg = paste("Connected to", app, "succesfully.")
    }
  } else {
    start_msg = "Run dss_connect() to initialize."
  }
  msg = paste(dir_msg, param_msg, start_msg, sep = "\n")
  packageStartupMessage(msg)
}
