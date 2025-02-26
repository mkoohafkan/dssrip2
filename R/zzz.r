#' dssrip2
#'
#' A wrapper for the
#' [HEC-DSS Java API](https://www.hec.usace.army.mil/confluence/dssjavaprogrammer)
#' allowing an interface between R and many common hydrologic models.
#' This is a rewrite of the original "dssrip" package.
#'
"_PACKAGE"


#' @importFrom rJava .jpackage
#' @importFrom utils packageName
.onLoad = function(libname, pkgname) {
  .jpackage(packageName(), nativeLibrary = FALSE, own.loader = TRUE)
}


.onAttach = function(libname, pkgname) {
  packageStartupMessage("Run dss_connect() to initialize.")
}

.onDetach = function() {
  dss_close_all()
}
