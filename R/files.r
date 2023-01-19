#' @importFrom rJava .jcall
#' @keywords internal
dss_file = function(filename) {
  dssFile = .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;",
    method = "open", filename)
  return(dssFile)
}


#' DSS File Handle
#'
#' Open or create a DSS file.
#'
#' @param filename Location of DSS file to open.
#' @return A `hec.heclib.dss.HecDss` Java object reference.
#'
#' @details Objects created by this function must be explicitly closed
#'   using the `$close()` or `$done()` methods. If this function fails
#'   with a message that `javaHeclib.dll` cannot be found, this usually
#'   indicates that `dss.home` is not set correctly.
#'
#' @export
dss_open = function(filename) {
  filepath = normalizePath(filename, mustWork = TRUE)
  dss_file(filepath)
}


#' @rdname dss_open
#' @export
dss_create = function(filename) {
  filepath = normalizePath(filename, mustWork = FALSE)
  if (file.exists(filepath)) {
    stop("File ", filepath, " already exists.")
  }
  dss_file(filepath)
}

#' Squeeze a DSS File
#'
#' Calls the squeeze method on file to remove deleted or overwritten
#' data.
#'
#' @param file A DSS file handle, e.g., output of [dss_open()].
#'
#' @export
dss_squeeze = function(file) {
  file$getDataManager()$squeeze()
  invisible(TRUE)
}
