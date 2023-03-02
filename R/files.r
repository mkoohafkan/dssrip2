#' Open DSS File
#'
#' Call the Java HecDss Open method.
#'
#' @param filename A valid filepath.
#' @return A DSS file handle Java object.
#'
#' @importFrom rJava .jcall
#' @keywords internal
dss_file = function(filename, ...) {
  file = .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;",
    method = "open", filename, ...)
  on.exit(file$done(), add = TRUE)
  file
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
#' @seealso [dss_close()] [dss_version()]
#'
#' @export
dss_open = function(filename) {
  assert_dss_connected()
  filepath = normalizePath(filename, mustWork = TRUE)
  dss_file(filepath)
}


#' @rdname dss_open
#'
#' @param version The DSS version of the created file (6 or 7).
#'
#' @export
dss_create = function(filename, version = 7L) {
  assert_dss_connected()
  version = as.integer(version[1])
  if (!any(version == c(6L, 7L))) {
    stop("Only DSS version 6 or 7 is supported")
  }
  filepath = normalizePath(filename, mustWork = FALSE)
  if (file.exists(filepath)) {
    stop("File ", filepath, " already exists.")
  }
  dss_file(filepath, version)
}


#' Squeeze a DSS File
#'
#' Calls the squeeze method on file to remove deleted or overwritten
#' data.
#'
#' @param file A DSS file handle, e.g., output of [dss_open()].
#'
#' @seealso [dss_delete()]
#'
#' @export
dss_squeeze = function(file) {
  assert_dss_connected()
  assert_dss_file(file)
  on.exit(file$done(), add = TRUE)
  file$getDataManager()$squeeze()
  invisible(TRUE)
}


#' DSS File Version
#'
#' Get the DSS file version.
#'
#' @inheritParams dss_squeeze
#' @return An integer
#'
#' @seealso [dss_convert()]
#'
#' @export
dss_version = function(file) {
  assert_dss_connected()
  assert_dss_file(file)
  on.exit(file$done(), add = TRUE)
  as.integer(file$getDataManager()$getDssFileVersion())
}


#' DSS File Conversion
#'
#' Convert between DSS file versions.
#'
#' @inheritParams dss_squeeze
#' @param to The destination file path.
#'
#' @seealso [dss_version()] [dss_create()]
#'
#' @export
dss_convert = function(file, to) {
  assert_dss_connected()
  assert_dss_file(file)
  on.exit(file$done(), add = TRUE)
  file$getDataManager()$convertVersion(to)
  invisible(TRUE)
}


#' Close DSS File
#'
#' Close a DSS file. This function is provided for completion of the
#' interface but is not strictly required as these methods can be
#' called directly from the file object (i.e., `file$done()`).
#'
#' @inheritParams dss_squeeze
#' @param implicit If `TRUE`, use method `done()` instead of `close()`.
#'
#' @seealso [dss_open()]
#'
#' @export
dss_close = function(file, implicit = FALSE) {
  assert_dss_connected()
  assert_dss_file(file)
  if (implicit) {
    file$done()
  } else {
    file$close()
  }
  invisible(TRUE)
}