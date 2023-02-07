#' DSS Assertions
#'
#' Helper functions for DSS type and format checking.
#'
#' @name dss-assertions
#' @keywords internal
NULL


#' @describeIn dss-assertions
#'
#' Assert that DSS dependencies have been loaded.
#'
#' @keywords internal
assert_dss_connected = function() {
  if (!isTRUE((hecJavaConnectionDB$DSS_CONNECTED))) {
    stop("DSS connection not active. Did you connect to DSS using ",
      "dss_connect()", "?")
  }
}


#' @describeIn dss-assertions
#'
#' Assert the supplied object is a DSS file handle.
#'
#' @keywords internal
assert_dss_file = function(file) {
  if (!(inherits(file, "jobjRef") &&
    attr(file, "jclass") == "hec/heclib/dss/HecDss")) {
    stop("Invalid DSS file handle. Did you open the DSS file using ",
      "dss_open()", "?")
  }
}


#' @describeIn dss-assertions
#'
#' Assert the supplied path is in the correct format.
#'
#' @keywords internal
assert_path_format = function(path) {
  bad = !grepl("^/.*/.*/.*/.*/.*/.*/$", path)
  if (any(bad)) {
    stop("Unexpect path formats:\n", paste0("\t", path[bad],
      collapse = "\n"))
  }
}


#' @describeIn dss-assertions
#'
#' Assert set of supplied parts is named and complete.
#'
#' @keywords internal
assert_parts_format = function(parts) {
  missing = setdiff(DSS_PARTS, names(parts))
  if (length(missing) > 0L) {
    stop("Missing path parts: ", paste(missing, collapse = ", "))
  }
}


#' @describeIn dss-assertions
#'
#' Assert the supplied rJava object is a DSS timeseries.
#'
#' @importFrom rJava .jclass
#' @keywords internal
assert_read_support = function(obj) {
  supported_classes = c(
    "hec.io.TimeSeriesContainer",
    "hec.io.PairedDataContainer",
    "hec.io.GridContainer"
  )
  jclass = .jclass(obj)
  if (!(jclass %in% supported_classes)) {
    stop("No read support for objects of type ", jclass)
  }
}


#' @describeIn dss-assertions
#'
#' Assert the supplied rJava object is a DSS timeseries.
#'
#' @importFrom rJava .jclass
#' @keywords internal
assert_write_support = function(obj) {
  supported_classes = c(
#    "hec.io.TimeSeriesContainer",
#    "hec.io.PairedDataContainer",
#    "hec.io.GridContainer"
  )
  jclass = .jclass(obj)
  if (!(jclass %in% supported_classes)) {
    stop("No write support for objects of type ", jclass)
  }
}


#' @describeIn dss-assertions
#'
#' Assert the supplied rJava object is a DSS timeseries.
#'
#' @importFrom rJava .jclass
#' @keywords internal
assert_timeseries = function(obj) {
  jclass = .jclass(obj)
  if (jclass != "hec.io.TimeSeriesContainer") {
    stop("Unexpected data type: ", jclass)
  }
}


#' @describeIn dss-assertions
#'
#' Assert the supplied rJava object is a DSS paired data container.
#'
#' @importFrom rJava .jclass
#' @keywords internal
assert_paired = function(obj) {
  jclass = .jclass(obj)
  if (jclass != "hec.io.PairedDataContainer") {
    stop("Unexpected data type: ", jclass)
  }
}


#' @describeIn dss-assertions
#'
#' Assert the supplied rJava object is a DSS grid data container.
#'
#' @importFrom rJava .jclass
#' @keywords internal
assert_grid = function(obj) {
  jclass = .jclass(obj)
  if (jclass != "hec.io.GridContainer") {
    stop("Unexpected data type: ", jclass)
  }
}
