#' DSS Assertions
#'
#' Helper functions for DSS type and format checking.
#'
#' @name dss-assertions
#' @keywords internal
NULL

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
