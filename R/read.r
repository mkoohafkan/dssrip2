#' Read DSS Attributes
#'
#' Read attributes from a DSS file.
#'
#' @inheritParams dss_squeeze
#' @param path The DSS path to query attributes for.
#' @return A List of attributes.
#'
#' @seealso [dss_read()]
#' @importFrom rJava .jclass
#' @export
dss_attributes = function(file, path) {
  assert_path_format(path)
  assert_dss_file(file)
  dssObj = file$get(path, TRUE)
  java_metadata(dssObj)
}


#' Read DSS Data
#'
#' Read data from a DSS file.
#'
#' @inheritParams dss_squeeze
#' @param path The DSS path to extract.
#' @param full If `TRUE`, read the entire dataset. If `FALSE`, only
#'   read the section as specified in the D Part of the path.
#' @param offset (TimeSeries only) If `TRUE`, and values are period
#'   averages, offset timestamps to center on periods.
#' @return A data frame. For a time series object, the first column
#'   will be "datetime". Further columns will be named according to
#'   the DSS "parameter" value specified in the object metadata.
#'
#' @details The returned data frame includes additional attributes
#'   `"dss.type"` and `"dss.units"` describing the DSS data format.
#'
#' @seealso [dss_attributes()]
#' @importFrom rJava .jclass
#' @export
dss_read = function(file, path, full = TRUE, offset = FALSE) {
  assert_path_format(path)
  assert_dss_file(file)
  if (full) {
    path = unique(dss_parts_replace(path, list(D = "")))
  }
  if (length(path) > 1L) {
    stop("Multiple paths detected. Only single or ",
      "condensed paths are supported.")
  }
  dssObj = file$get(path, full)
  jclass = .jclass(dssObj)
  if (jclass == "hec.io.TimeSeriesContainer") {
    dss_read_timeseries(dssObj, offset)
  } else if (jclass == "hec.io.PairedDataContainer") {
    dss_read_paired(dssObj)
  } else if (jclass == "hec.io.GridContainer") {
    dss_read_grid(dssObj)
  } else {
    stop("Object of type ", jclass, " is not currently supported.")
  }
}


#' DSS Time Series
#'
#' Read a time series from a DSS file.
#'
#' @param tsObj A `hec.io.TimeSeriesContainer` Java object reference.
#' @inheritParams dss_read
#' @return A data frame.
#'
#' @keywords internal
dss_read_timeseries = function(tsObj, offset = FALSE) {
  assert_timeseries(tsObj)
  metadata = java_metadata(tsObj)
  valuenames = tolower(metadata[["parameter"]])
  out = data.frame(dss_times(tsObj, metadata, offset),
    ifelse(abs(tsObj$values - rep(DSS_MISSING_VALUE,
      length(tsObj$values))), NA, tsObj$values)
  )
  names(out) = c("datetime", valuenames)
  attr(out, "dss.type") = metadata[["type"]]
  attr(out, "dss.units") = metadata[["units"]]
  out
}


#' DSS Paired Data
#'
#' Read paired data from a DSS file.
#'
#' @param pdObj A `hec.io.PairedDataContainer` Java object reference.
#' @return A data frame.
#'
#' @keywords internal
dss_read_paired = function(pdObj) {
  assert_paired(pdObj)
  metadata = java_metadata(pdObj)
  # ordinate label
  xname = metadata[["xparameter"]]
  # y ordinate labels
  labels = ifelse(metadata[["labelsUsed"]], metadata[["labels"]],
    seq.int(metadata[["numberCurves"]]))
  if (metadata[["labelsUsed"]] || (length(labels) > 1L)) {
    ynames = paste(pdObj$yparameter, labels, sep = ".")
  } else {
    ynames = paste(pdObj$yparameter)
  }
  out = cbind(pdObj$xOrdinates,
    as.data.frame(t(pdObj$yOrdinates)))
  names(out) = c(xname, ynames)
  attr(out, "dss.type") = unlist(metadata[c("xtype", "ytype")],
    use.names = FALSE)
  attr(out, "dss.units") = unlist(metadata[c("xunits", "yunits")],
    use.names = FALSE)
  out
}


#' DSS Grid Data
#'
#' Read grid data from a DSS file.
#'
#' @param gridObj A `hec.io.GridContainer` Java object reference.
#' @return A data frame.
#'
#' @keywords internal
dss_read_grid = function(gridObj) {
  stop("Grid is not supported yet")
  assert_grid(gridObj)
  metadata = java_metadata(gridObj)
  # ordinate label
  xname = metadata[["xparameter"]]
  # y ordinate labels
  labels = ifelse(metadata[["labelsUsed"]], metadata[["labels"]],
    seq.int(metadata[["numberCurves"]]))
  if (metadata[["labelsUsed"]] || (length(labels) > 1L)) {
    ynames = paste(gridObj$yparameter, labels, sep = ".")
  } else {
    ynames = paste(gridObj$yparameter)
  }
  out = cbind(gridObj$xOrdinates,
    as.data.frame(t(gridObj$yOrdinates)))
  names(out) = c(xname, ynames)
  attr(out, "dss.type") = unlist(metadata[c("xtype", "ytype")],
    use.names = FALSE)
  attr(out, "dss.units") = unlist(metadata[c("xunits", "yunits")],
    use.names = FALSE)
  out
}
