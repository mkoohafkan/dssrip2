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
    if (length(unique(dss_parts_replace(path, list(D = "")))) > 1L) {
      stop("Multiple paths detected. Only single or ",
        "condensed paths are supported.")
    }
  }
  dssObj = file$get(path, full)
  assert_read_support(dssObj)
  jclass = .jclass(dssObj)
  if (jclass == "hec.io.TimeSeriesContainer") {
    dss_read_timeseries(dssObj, offset)
  } else if (jclass == "hec.io.PairedDataContainer") {
    dss_read_paired(dssObj)
  } else if (jclass == "hec.io.GridContainer") {
    dss_read_grid(dssObj)
  } else {
    stop("Something went wrong, file a bug report.")
  }
}


#' DSS Time Series
#'
#' Read DSS time series.
#'
#' @param tsObj A `hec.io.TimeSeriesContainer` Java object reference.
#' @inheritParams dss_read
#' @return A data frame.
#'
#' @keywords internal
dss_read_timeseries = function(tsObj, offset = FALSE) {
  assert_timeseries(tsObj)
  ts_info =   list(
    "timeGranularitySeconds" = tsObj$getTimeGranularitySeconds(),
    "type" = tsObj$getType(),
    "timeIntervalSeconds" = tsObj$getTimeIntervalSeconds(),
    # no method for timezone offset
    "timeZoneRawOffset" = tsObj$timeZoneRawOffset,
    "parameter" = tsObj$getParameterName(),
    "units" = tsObj$getUnits()
  )
  times = tsObj$getMinutes()
  values = tsObj$getValues()
  out = data.frame(dss_times(times, ts_info, offset),
    ifelse(abs(values - rep(DSS_MISSING_VALUE,
      length(values))), NA, values)
  )
  names(out) = c("datetime", tolower(ts_info[["parameter"]]))
  attr(out, "dss.type") = ts_info[["type"]]
  attr(out, "dss.units") = ts_info[["units"]]
  out
}


#' DSS Paired Data
#'
#' Read DSS paired data.
#'
#' @param pdObj A `hec.io.PairedDataContainer` Java object reference.
#' @return A data frame.
#'
#' @importFrom rJava .jevalArray
#' @keywords internal
dss_read_paired = function(pdObj) {
  assert_paired(pdObj)
  pd_info =   list(
    "xparameter" = pdObj$xparameter,
    "labels" = pdObj$getLabels(),
    "labelsUsed" = pdObj$labelsUsed,
    "numberCurves" = pdObj$getNumberCurves(),
    "xtype" = pdObj$getXType(),
    "ytype" = pdObj$getYType(),
    "xunits" = pdObj$getXUnits(),
    "yunits" = pdObj$getYUnits()
  )
  # y ordinate labels
  labels = ifelse(pd_info[["labelsUsed"]], pd_info[["labels"]],
    seq.int(pd_info[["numberCurves"]]))
  if (pd_info[["labelsUsed"]] || (length(labels) > 1L)) {
    ynames = paste(pd_info[["yparameter"]], labels, sep = ".")
  } else {
    ynames = paste(pd_info[["yparameter"]])
  }
  # sic, typo is in Java
  out = cbind(
    pdObj$getXOridnates(),
    as.data.frame(t(.jevalArray(pdObj$getYOridnates(),
      simplify = TRUE)))
  )
  names(out) = c(pd_info[["xparameter"]], ynames)
  attr(out, "dss.type") = unlist(pd_info[c("xtype", "ytype")],
    use.names = FALSE)
  attr(out, "dss.units") = unlist(pd_info[c("xunits", "yunits")],
    use.names = FALSE)
  out
}


#' DSS Grid Data
#'
#' Read DSS grid.
#'
#' @param gridObj A `hec.io.GridContainer` Java object reference.
#' @return A data frame.
#'
#' @importFrom terra rast
#' @keywords internal
dss_read_grid = function(gridObj) {
  assert_grid(gridObj)

  infObj = gridObj$getGridInfo()
  grid_dims = list(
    llY = infObj$getLowerLeftCellY(),
    numY = infObj$getNumberOfCellsY(),
    llX = infObj$getLowerLeftCellX(),
    numX = infObj$getNumberOfCellsX(),
    cellSize = infObj$getCellSize()
  )
  grid_dims[["lrX"]] = grid_dims[["llX"]] + grid_dims[["numX"]]
  grid_dims[["ulY"]] = grid_dims[["llY"]] + grid_dims[["numY"]]

  grid_info = list(
    SpatialReferenceSystem = infObj$getSpatialReferenceSystem(),
    dataType = infObj$getDataTypeName(),
    dataUnits = infObj$getDataUnits(),
    noDataValue = infObj$getGridNodataValue(),
    startTime = strptime(infObj$getStartTime(), "%d %B %Y, %H:%M",
      tz = "UTC"),
    endTime = strptime(infObj$getEndTime(), "%d %B %Y, %H:%M",
      tz = "UTC")
  )

  zname = dss_parts_split(gridObj$fullName)[["C"]]

  d = matrix(gridObj$getGridData()$getData(), byrow = FALSE,
    nrow = grid_dims[["numX"]], ncol = grid_dims[["numY"]])
  d = d[, rev(seq_len(ncol(d)))]

  if (grepl("PER-", grid_info[["dataType"]])) {
    grid_time = grid_info[["endTime"]]
  } else {
    grid_time = grid_info[["startTime"]]
  }

  out = rast(
    vals = as.vector(d),
    resolution = grid_dims[["cellSize"]],
    xmin = grid_dims[["llX"]] * grid_dims[["cellSize"]],
    ymin = grid_dims[["llY"]] * grid_dims[["cellSize"]],
    xmax = grid_dims[["lrX"]] * grid_dims[["cellSize"]],
    ymax = grid_dims[["ulY"]] * grid_dims[["cellSize"]],
    crs = grid_info$SpatialReferenceSystem,
    time = grid_time,
    names = zname
  )
  out[abs(out - grid_info[["noDataValue"]]) < 1e-5] = NA

  attr(out, "dss.type") = grid_info[["dataType"]]
  attr(out, "dss.units") = grid_info[["dataUnits"]]
  attr(out, "dss.times") = c(grid_info[["startTime"]],
    grid_info[["endTime"]])

  out
}
