#' List DSS Attributes
#'
#' Return a vector of attribute names associated with a
#' DSS object class.
#'
#' @param dssObj A DSS object.
#' @return A list of required and optional attribute names.
#'
#' @importFrom rJava .jclass
#' @keywords internal
list_attributes = function(dssObj) {
  jclass = .jclass(dssObj)
  if (jclass == "hec.io.TimeSeriesContainer") {
    list_attributes_timeseries()
  } else if (jclass == "hec.io.PairedDataContainer") {
    list_attributes_paired()
  } else if (jclass == "hec.io.GridContainer") {
    list_attributes_grid()
  } else {
    stop("Object of type ", jclass, " is not currently supported.")
  }
}


#' @rdname list_attributes
list_attributes_timeseries = function() {
  list(
    required = c(
      "interval",
      "type",
      "units"
    ),
    optional = c(
      "subParameter",
      "version",
      "subVersion",
      "watershed",
      "location",
      "subLocation",
      "precision",
      "quality"
    ),
    derived = c(
      "fileName",
      "fullName",
      "startTime",
      "endTime",
      "timeZoneRawOffset",
      "timeZoneID",
      "parameter",
      "numberValues",
      "times",
      "values"
    )
  )
}


#' @rdname list_attributes
list_attributes_paired = function() {
  list(
    required = c(
      "xunits",
      "yunits",
      "xtype",
      "ytype"
    ),
    optional = c(
      "labels"
    ),
    derived = c(
      "xparameter",
      "yparameter",
      "xOrdinates",
      "yOrdinates",
      "labelsUsed",
      "numberCurves"
    )
  )
}


#' @rdname list_attributes
list_attributes_grid = function() {
  list(
    required = c(""),
    optional = c("")
  )
}

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
#' @keywords internal
dss_attributes = function(file, path) {
  stop("not implemented")
  assert_dss_connected()
  assert_dss_file(file)
  assert_path_format(path)
  dssObj = file$get(path, TRUE)
  jclass = .jclass(dssObj)
  if (jclass == "hec.io.TimeSeriesContainer") {
    dss_attributes_timeseries(dssObj)
  } else if (jclass == "hec.io.PairedDataContainer") {
    dss_attributes_paired(dssObj)
  } else if (jclass == "hec.io.GridContainer") {
    dss_attributes_grid(dssObj)
  } else {
    stop("Object of type ", jclass, " is not currently supported.")
  }
}


#' DSS Time Series Attributes
#'
#' Get DSS time series attributes.
#'
#' @inheritParams dss_read_timeseries
#' @return A data frame.
#'
#' @keywords internal
dss_attributes_timeseries = function(tsObj) {
  #exclude = c("values", "times", "modified", "quality", "xOrdinates", "yOrdinates", "xData", "yData")
#  list(
#    "quality" = 
#    "numberValues" =
#    "timeIntervalSeconds" = tsObj$getTimeIntervalSeconds()
#    "startTime" = tsObj$getStartTime()$dateAndTime()
#    "endTime" = tsObj$getEndTime()$dateAndTime()
#    "units" = tsObj$getUnits()
#    "type" = tsObj$getType()
#    "precision" = tsObj$precision
#    "subLocation" = tsObj$subLocation,
#    "parameter" = tsObj$getParameterName(),
#    "subParameter" = tsObj$subParameter,
#    "timeZoneID" = tsObj$getTimeZoneID()
#    "timeZoneRawOffset" = tsObj$timeZoneRawOffset
#    "julianBaseDate" = tsObj$getJulianBaseDate(),
#    "timeGranularitySeconds" = tsObj$getTimeGranularitySeconds()
#    "quality7" = tsObj$getQuality7(),
#    "sizeEachQuality7" =
#    "inotes" =
#    "sizeEachNote" =
#    "cnotes" =
#    "numberDepths" =
#    "profileDepths" =
#    "profileValues" =
#    "unitsProfileDepths" =
#    "unitsProfileValues" =
#    "profileLabel" =
#    "VERTICAL_DATUM_INFO_KEY" =
#    "VERTICAL_DATUM_INFO_HEADER" =
#    "CURRENT_VERTICAL_DATUM_KEY" =
#    "CURRENT_VERTICAL_DATUM_HEADER" =
#    "fullName" =
#    "watershed" =
#    "location" = tsObj$getLocationName()
#    "version" =
#    "subVersion" =
#    "fileName" =
#    "storedAsdoubles" =
#    "dataType" =
#    "lastWriteTimeMillis" =
#    "fileLastWriteTimeMillis" =
#    "xOrdinate" =
#    "yOrdinate" =
#    "zOrdinate" =
#    "coordinateSystem" =
#    "coordinateID" =
#    "horizontalUnits" =
#    "horizontalDatum" =
#    "verticalUnits" =
#    "verticalDatum" =
#    "locationTimezone" =
#    "supplementalInfo" =
#    "otherInfo" =
#    "modified" =
#  )
}


#' DSS Paired Data Attributes
#'
#' Get DSS paired data attributes.
#'
#' @inheritParams dss_read_paired
#' @return A data frame.
#'
#' @keywords internal
dss_attributes_paired = function(pdObj) {

}


#' DSS Grid Attributes
#'
#' Get DSS grod attributes.
#'
#' @inheritParams dss_read_grid
#' @return A data frame.
#'
#' @keywords internal
dss_attributes_grid = function(gridObj) {

}
