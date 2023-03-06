#' DSS Attributes
#'
#' Get and set DSS attributes.
#'
#' @param x An R object.
#' @return The object `x`, with additional attribute
#'   `"dss_attributes"`.
#'
#' @export
dss_attributes = function(x) {
  attr(x, "dss_attributes")
}

#' @rdname dss_attributes
#'
#' @param attributes_list A named list of DSS attributes.
#'
#' @export
dss_add_attributes = function(x, attributes_list = list()) {
  class_name = guess_dss_container(x)
  # merge with existing attributes, if any
  dss_attr = attr(x, "dss_attributes")
  dss_attr[names(attributes_list)] = attributes_list
  # check that all required attributes are present
  assert_attributes(class_name, dss_attr)
  attr(x, "dss_attributes") = dss_attr
  x
}

#' List DSS Attributes
#'
#' Return a vector of attribute names associated with a
#' DSS object class.
#'
#' @param class_name A DSS Java object class name.
#' @return A list of required and optional attribute names.
#'
#' @importFrom rJava .jclass
#' @keywords internal
list_attributes = function(class_name) {
  if (class_name == "hec.io.TimeSeriesContainer") {
    list_attributes_timeseries()
  } else if (class_name == "hec.io.PairedDataContainer") {
    list_attributes_paired()
  } else if (class_name == "hec.io.GridContainer") {
    list_attributes_grid()
  } else {
    stop("Object of type ", class_name, " is not currently supported.")
  }
}


#' @rdname list_attributes
list_attributes_timeseries = function() {
  list(
    required = c(
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
      "interval",
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
    optional = c(),
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
    required = c(),
    optional = c(),
    derived = c()
  )
}
