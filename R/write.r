# SEE
#
# https://www.hec.usace.army.mil/software/hec-dssvue/documentation/User%27sManual_2.0/HEC-DSSVue_20_Users_Manual.pdf
#
# Appendix B 5.3, 5.4, 5.5


#' Delete DSS Records
#'
#' Delete records from a DSS file.
#'
#' @inheritParams dss_squeeze
#' @inheritParams dss_parts_split
#' @param full If `TRUE`, delete the entire dataset. If `FALSE`, only
#'   delete the section as specified in the D Part of the path.
#' @param squeeze If `TRUE`, squeeze the file after deleting records.
#'
#' @seealso [dss_catalog()] [dss_squeeze()]
#'
#' @importFrom rJava .jnew
#' @export
dss_delete = function(filename, path, full = TRUE, squeeze = FALSE) {
  assert_dss_connected()
  assert_path_format(path)
  file = dss_file(filename)
  on.exit(file$done(), add = TRUE)
  if (full) {
    path = dss_parts_replace(path, list(D = ".*"))
  }
  search_pattern = paste(sprintf("(%s)", path), collapse = "|")
  # cannot delete a condensed path; must provide full set of paths
  delete_paths = dss_catalog(filename, condensed = FALSE,
    pattern = search_pattern)
  if (length(delete_paths) < 1L) {
    stop("Could not find pathname ", path)
  }
  path_vector = .jnew("java.util.Vector", length(delete_paths))
  for (p in delete_paths) {
    path_vector$add(p)
  }
  file$getDataManager()$delete(path_vector)
  if (squeeze) {
    dss_squeeze(filename)
  }
  dss_catalog(filename, condensed = FALSE, rebuild = TRUE)
  invisible(TRUE)
}


#' Guess DSS Class
#'
#' Guess the DSS container based on the R object structure.
#'
#' @param x An R object.
#' @return The DSS Java object type.
#'
#' @importFrom lubridate is.instant
#' @keywords internal
guess_dss_container = function(x) {
  if (inherits(x, "raster")) {
    "hec.io.GridContainer"
  } else if (inherits(x, "data.frame") && ncol(x) > 1L) {
    if ((ncol(x) == 2L) && is.instant(x[[1]])) {
      "hec.io.TimeSeriesContainer"
    } else {
      "hec.io.PairedDataContainer"
    }
  } else {
    stop("Could not determine DSS container type of 'x'.")
  }
}


#' Write DSS Records
#'
#' Write a DSS time series, paired dataset, or grid to a DSS file.
#'
#' @param x The dataframe (Time Series or Paired Data) or raster (Grid)
#'   to write to DSS. Must include attribute `"dss_attributes"`
#'   containing required attributes.
#' @inheritParams dss_squeeze
#' @param path The DSS path to write.
#'
#' @seealso [dss_read()] [dss_catalog()] [dss_attributes()]
#'
#' @export
dss_write = function(x, filename, path) {
  assert_write_support(x)
  assert_path_format(path)
  file = dss_file(filename)
  on.exit(file$done(), add = TRUE)
  class_name = guess_dss_container(x)
  dssObj = switch(class_name,
    "hec.io.GridContainer" = dss_to_grid(x),
    "hec.io.TimeSeriesContainer" = dss_to_timeseries(x,
      dss_parts_split(path)[["E"]]),
    "hec.io.PairedDataContainer" = dss_to_paired(x,
      dss_parts_split(path)[["C"]])
  )
  dssObj$setFullName(path)
  file$put(dssObj)
  invisible(TRUE)
}


#' DSS Times From POSIXct
#'
#' Convert POSIXct timestamps to DSS times.
#'
#' @param datetimes A POSIXct vector.
#'   Default is 60L (one  minute).
#' @inheritParams dss_to_timeseries
#' @return A named list for writing to DSS.
#'
#' @importFrom rJava .jnew
#' @importFrom lubridate as_datetime tz
#' @keywords internal
dss_times_from_posix = function(datetimes, dss_interval) {
  dss_interval = toupper(dss_interval[1])
  assert_timeseries_interval(dss_interval)

  timeIntervalSeconds = unique(as.integer(as.numeric(diff(datetimes),
    "secs")))

  if (grepl("^[0-9]MIN$", dss_interval)) {
    base_interval = as.integer(gsub("MIN$", "", dss_interval)) * 60L
    expected_interval = base_interval
  } else if(grepl("^[0-9]HOUR$", dss_interval)) {
    base_interval = as.integer(gsub("HOUR$", "", dss_interval)) * 3600L
    expected_interval = base_interval
  } else if (grepl("^1DAY$", dss_interval)) {
    base_interval = 86400L
    expected_interval = base_interval
  } else if (grepl("^1WEEK$", dss_interval)) {
    base_interval = 86400L * 7L
    expected_interval = base_interval
  } else if (grepl("^1YEAR$", dss_interval)) {
    expected_interval = c(365L, 366L) * 86400L
  } else if (grepl("^1MON$", dss_interval)) {
    expected_interval = c(28L, 29L, 30L, 31L) * 86400L
  } else if (grepl("SEMI-MONTH$", dss_interval)) {
    expected_interval = c(14L, 15L, 16L) * 86400
  } else if (grepl("TRI-MONTH$", dss_interval)) {
    expected_interval = c(9L, 10L, 11L) * 86400
  } else {
    # irregular time series
    expected_interval = timeIntervalSeconds
  }
  if (!all(timeIntervalSeconds %in% expected_interval)) {
    warning("Time intervals in data do not match specified DSS interval of ",
      dss_interval, call. = FALSE)
  }
  min_interval = min(timeIntervalSeconds)
  if (min_interval < 60) {
    granularity_seconds = 1L
  } else {
    granularity_seconds = 60L
  }
  # origin time as POSIXct
  dss_origin_time = as_datetime(DSS_ORIGIN, tz = DSS_TIMEZONE)
  # get timezone offset in milliseconds
  timeZoneRawOffset = as.integer(as_datetime(DSS_ORIGIN,
    tz = tz(datetimes)) - dss_origin_time) * 1000L
  # get times as integers
  times = as.integer(difftime(datetimes, dss_origin_time,
    units = "mins"))
  # return list of time properties for writing to DSS
  list(
    "timeZoneRawOffset" = timeZoneRawOffset,
    "timeGranularitySeconds" = granularity_seconds,
    "times" = .jnew("hec.heclib.util.HecTimeArray", times)
  )
}


#' POSIXct to HecTime
#'
#' Convert an R POSIXct datetime to Java HecTime.
#'
#' @param x A POSIXct timestamp.
#' @param granularity The minimum time interval.
#'   Can be "secs" or "mins".
#' @param as_period If `TRUE`, adjust timestamp to represent a period,
#'   with timestamp specifying period end time (0100 to 2400 hours).
#' @return A Java `hec.heclib.util.HecTime` object.
#'
#' @importFrom rJava .jnew
#' @importFrom lubridate tz
#' @keywords internal
as_hectime = function(x, granularity_seconds) {
  tObj = .jnew("hec.heclib.util.HecTime")
  tObj$setTimeGranularityInSeconds(granularity_seconds)
  date_part = strftime(x, "%Y-%m-%d", tz = tz(x))
  time_part = strftime(x, "%H:%M:%S", tz = tz(x))
  tObj$setDate(date_part)
  tObj$setTime(time_part)
  tObj
}


#' Timeseries to DSS
#'
#' Convert a data frame to a DSS Time Series object.
#'
#' @param d A data frame.
#' @param dss_interval The DSS time interval, e.g., "15MIN", "1DAY",
#'   "IR-CENTURY", etc.
#' @return A `TimeSeriesContainer` Java object reference.
#'
#' @importFrom rJava .jnew .jclass
#' @keywords internal
dss_to_timeseries = function(d, dss_interval) {
  formatted_times = format_datetimes(d[, 1])
  # build time series object
  tsObj = .jnew("hec.io.TimeSeriesContainer")
  attributes = attr(d, "dss_attributes")
  assert_attributes(.jclass(tsObj), attributes)
  tsObj$numberValues = length(formatted_times)
  # get time properties
  dsstimes = dss_times_from_posix(formatted_times, dss_interval)
  tsObj$setTimeGranularitySeconds(dsstimes$timeGranularitySeconds)
  tsObj$setTimes(dsstimes$times)
  tsObj$setStartTime(as_hectime(min(formatted_times),
    dsstimes$timeGranularitySeconds))
  tsObj$setEndTime(as_hectime(max(formatted_times),
    dsstimes$timeGranularitySeconds))
  # value properties
  tsObj$setUnits(attributes[["units"]])
  tsObj$setType(attributes[["type"]])
  tsObj$setValues(na_to_java(as.numeric(d[, 2])))

  tsObj

#  tsObj$setParameter
#
#
#      "fileName",
#      "fullName",
#      "startTime",
#      "endTime",
#      "timeZoneRawOffset",
#      "timeZoneID",
#      "parameter",
#      "numberValues",
#      "times",
#      "values"
#
}


#' Paired Data to DSS
#'
#' Convert a data frame to a DSS Paired Data object.
#'
#' @param d A data frame.
#' @return A `PairedDataContainer` Java object reference.
#'
#' @importFrom rJava .jnew .jarray
#' @keywords internal
dss_to_paired = function(d, param_part) {
  # build paired data object
  pdObj = .jnew("hec.io.PairedDataContainer")
  attributes = attr(d, "dss_attributes")
  assert_attributes(.jclass(pdObj), attributes)
  ncurves = ncol(d) - 1L
  pdObj$setNumberCurves(ncurves)
  pdObj$setNumberOrdinates(nrow(d))
  pdObj$setXType(attributes$xtype)
  pdObj$setXUnits(attributes$xunits)
  pdObj$setYType(attributes$ytype)
  pdObj$setYUnits(attributes$yunits)
  # check for labels
  yparam = strsplit(param_part, "-")[[c(1, 2)]]
  if (all(grepl(sprintf("^%s(_[0-9]+)*", yparam), names(d)[-1],
    ignore.case = TRUE))) {
    pdObj$labelsUsed = FALSE
  } else {
    pdObj$setLabels(names(d)[-1])
  }
  # set NA values
  for (n in seq_len(ncol(d))) {
    d[n] = na_to_java(d[[n]])
  }
  pdObj$setXOrdinates(d[, 1])
  # need a double[][] array
  pdObj$setYOrdinates(.jarray(lapply(d[seq.int(2L, ncol(d))],
    .jarray, "[D"), "[D"))

  pdObj
}


#' Grid to DSS
#'
#' Convert a raster to a DSS Grid object.
#'
#' @param r A raster.
#' @return A `hec.io.GridContainer` Java object reference.
#'
#' @importFrom rJava .jnew .jarray
#' @keywords internal
dss_to_grid = function(r) {
  stop("Not implemented")
  attributes = attr(r, "dss_attributes")


}
