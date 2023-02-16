# SEE
#
# https://www.hec.usace.army.mil/software/hec-dssvue/documentation/User%27sManual_2.0/HEC-DSSVue_20_Users_Manual.pdf
#
# Appendix B 5.3, 5.4, 5.5




#' Write to DSS
#'
#' @param x The dataframe (Time Series or Paired Data) or raster (Grid)
#'   to write to DSS.
#' @inheritParams dss_squeeze
#' @param path The DSS path to write.
#' @param attributes A named list of DSS attributes.
#'
#' @importFrom lubridate is.instant
#' @export
dss_write = function(x, file, path, attributes = list()) {
  on.exit(file$done(), add = TRUE)
  assert_write_support(x)
  assert_dss_file(file)
  assert_path_format(path)
  if (inherits(x, "raster")) {
    dssObj = dss_to_grid(x, attributes)
  } else if (inherits(x, "data.frame") && ncol(x) > 1L) {
    if ((ncol(x) == 2L) && is.instant(x[[1]])) {
      dssObj = dss_to_timeseries(x, attributes,
        dss_parts_split(path)["E"])
    } else {
      dssObj = dss_to_paired(x, attributes)
    }
  } else {
    stop("Could not determine DSS container type of 'x'.")
  }
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
#' @param attributes A list of `TimeSeriesContainer` attributes.
#' @param dss_interval The DSS time interval, e.g., "15MIN", "1DAY",
#'   "IR-CENTURY", etc.
#' @return A `TimeSeriesContainer` Java object reference.
#'
#' @importFrom rJava .jnew
#' @keywords internal
dss_to_timeseries = function(d, attributes, dss_interval) {
  formatted_times = format_datetimes(d[, 1])
  # build time series object
  tsObj = .jnew("hec.io.TimeSeriesContainer")
  assert_attributes(tsObj, attributes)
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
#' @param attributes A list of `PairedDataContainer` attributes.
#' @return A `PairedDataContainer` Java object reference.
#'
#' @importFrom rJava .jnew .jarray
#' @keywords internal
dss_to_paired = function(d, attributes) {
  # build paired data object
  pdObj = .jnew("hec.io.PairedDataContainer")
  assert_attributes(pdObj, attributes)
  ncurves = ncol(d) - 1L
 pdObj$setNumberCurves(ncurves)
  pdObj$setNumberOrdinates(nrow(d))
  pdObj$setXType(attributes$xtype)
  pdObj$setXUnits(attributes$xunits)
  pdObj$setYType(attributes$ytype)
  pdObj$setYUnits(attributes$yunits)
  if ("labels" %in% names(attributes)) {
    if (length(attributes$labels) != ncurves) {
      stop("Attribute \"labels\" does not match the supplied dataset ",
      "(expects length of 1 or ", ncurves, ")")
    }
    pdObj$setLabels(attributes$labels)
    pdObj$labelsUsed = TRUE
  }
  # set NA values
  for (n in seq(2, ncurves)) {
    d[n] = na_to_java(d[[n]])
  }
  pdObj$setXOrdinates(na_to_java(d[, 1]))
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
#' @param attributes A list of `hec.io.GridContainer` attributes.
#' @return A `hec.io.GridContainer` Java object reference.
#'
#' @importFrom rJava .jnew .jarray
#' @keywords internal
dss_to_grid = function(r, attributes) {
  stop("Not implemented")
}
