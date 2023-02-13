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
#' @param attributes A named list of DSS attributes.
dss_write = function(x, file, path, attributes = list()) {
  on.exit(file$done(), add = TRUE)
  assert_write_support(x)
  assert_dss_file(file)
  assert_path_format(path)
  if (inherits(x, "raster")) {
    dssObj = dss_to_grid(x, attributes)
  } else if (inherits(x, "data.frame")) {
    if (ncol(x) > 2L) {
      dssObj = dss_to_paired(x, attributes)
    } else {
      dssObj = dss_to_timeseries(x, attributes)
    }
  } else {
    stop("Could not determine DSS container type of 'x'.")
  }
  dssObj$setFullName(path)
  file$put(dssObj)
}


#' DSS Times From POSIXct
#'
#' Convert POSIXct timestamps to DSS times.
#'
#' @param datetimes A POSIXct vector.
#' @param type The DSS data type, e.g. "PER_AVER" or "INST".
#' @param granularity The smallest time interval in the data.
#'   Default is 60L (one  minute).
#' @return A named list for writing to DSS.
#'
#' @importFrom rJava .jnew
#' @importFrom lubridate as_datetime tz
#' @keywords internal
dss_times_from_posix = function(datetimes) {
  timeIntervalSeconds = unique(as.integer(as.numeric(diff(datetimes),
    "secs")))
  if (length(timeIntervalSeconds) > 1L) {
    stop("Argument \"datetimes\" is not a regular timeseries.")
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
    "timeIntervalSeconds" = timeIntervalSeconds,
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


#' DSS to Timeseries
#'
#' Convert a data frame to a DSS Time Series object.
#'
#' @param datetimes A POSIXct vector.
#' @param type The DSS data type, e.g. "PER_AVER" or "INST".
#' @param granularity The smallest time interval in the data.
#'   Default is 60L (one  minute).
#' @return A named list for writing to DSS.
#'
#' @importFrom rJava .jnew
#' @keywords internal
dss_to_timeseries = function(d, attributes) {
  formatted_times = format_datetimes(d[, 1])
  # build time series object
  tsObj = .jnew("hec.io.TimeSeriesContainer")
  assert_attributes(tsObj, attributes)
  tsObj$numberValues = length(formatted_times)
  # get time properties
  dsstimes = dss_times_from_posix(formatted_times)
  tsObj$setTimeGranularitySeconds(dsstimes$timeGranularitySeconds)
  tsObj$setTimes(dsstimes$times)
  tsObj$setStartTime(as_hectime(min(formatted_times),
    dsstimes$timeGranularitySeconds))
  tsObj$setEndTime(as_hectime(max(formatted_times),
    dsstimes$timeGranularitySeconds))
  # value properties
  tsObj$setUnits(attributes[["units"]])
  tsObj$setType(attributes[["type"]])
  tsObj$setValues(d[, 2])
  
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
