#' DSS Time Vector
#'
#' Extract timestamps from a DSS object.
#'
#' @param times A vector of DSS times.
#' @param info A list of attributes relevant to processing timestamps.
#'   Must include "timeGranularitySeconds", "timeIntervalSeconds",
#'   "timeZoneRawOffset", and "type".
#' @return A POSIXct vector.
#'
#' @importFrom lubridate force_tz
#' @keywords internal
dss_times_to_posix = function(times, info) {
  granularity = as.numeric(info[["timeGranularitySeconds"]])
   timezone = dss_timezone(info[["timeZoneRawOffset"]])
   as.POSIXct(granularity * (times), origin = DSS_ORIGIN,
     tz = timezone)
}


#' DSS Time Zone
#'
#' Convert DSS raw timezone offset to an R timezone.
#'
#' @param tzoffset The DSS object's "timeZoneRawOffset" attribute
#'   (time offset in milliseconds).
#' @return An R timezone string in format "etc/GMT*".
#'
#' @keywords internal
dss_timezone = function(tzoffset) {
  if (!isTRUE(nzchar(tzoffset))) {
    tzoffset = 0L
  } else if ((tzoffset %% 3600000L) > 0L) {
    warning("timezone offset is fractional hours. ",
      "Defaulting to ", DSS_TIMEZONE, ".", call. = FALSE)
    tzoffset = 0L
  }
  offset_hours = tzoffset %/% 3600000L
  tzsign = ifelse(offset_hours > 0L, "-", "+")
  paste0("etc/GMT", tzsign, abs(offset_hours))
}


#' Format Datetimes
#'
#' Helper function to format datetimes.
#'
#' @param x A vector to be coerced to datetimes
#' @param default_tx The default timezone to use for coercing
#'   objects without a specified timezone.
#' @return A vector of datetimes.
#'
#' @importFrom lubridate as_datetime tz force_tz
#' @keywords internal
format_datetimes = function(x, default_tz = "etc/GMT+0") {
  if (!inherits(x, "POSIXct")) {
    warning("No timezone set for argument \"x\". Using ",
      default_tz, ".", call. = FALSE)
    as_datetime(x, tz = default_tz)
  } else if (!nzchar(tz(x))) {
    warning("No timezone set for argument \"x\". Using ",
      default_tz, ".", call. = FALSE)
    force_tz(x, tzone = default_tz)
  } else {
    x
  }
}


#' Fill NA Values
#'
#' Fill NA values with the previous (or following) value.
#'
#' @param v A vector.
#' @param forward If `TRUE`, fill starting from first to last element,
#'   If `FALSE`, fill in from last to first element.
#' @return The vector `v`, with NA values filled.
#'
#' @export
na_fill = function(v, forward = TRUE) {
  if (!forward) {
    dir_fun = rev
  } else {
    dir_fun = identity
  }
  v = dir_fun(v)
  i = c(TRUE, !is.na(v[-1]))
  dir_fun(v[i][cumsum(i)])[seq_along(v)]
}

#' Format NA Values
#'
#' Replace the DSS placeholder NA value with R NA values.
#'
#' @param x A numeric vector.
#' @return The vector `x`, possibly with NA values
#'
#' @keywords internal
format_na = function(x) {
  na_vec = rep(DSS_MISSING_VALUE, length(x))
  ifelse(abs(x - na_vec) < 1e-5, NA, x)
}
