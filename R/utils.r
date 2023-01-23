#' DSS Time Vector
#'
#' Extract timestamps from a DSS object.
#'
#' @param times A vector of DSS times.
#' @param metadata The metadata associated with `obj`, i.e.,
#'   output of [java_metadata()].
#' @return A POSIXct vector.
#'
#' @keywords internal
dss_times = function(times, metadata, offset) {
  granularity = as.numeric(metadata[["timeGranularitySeconds"]])
  if(offset && grepl("^PER-", metadata[["type"]])) {
    time_offset = -as.numeric(metadata[["interval"]])
  } else {
    time_offset = 0
  }
   timezone = dss_timezone(metadata[["timeZoneRawOffset"]])
   as.POSIXct(times * granularity + time_offset, tz = timezone,
     origin = DSS_ORIGIN)
}


#' DSS Time Zone
#'
#' Convert DSS raw timezone offset to an R timezone.
#'
#' @param tzoffset The DSS object's "timeZoneRawOffset" attribute
#'   (time offset in milliseconds).
#' @return An R timezone string, either "UTC" or "Etc/GMT*".
#'
#' @keywords internal
dss_timezone = function(tzoffset) {
  if (!isTRUE(nzchar(tzoffset))) {
    tzoffset = 0L
  } else if ((tzoffset %% 3600000L) > 0L) {
    warning("timezone offset is fractional hours. ",
      "Defaulting to 'etc/GMT+0' (UTC).")
    tzoffset = 0L
  }
  offset_hours = tzoffset %/% 3600000L
  tzsign = ifelse(offset_hours > 0L, "-", "+")
  paste0("etc/GMT", tzsign, abs(offset_hours))
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
