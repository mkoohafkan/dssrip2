#' DSS Time Vector
#'
#' Extract timestamps from a DSS object.
#'
#' @param obj A DSS Java object.
#' @param metadata The metadata associated with `obj`, i.e.,
#'   output of [java_metadata()].
#' @return A POSIXct vector.
#'
#' @keywords internal
dss_times = function(obj, metadata, offset) {
  granularity = as.numeric(metadata[["timeGranularitySeconds"]])
  if(offset && grepl("^PER-", metadata[["type"]])) {
    time_offset = -as.numeric(metadata[["interval"]])
  } else {
    time_offset = 0
  }
   timezone = dss_timezone(metadata[["timeZoneRawOffset"]])
   as.POSIXct(obj$times * granularity + time_offset, tz = timezone,
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
  if (!nzchar(tzoffset)) {
    "UTC"
  } else if ((tzoffset %% 3600000L) > 0L) {
    warning("timezone offset is fractional hours. Defaulting to UTC")
    "UTC"
  } else {
    offset_hours = tzoffset %/% 3600000L
    tzsign = ifelse(offset_hours > 0, "-", "+")
    paste0("etc/GMT", tzsign, abs(offset_hours))
  }
}
