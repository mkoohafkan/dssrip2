TSC_TYPES = c("INST-VAL", "INST-CUM", "PER-AVER", "PER-CUM")

minutes = c(1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30)

hours = c(1, 2, 3, 4, 6, 8, 12)

#' @importFrom stats setNames
TSC_INTERVALS = setNames(
  c(minutes, 60 * hours, 60 * 24 * c(1, 7, 10, 15, 30, 365), rep(0, 5)),
## Irregular appears to have interval of 0, not -1
  c(paste0(minutes, "MIN"), paste0(hours, "HOUR"),
    "1DAY", "1WEEK", "TRI-MONTH", "SEMI-MONTH", "1MON", "1YEAR",
    paste0("IR-", c("DAY", "MON", "YEAR", "DECADE", "CENTURY"))))

# used to help with introspection on Java Objects
sigConversions = list(boolean = "Z", byte = "B", char = "C",
  short = "T", void = "V", int = "I", long = "J", float = "F",
  double = "D")

DSS_MISSING_VALUE = -3.402823e+38

DSS_PARTS = LETTERS[1:6]

DSS_ORIGIN = "1899-12-31 00:00:00"