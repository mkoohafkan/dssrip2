TSC_TYPES = c("INST-VAL", "INST-CUM", "PER-AVER", "PER-CUM")

TSC_INTERVALS = c("1MIN", "2MIN", "3MIN", "4MIN", "5MIN", "6MIN",
  "10MIN", "12MIN", "15MIN", "20MIN", "30MIN", "1HOUR", "2HOUR",
  "3HOUR", "4HOUR", "6HOUR", "8HOUR", "12HOUR", "1DAY", "1WEEK",
  "1MON", "SEMI-MONTH", "TRI-MONTH", "1YEAR", "IR-DAY", "IR-MON",
  "IR-YEAR", "IR-DECADE", "IR-CENTURY")

# used to help with introspection on Java Objects
sigConversions = list(boolean = "Z", byte = "B", char = "C",
  short = "T", void = "V", int = "I", long = "J", float = "F",
  double = "D")

DSS_MISSING_VALUE = -3.402823466385288603362e+38

DSS_PARTS = LETTERS[1:6]

DSS_ORIGIN = "1899-12-31 00:00:00"
DSS_TIMEZONE = "etc/GMT+0"
