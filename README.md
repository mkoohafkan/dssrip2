# DSS R Interface Project (2)

  <!-- badges: start -->
  ![CRAN Release](https://www.r-pkg.org/badges/version-last-release/dssrip2)
  [![R-CMD-check](https://github.com/mkoohafkan/dssrip2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mkoohafkan/dssrip2/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

A rewrite of [`dssrip`](https://github.com/eheisman/dssrip). Supports
reading and writing time series and paired data to DSS. Linux support
and read/write support for DSS grid data is planned.


## Setup

You can install the `dssrip2` directly using

```r
remotes::install_github("mkoohafkan/dssrip2")
```

### Monolith

`dssrip2` can connect to HEC-Monolith libraries using

```r
dss_install_monolith()
dss_connect(monolith = TRUE)
```

To use Monolith automatically, set the following
options in your R profile:

```r
options(dss.monolith = TRUE)
```

By default, `dssrip2` will install HEC-Monolith to your
`%LOCALAPPDATA%` folder, but this can be overridden. If the option
`dss.monolith` is set to `TRUE`, `dssrip2` will interpret the
`dss.home` option as the HEC-Monolith directory path.

### Existing HEC-DSSVue install

If you don't want to use the HEC-Monolith libraries, `dssrip2` can
connect to an existing HEC-DSSVue program installation instead.
To use this option, both a 64-bit install of HEC-DSSVue and a 64-bit
JDK are required. To load DSS functionality, call

```r
library(dssrip2)
dss_connect("path/to/HEC-DSSVue")
```

`dss_connect()` will retrieve the path using `getOption("dss.home")` 
if no path is provided. 

To suppress messages from DSS, use

```r
dss_connect(message_level = 1L)

# or after calling dss_connect()
dss_message_level(1L)
```

If no message level is provided, `dss_message_level()` will retrieve
the level using `getOption("dss.messagelevel")`.

You can set either or both of these options in your `.Rprofile`:

```r
# paste this into .Rprofile
# (replace with the install path on your machine)
options(
  dss.home = "C:/Program Files/HEC/HEC-DSSVue",
  dss.messagelevel = 1L
)
```


## Usage

### Creating Files

`dssrip2` can create DSS version 6 and 7 files:

```r
tf1 = tempfile(fileext = ".dss")
tf2 = tempfile(fileext = ".dss")

# create a DSS-7 file (default behavior)
dss_create(tf1)
# create a DSS-6 file
dss_create(tf2, version = 6)
```

`dssrip2` also provides methods to convert between DSS versions:

```r
tf3 = tempfile(fileext = ".dss")
dss_convert(tf2, tf3)
```


### Reading data

`dssrip2` automatically detects the data type being read, so the same
function call can be used for reading time series and paired data.

```r
exfile = system.file("extdata/example.dss", package = "dssrip2")

# time series
dss_read(exfile, "/BRANDYWINE CREEK/WILMINGTON, DE/FLOW/01JAN1946/1DAY/USGS/")

# paired data
dss_read(exfile, "/BRANDYWINE CREEK/WILMINGTON, DE/FLOW-STAGE///GENERATED DATA PAIRS/")
```

DSS files will usually close on their own after some period of
inactivity, but it is best practice for users to explicitly close the
file when they are finished:

```r
dss_close(exfile)
# or close all files opened by dssrip2 with dss_close_all()
```

### Writing data

`dssrip2` deduces the data type to write based on the supplied
R object, so the same function call can be used for reading time
series and paired data. However, writing DSS objects requires
additional attributes that must be explicitly supplied by the user:

```r
tf = tempfile(fileext = ".dss")
dss_create(tf)

# time series
data(Nile)
nile = data.frame(datetime = as.Date(sprintf("%d-10-01", time(Nile))),
  flow = as.vector(Nile) * 1e8)
nile = dss_add_attributes(nile, list(units = "cubic meters",
  type = "PER-CUM"))
path = "//NILE RIVER/VOLUME//1YEAR/R DATASET/"
dss_write(nile, tf, path)

# paired data
data(CO2)
uptake = reshape(CO2[c("Plant", "conc", "uptake")],
  direction = "wide", idvar = "conc", timevar = "Plant")
names(uptake) = gsub("uptake.", "", names(uptake))
uptake = dss_add_attributes(uptake, list(xtype = "LINEAR",
  ytype = "LINEAR", xunits = "mL/L", yunits = "umol/m^2"))
path = "//PLANT CO2 UPTAKE/CONCENTRATION-UPTAKE///R DATASET/"
dss_write(uptake, tf, path)
```
