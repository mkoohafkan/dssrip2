# DSS R Interface Project (2)

<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dssrip2)](http://cran.r-project.org/package=dssrip2)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/mkoohafkan/dssrip2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mkoohafkan/dssrip2/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A rewrite of [`dssrip`](https://github.com/eheisman/dssrip). Supports
reading and writing time series and paired data to DSS.
Read/write support for DSS grid data is planned.

Supported Operating Systems:
- Windows
- Linux
- MacOS

Supported HEC-DSSVue versions:
- HEC-DSSVue 2.0.1


## Setup

You can install the `{dssrip2}` directly using

```r
remotes::install_github("mkoohafkan/dssrip2")
```

To build the vignettes on install, use the argument `build_vignettes = TRUE`.

### Monolith

`{dssrip2}` can connect to HEC-Monolith libraries using

```r
dss_install_monolith()
dss_connect(monolith = TRUE)
```

If the option `dss.monolith` is set to `TRUE`,
`{dssrip2}` will interpret the
`dss.home` option as the HEC-Monolith directory path.
By default, `{dssrip2}` will install HEC-Monolith to
`%LOCALAPPDATA%` (Windows) or `~/.dssrip2/monolith` (Unix),
but this can be overridden.
`dss_connect()` will retrieve the path using `getOption("dss.home")` 
if no path is provided. 

To use Monolith automatically, set the following
options in your R profile:

```r
options(dss.monolith = TRUE)
```


### Existing HEC-DSSVue install

If you don't want to use the HEC-Monolith libraries, `{dssrip2}` can
connect to an existing HEC-DSSVue program installation instead.
To use this option, both a 64-bit install of HEC-DSSVue and a 64-bit
JDK are required. To load DSS functionality, call

```r
library(dssrip2)
dss_connect("path/to/HEC-DSSVue")
```

If the option `dss.monolith` is set to `FALSE` (the default),
`{dssrip2}` will interpret the `dss.home` option as the 
HEC-DSSVue application directory path.

To use an HEC-DSSVue installation automatically,
set the following option in your R profile:

```r
options(
  # replace with the install path on your machine
  dss.home = "C:/Program Files/HEC/HEC-DSSVue"
)
```

## DSS Messaging

To suppress messages from DSS, use

```r
dss_connect(message_level = 1L)

# or after calling dss_connect()
dss_message_level(1L)
```

If no message level is provided, `dss_message_level()` will retrieve
the level using `getOption("dss.messagelevel")`. You can also set
this option in your `.Rprofile`:

```r
options(
  dss.messagelevel = 1L
)
```


## Usage

### Creating Files

`{dssrip2}` can create DSS version 6 and 7 files:

```r
tf1 = tempfile(fileext = ".dss")
tf2 = tempfile(fileext = ".dss")

# create a DSS-7 file (default behavior)
dss_create(tf1)
# create a DSS-6 file
dss_create(tf2, version = 6)
```

`{dssrip2}` also provides methods to convert between DSS versions:

```r
tf3 = tempfile(fileext = ".dss")
dss_convert(tf2, tf3)
```


### Reading data

`{dssrip2}` automatically detects the data type being read, so the same
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

`{dssrip2}` deduces the data type to write based on the supplied
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
