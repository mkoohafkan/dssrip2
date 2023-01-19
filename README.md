# DSS R Interface Project (2)

A cannibilizationa and rewrite of
[dssrip](https://github.com/eheisman/dssrip). Support for additional
DSS data types and writing DSS data are planned.
 
 # Setup

You can install the `dssrip2` directly using

```r
remotes::install_github("mkoohafkan/dssrip2")
```

 ## Existing HEC-DSSVue install

A 64-bit install of HEC-DSSVue and a 64-bit JDK are required. To load
DSS functionality, call

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

## Monolith

Support for Monolith is planned. 

```r
dss_install_monolith()
dss_connect(monolith = TRUE)
```

# Usage

## File connections

Connections to DSS files must be handled manually by the user. To
create a file connection, use

```r
filepath = system.file("extdata/test.dss", package = "dssrip2")
conn = dss_file(filepath)
```

To close the file connection, use the `$close()` or `$done()` methods:

```r
conn$close()
```

Dss files will usually close on their own after some period of
inactivity, but it is best practice for users to explicitly close
files when they are done working with them.

## Reading data

`dssrip2` automatically detects the data type being read, so the same function call can be used for reading time series and paired data.

```r
# time series
dss_read(conn, "/BRANDYWINE CREEK/WILMINGTON, DE/FLOW/01JAN1946/1DAY/USGS/")

# paired data container
dss_read(conn, "/BRANDYWINE CREEK/WILMINGTON, DE/FLOW-STAGE///GENERATED DATA PAIRS/")
```
