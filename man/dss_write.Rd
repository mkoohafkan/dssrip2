% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.r
\name{dss_write}
\alias{dss_write}
\title{Write DSS Records}
\usage{
dss_write(x, filename, path)
}
\arguments{
\item{x}{The dataframe (Time Series or Paired Data) or raster (Grid)
to write to DSS. Must include attribute \code{"dss_attributes"}
containing required attributes.}

\item{filename}{A DSS file path.}

\item{path}{The DSS path to write.}
}
\description{
Write a DSS time series, paired dataset, or grid to a DSS file.
}
\seealso{
\code{\link[=dss_read]{dss_read()}} \code{\link[=dss_catalog]{dss_catalog()}} \code{\link[=dss_attributes]{dss_attributes()}}
}
