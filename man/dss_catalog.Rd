% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paths.r
\name{dss_catalog}
\alias{dss_catalog}
\title{Get DSS Paths}
\usage{
dss_catalog(filename, pattern = ".*", condensed = TRUE, rebuild = FALSE)
}
\arguments{
\item{filename}{A DSS file path.}

\item{pattern}{character string containing a regular expression to
be matched against the DSS pathname catalog.}

\item{condensed}{If \code{TRUE} returned the condensed path catalog.}

\item{rebuild}{If \code{TRUE}, force rebuild the catalog.}
}
\value{
A vector of paths.
}
\description{
Get a list of DSS paths in a file.
}
\section{Java Virtual Machine Parameters}{

When working with very large DSS files, you may need to initialize
the Java Virtual Machine (JVM) with a larger heap size. For more
information see the help vignette:
\code{vignette("Java Virtual Machine Parameters", package = "dssrip2")}
}

\seealso{
\code{\link[=dss_read]{dss_read()}} \code{\link[=dss_parts_split]{dss_parts_split()}}
\code{\link[=dss_parts_combine]{dss_parts_combine()}} \code{\link[=dss_parts_replace]{dss_parts_replace()}}
}
