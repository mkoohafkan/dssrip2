% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.r
\name{as_hectime}
\alias{as_hectime}
\title{POSIXct to HecTime}
\usage{
as_hectime(x, granularity_seconds)
}
\arguments{
\item{x}{A POSIXct timestamp.}

\item{granularity}{The minimum time interval.
Can be "secs" or "mins".}

\item{as_period}{If \code{TRUE}, adjust timestamp to represent a period,
with timestamp specifying period end time (0100 to 2400 hours).}
}
\value{
A Java \code{hec.heclib.util.HecTime} object.
}
\description{
Convert an R POSIXct datetime to Java HecTime.
}
\keyword{internal}
