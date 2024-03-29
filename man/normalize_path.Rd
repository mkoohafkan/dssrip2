% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.r
\name{normalize_path}
\alias{normalize_path}
\title{Normalize DSS File Path}
\usage{
normalize_path(filename, exists)
}
\arguments{
\item{filename}{A DSS file path.}

\item{exists}{If \code{TRUE}, assert that the DSS file exists.}
}
\value{
A normalized file path.
}
\description{
Normalize a DSS file path. A wrapper for \code{\link[base:normalizePath]{base::normalizePath()}}
with winslash set to \code{"/"}.
}
\details{
This function attempts to strictly resolve file paths
by traversing the file path back to the deepest existing
directory, in order to resolve potential issues with directory
names (e.g., differences in letter case).
}
\keyword{internal}
