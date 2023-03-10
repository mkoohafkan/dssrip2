% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.r
\name{dss_close}
\alias{dss_close}
\title{Close DSS File}
\usage{
dss_close(file, implicit = FALSE)
}
\arguments{
\item{file}{A DSS file handle, e.g., output of \code{\link[=dss_open]{dss_open()}}.}

\item{implicit}{If \code{TRUE}, use method \code{done()} instead of \code{close()}.}
}
\description{
Close a DSS file. This function is provided for completion of the
interface but is not strictly required as these methods can be
called directly from the file object (i.e., \code{file$done()}).
}
\seealso{
\code{\link[=dss_open]{dss_open()}}
}
