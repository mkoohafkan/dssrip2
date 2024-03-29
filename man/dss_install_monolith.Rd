% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monolith.r
\name{dss_install_monolith}
\alias{dss_install_monolith}
\title{Install Monolith}
\usage{
dss_install_monolith(install_path, requirements_file, overwrite = TRUE)
}
\arguments{
\item{install_path}{The directory to download the monolith libraries to.}

\item{requirements_file}{The list of required assets. For expert use only.}

\item{overwrite}{If \code{TRUE}, delete any existing data in
\code{install_path} and recreate.}
}
\description{
Download the HEC Monolith Libraries.
}
\seealso{
\code{\link[=dss_connect]{dss_connect()}}
}
