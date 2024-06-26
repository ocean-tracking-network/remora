% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_test_dist_release.R
\name{qc_test_dist_release}
\alias{qc_test_dist_release}
\title{QC Distance Release test}
\usage{
qc_test_dist_release(data, qc_results, distance_threshold = 500)
}
\arguments{
\item{data}{data formatted by either \code{remora::get_data} or \code{remora::get_data_arbitrary}}

\item{qc_results}{the \code{temporal_outcome} object, holding all QC test results,
generated by \code{remora::qc}}

\item{distance_threshold}{...}
}
\value{
adds Distance Release test outcomes to temporal_outcome object
}
\description{
...
}
\details{
...
}
\keyword{internal}
