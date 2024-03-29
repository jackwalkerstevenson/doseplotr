% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aesthetic_utils.R
\name{minor_breaks_log}
\alias{minor_breaks_log}
\title{Generate minor breaks for an axis in log units}
\usage{
minor_breaks_log(min_val, max_val)
}
\arguments{
\item{min_val}{The minimum value to plot, or log floor of same, in log-transformed units (-8.3 and -8 will both be converted to -8).}

\item{max_val}{The maximum value to plot, or log ceiling of same, in log-transformed units (-4.5 and -4 will both be converted to -4).}
}
\value{
A vector of minor axis breaks in log-transformed units.
}
\description{
This function generates logistic minor axis breaks for an axis that uses
log-transformed units, such that the data being plotted is e.g. -3 rather
than 0.001.
}
\examples{
minor_breaks_log(-12, -4)
minor_breaks_log(-12.2, -3.6)
}
