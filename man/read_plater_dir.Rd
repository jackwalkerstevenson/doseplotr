% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{read_plater_dir}
\alias{read_plater_dir}
\title{Read an entire directory of \code{plater}-formatted CSVs without processing}
\usage{
read_plater_dir(dir)
}
\arguments{
\item{dir}{The directory from which to import all CSV files.}
}
\value{
A data frame containing the combined data from all the files.
}
\description{
\code{read_plater_CSVs()} attempts to use \code{plater::read_plates()} to import every
CSV file in the given directory. Will only work if every CSV file is in
proper \code{plater} format. Does not process the data. Throws an error if no CSV
files are present.
}
