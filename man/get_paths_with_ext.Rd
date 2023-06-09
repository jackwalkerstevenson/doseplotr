% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{get_paths_with_ext}
\alias{get_paths_with_ext}
\title{Get all paths to files with a specific extension in a directory}
\usage{
get_paths_with_ext(dir, ext)
}
\arguments{
\item{dir}{Path to the directory in which to get files}

\item{ext}{Extension for which to find files, e.g. "xls". Note this is a
regex, so "xls" will find both .xls and .xlsx files.}
}
\value{
A vector of file paths with the given extension.
}
\description{
Get all paths to files with a specific extension in a directory
}
