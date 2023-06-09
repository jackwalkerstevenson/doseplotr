% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{filter_trt_tgt}
\alias{filter_trt_tgt}
\title{Filter data to include only given treatments and targets}
\usage{
filter_trt_tgt(
  data,
  trt = NULL,
  tgt = NULL,
  trt_col = "treatment",
  tgt_col = "target"
)
}
\arguments{
\item{data}{A dataframe containing the following columns:
\itemize{
\item "treatment" (default) or other treatment column name (see \code{trt_col})
\item "target" (default) or other target column name (see \code{tgt_col})
}}

\item{trt}{Name of treatments to use in the model. String or character vector.}

\item{tgt}{Name of targets to use in the model. String or character vector.}

\item{trt_col}{Name of the column containing treatments. Default is
"treatment". Typical alternatives might include e.g. "drug", "additive",
"supplement".}

\item{tgt_col}{Name of the column containing targets. Default is "target".
Typical alternatives might include e.g. "mutant", "cell_line", "species".}
}
\value{
A dataframe containing only rows filtered for the desired treatments
and targets.
}
\description{
Filter data to include only given treatments and targets
}
\examples{
df <- data.frame(treatment = c("foo", "foo", "foo", "bar", "baz"),
                 target = c("apple", "orange", "banana", "apple", "orange"))
filter_trt_tgt(df, "foo", "orange")
}
