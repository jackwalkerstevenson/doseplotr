% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess.R
\name{make_log_dose}
\alias{make_log_dose}
\title{Create a log_dose column from dose_uM or dose_nM columns}
\usage{
make_log_dose(df)
}
\arguments{
\item{df}{A dataframe containing either a column called "dose_nM" or a column
called "dose_uM", representing dose in nanomolar or micromolar units
respectively.}
}
\value{
The same dataframe with an additional column "log_dose" representing
dose in log10(molar) units.
}
\description{
\code{make_log_dose()} takes a dataframe that contains a column called "dose_uM"
or "dose_nM" and creates a column called log_dose" from
}
\examples{
df <- data.frame("treatment" = c("foo", "bar", "baz"),
                 "dose_nM" = c(1, 10, 100))
make_log_dose(df)
}
