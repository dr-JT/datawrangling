% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim.R
\name{trim}
\alias{trim}
\title{Trim scores}
\usage{
trim(x, variables, cutoff = 3.5, replace = "NA", id = "")
}
\arguments{
\item{x}{dataframe}

\item{variables}{variables c() to be trimmed}

\item{cutoff}{zscore cutoff to use for trimming (default: 3.5)}

\item{replace}{What value should the trimmed values be replaced with. (default: replace = "NA")}
}
\description{
This function will trim variable(s) based on a zscore cutoff
}
\examples{
trim(x, variables = c(), cutoff = 3.5)
}
\keyword{trim}
