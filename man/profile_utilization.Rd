% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profile.R
\name{profile_utilization}
\alias{profile_utilization}
\title{Profiles utilization metrics vs. national sample}
\usage{
profile_utilization(df, aco_num, year = NULL)
}
\arguments{
\item{df}{SSP data}

\item{aco_num}{ACO Number.}

\item{year}{MSSP performance year.}
}
\value{
Data frame with mssp data.
}
\description{
Profiles utilization metrics vs. national sample
}
\examples{
profile_utilization(df, "A95164", 2016)
}
