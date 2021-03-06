% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscellaneous.R
\name{median_and_quantiles}
\alias{median_and_quantiles}
\title{Calculates the median and quantiles for each deme size}
\usage{
median_and_quantiles(sizes_list, times)
}
\arguments{
\item{sizes_list}{list containg the reorganized element size (effective number
of infections).}

\item{times}{vector containig the time points used in the simulations}
}
\value{
dataframe for median and quantiles for each deme (gpm, gpmf and msm)
}
\description{
Calculates the median and quantiles (0.025 and 0.975) for each deme for the
reorganized element size (effective number of infections) of the solved
demographic model. See \code{\link{reorganize_deme_sizes}}.
}
\examples{
#TO DO
}
