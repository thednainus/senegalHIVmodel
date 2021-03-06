% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmc_posterior.R
\name{df_sizes_prop}
\alias{df_sizes_prop}
\title{Create dataframe for the proportion of effective number of infections}
\usage{
df_sizes_prop(sizes.p, sizes.map, times, Nrep, Ntime)
}
\arguments{
\item{sizes.p}{list for size elements (resulted from calculating the
demographic model)}

\item{sizes.map}{list for size elements for MAP (maximum a posteriori)}

\item{times}{vector of times points associated to the size list}

\item{Nrep}{integer for the number of replicates (number of combination of
parameter values based on the posterior distribution)}

\item{Ntime}{integer for the number of time points}
}
\value{
dataframe of median, upper and lower bounds for the posterior,
   and for MAP
}
\description{
Creates a dataframe for the proportion of effective number of infections.
This dataframe have information for median, lower and upper bounds for the
proportions of effective number of infections by group (gpm, gpf and msm).
}
\examples{
#To DO
}
