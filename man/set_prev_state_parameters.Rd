% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prevalence.R
\name{set_prev_state_parameters}
\alias{set_prev_state_parameters}
\title{Set states for prevalence values}
\usage{
set_prev_state_parameters(n = 1e+06)
}
\value{
mean (MEAN_PREV_STAT) and standard deviation (SD_PREV_STAT)
for prevalence
}
\description{
Function that sets the states for the prevalence values to calculate mean
and standard deviation to be used in MCMC runs
}
\details{
\itemize{
   \item q = proportion of males who are msm
   \item p_msm = msm HIV prevalence
   \item p_m = heterosexual male (gpm) prevalence
   \item X = proportion of infected gpm who are msm (assumes that the estimation
of p_msm is independent of estimated prevalence )
}
}
\examples{
#TO DO
}
