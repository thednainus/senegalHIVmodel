% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscellaneous.R
\name{births_mq}
\alias{births_mq}
\title{Calculate median and quantiles when using processed object "births".}
\usage{
births_mq(birth_list, times)
}
\arguments{
\item{birth_list}{list for information processed from births object from the
solved demographic object. This can be processed either
by \code{\link{calculate_pafs}} or \code{\link{calculate_newCases}}}

\item{times}{time points associated to the birth list}
}
\value{
data.frame object in which columns correspond to times, median,
  lower and upper quantiles. Two additional columns are also provided
  to group data by gpm, gpf, and msm; and by gp and msm (for plotting
  purposes)
}
\description{
Calculate median and quantiles when using processed object "births".
}
\examples{
# TO DO
}
