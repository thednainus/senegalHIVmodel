# Functions to allow to add a likelihood term to the Senegal model
# by adding information on prevalence used in Christine's paper
#' Converts size for point estimate 2010 to prevalence of males that are msm
#'
#' @param tfgy object of class tfgy. See demographic function (TO DO)
#'
#' @return numeric msm prevalence value
#' @export
#'
#' @examples
#' #To do
tfgy2prevalenceStat <- function(tfgy)
{
  times <- tfgy[[1]]
  #gets the index of the first minimum
  #in this case, gets the location for the year 2010 in object times
  i2010 <- which.min(abs(times - 2010 ))
  #get the object sizes for time point 2010
  y2010 <- tfgy[[4]][[i2010]]
  # gets the proportion of males that are msm in time point 2010
  y2010['msm'] / ( y2010['msm'] + y2010['gpm'] )
}


#' Set states for prevalence values
#'
#' Function that sets the states for the prevalence values to calculate mean
#' and standard deviation to be used in MCMC runs
#' q = proportion of males who are msm
#' p_msm = msm HIV prevalence
#' p_m = heterosexual male (gpm) prevalence
#' X = proportion of infected gpm who are msm (assumes that the estimation
#' of p_msm is independent of estimated prevalence )
#'
#' @return mean (MEAN_PREV_STAT) and standard deviation (SD_PREV_STAT)
#' for prevalence
#' @export
#'
#' @examples
#' #TO DO
set_prev_state_parameters <- function(n = 1e6)
{
  # q = proportion of males who are msm
  q <- 0.012

  # p_msm = msm HIV prevalence
  p_msm <- rnorm(n, 0.297, sd = (0.381 - 0.213) / 3.92) #for 2016
  p_msm <- pmin( pmax( p_msm, .213 ), .381)

  # p_m = male HIV prevalence
  p_m <- rnorm(n, 0.004, sd = (0.008 - 0.0014) / 3.92) #for 2010
  p_m <- pmin( pmax(p_m, .0014), .008)

  # X = proportion of infected men who are msm
  X <- q * p_msm / (q * p_msm + (1-q) * p_m)

  MEAN_PREV_STAT <<- mean(X)
  SD_PREV_STAT <<- sd(X)
}


#' Density function for the prevalence statistics
#'
#' Calculates the density function for the proportion of infected men who are
#' msm
#'
#' @param x is of class tfgy (after using the demographic function TO DO )
#'
#' @return density value for prevalence statistics
#' @export
#'
#' @examples
#' #TO DO
prPrevalenceStat <- function(x)
{
  dnorm(x, mean = MEAN_PREV_STAT, sd = SD_PREV_STAT, log = TRUE)
}
