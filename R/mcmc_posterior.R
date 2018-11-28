# Functions to manipulate the posterior distribution of the MCMC runs

#' Reads MCMC runs
#'
#' Read Markov chain Monte Carlo (MCMC) runs saved as RDS files
#'
#' @param fileNames vector containg the file names
#'
#' @return global variables for each MCMC run as an R object
#' @export
#'
#' @examples
#' #TO DO
read_mcmc_rds <- function(fileNames){
  # Create a loop that reads each file in the fileNames vector and assign it
  # to an R object
  n = 1
  while (n < length(fileNames) + 1){
    # read data
    job <- readRDS(fileNames[n])
    assign(paste("r", n, sep=""), job, envir = .GlobalEnv)
    n = n + 1
  }
}

#' Get posterior distributions of MCMC runs
#'
#' Gets posterior distributions of the Markov chain Monte Carlo (MCMC) runs
#'
#' @param run object of class "mcmcSamplerList" and "bayesianOutput"
#' @param burnin intenger representing number of samples to remove as burnin
#' @param rep string representing replicate of MCMC run
#' @param par_names vector containinig the parameter names
#'
#' @return a dataframe containing posterior distributions for each paramater
#'    in the model, with an additional column indicating the replicate number.
#' @export
#'
#' @examples
#' # TO DO
get_posterior <- function(run, burnin, rep, par_names){

  # get samples for each merged runs

  run.s <- as.data.frame(BayesianTools::getSample(run, start = burnin))
  names(run.s) <- par_names
  run.s["Rep"] <- rep

  return(run.s)

}

#' Get posterior distribution samples for the demographic model
#'
#' Solve the demographic model for n combination of parameter values from the
#' posterior distribution
#'
#'
#' @inheritParams get_posterior
#' @param n integer representing the number of samples to get from the
#'    posterior distribution
#' @param THETA list of parameter template used when running the MCMC analysis
#'
#' @seealso \code{\link{post_traj_mx}}
#'
#' @return a list of solved objects for posterior distributions and MAP (maximum
#'    a posteriori)
#' @export
#'
#' @examples
#' # TO DO
posterior_trajectories <- function(run, burnin, n, THETA){
  run.s <- BayesianTools::getSample(run, start = burnin)
  run_map <- BayesianTools::MAP(run, start = burnin)$parametersMAP

  # sampling n combination of parameters from the posterior
  run.n <- run.s[sample(nrow(run.s), size = n, replace=FALSE), ]

  # solve the demographic model for n combination of parameter values
  run_o.n <- do.call("cbind", apply(run.n, 1, post_traj_mx, THETA))
  # solve the demographic model for MAP (maximum a posteriori)
  run_map_o <- post_traj_mx(parameters = run_map, THETA = THETA)

  return(list(run = run_o.n, MAP = run_map_o))
}

#' Create dataframe for PAF
#'
#' Creates a dataframe for population attributable fraction (PAF). This
#' dataframe have information for median and lower and upper bounds for PAF
#' by group (gpm, gpf and msm).
#'
#' @param births.p list for birth elements (resulted from calculating the
#'    demographic model)
#' @param births.map list for birth elements for MAP (Maximum a posteriori)
#' @param times  vector of time points associated to the birth lists
#'
#' @return dataframe of median and upper, lower bounds for the posterior, and
#'    PAF for MAP
#' @export
#'
#' @examples
#' #TO DO
df_pafs <- function(births.p, births.map, times){

  # calculate the median (m) and quantiles (q) for pafs
  mq <- births_pafs(births.p, times)
  # calculate pafs for MAP
  mq_map <- t(sapply(births.map, calculate_pafs))

  #convert it to dataframe and transform to long format
  map.df <- as.data.frame(mq_map)
  map.df.m <- reshape2::melt(map.df)

  #add map to dataframe
  mq["MAP"] <- map.df.m$value

  return(mq)
}

#' Create dataframe for effective number of infections
#'
#' Creates a dataframe for effective number of infections. This
#' dataframe have information for median and lower and upper bounds for the
#' effective number of infections by group (gpm, gpf and msm).
#'
#' @param sizes.p list for size elements (resulted from calculating the
#'    demographic model)
#' @param sizes.map list for size elements for MAP (maximum a posteriori)
#' @param times vector of times points associated to the size list
#' @inheritParams reorganize_deme_sizes
#'
#' @return dataframe of median, and upper and lower bounds for the posterior,
#'    and for MAP
#' @export
#'
#' @examples
#' #To DO
df_sizes <- function(sizes.p, sizes.map, times, Nrep, Ntime){

    #re-organize demes by sizes element
  o.sizes.p <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                     sizes = sizes.p)

  # get the dataframe to plot tajectories for sizes (median and quantiles)
  mq <- median_and_quantiles(o.sizes.p, times)
  mq["group2"] <- ifelse(mq$group == "msm", "msm", "gp")

  # calculate sizes for MAP
  o.sizes.map <- reorganize_deme_sizes(Nrep = 1, Ntime = Ntime,
                                       sizes.map)

  #convert it to dataframe and transform to long format
  map.df <- data.frame(gpm=o.sizes.map[[1]][1:1000],
                       gpf=o.sizes.map[[2]][1:1000],
                       msm=o.sizes.map[[3]][1:1000])
  map.df.m <- reshape2::melt(map.df)

  #add map to dataframe
  mq["MAP"] <- map.df.m$value

  return(mq)

}

#' Create dataframe for the proportion of effective number of infections
#'
#' Creates a dataframe for the proportion of effective number of infections.
#' This dataframe have information for median, lower and upper bounds for the
#' proportions of effective number of infections by group (gpm, gpf and msm).
#'
#' @inheritParams df_sizes
#'
#' @return dataframe of median, upper and lower bounds for the posterior,
#'    and for MAP
#' @export
#'
#' @examples
#' #To DO
df_sizes_prop <- function(sizes.p, sizes.map, times, Nrep, Ntime){

  #re-organize demes by sizes element
  o.sizes.p <- reorganize_deme_sizes(Nrep = Nrep, Ntime = Ntime,
                                     sizes = sizes.p)

  # get the dataframe to plot tajectories for sizes (median and quantiles)
  mq <- prop_infected(o.sizes.p, times)
  mq["group2"] <- ifelse(mq$group == "msm", "msm", "gp")

  # calculate sizes for MAP
  o.sizes.map <- reorganize_deme_sizes(Nrep = 1, Ntime = Ntime,
                                       sizes.map)

  total_by_time <- Reduce("+", o.sizes.map)
  prop_infect <- lapply(o.sizes.map, function(x) x/total_by_time)

  #convert it to dataframe and transform to long format
  gpm.map <- as.data.frame(t(prop_infect$gpm))
  gpm.map["group"] <- "gpm"
  gpm.map["group2"] <- "gp"

  gpf.map <- as.data.frame(t(prop_infect$gpf))
  gpf.map["group"] <- "gpf"
  gpf.map["group2"] <- "gp"

  msm.map <- as.data.frame(t(prop_infect$msm))
  msm.map["group"] <- "msm"
  msm.map["group2"] <- "msm"

  map.df <- rbind(gpm.map, gpf.map, msm.map)
  names(map.df) <- c("MAP", "group", "group2")


  #add map to dataframe
  mq["MAP"] <- map.df$MAP

  return(mq)

}


#' Create dataframe for new cases of HIV
#'
#' Creates a dataframe for new cases of HIV. This
#' dataframe have information for median and lower and upper bounds for new HIV
#' cases by group (gpm, gpf and msm).
#'
#' @param newCases.p list for birth elements (resulted from calculating the
#'    demographic model)
#' @param newCases.map list for birth element for MAP (Maximum a posteriori)
#' @param times vector of time points associated to the birth list
#'
#' @return dataframe of median, and upper and lower bounds for the posterior,
#'    and the same for MAP
#' @export
#'
#' @examples
#' #TO DO
df_newCases <- function(newCases.p, newCases.map, times){

  # calculate the median and quantiles for new cases
  mq <- births_newCases(newCases.p, times)
  # calculate new cases for MAP
  mq_map <- t(sapply(newCases.map, calculate_newCases))

  #convert it to dataframe and transform to long format
  map.df <- as.data.frame(mq_map)
  map.df.m <- reshape2::melt(map.df)

  #add map to dataframe
  mq["MAP"] <- map.df.m$value

  return(mq)
}

#' Create dataframe for basic reproduction number
#'
#' Create dataframe for basic reproduction number (R0)
#'
#' @inheritParams get_posterior
#' @inheritParams gpspline
#' @param times vector of time points
#' @param GAMMA double representing the stage of infection
#'
#' @return dataframe of median, and upper and lower bounds for the posterior,
#'    and MAP
#' @details Basic Reproduction Number (R0) is calculated based on the equation
#'    \eqn{R0 = \beta / \gamma}. In our model \eqn{\gamma = 0.1} and \eqn{\beta}
#'     is represented by each of the linear function parameters
#'     (ex: gpsp0, gpsp1, etc). \eqn{\beta} is the number of transmissions per
#'     infected individuals.
#' @export
#'
#' @examples
#' #TO DO
df_r0 <- function(run, burnin, par_names, times, T0, T1, GAMMA){

  map <- BayesianTools::MAP(run, start =  burnin)
  map.p <- as.numeric(format(round(map$parametersMAP, 3), nsmall = 3))
  names(map.p) <- par_names

  map.gp <- as.data.frame(sapply(times, gpspline, T0 = T0, T1 = T1,
                                 parms = as.list(map.p[1:4])))
  names(map.gp) <- "MAP"
  map.msm <- as.data.frame(sapply(times, msmspline, T0 = T0, T1 = T1,
                                  parms = as.list(map.p[5:8])))
  names(map.msm) <- "MAP"

  map.all <- rbind(map.gp, map.msm)
  map.all <- map.all/GAMMA


  #POSTERIOR
  run.s <- BayesianTools::getSample(run, start =  burnin)
  run.p <- run.s[sample(nrow(run.s), size=1000, replace=FALSE), ]
  run.p <- as.data.frame(run.p[,1:8])
  names(run.p) <- par_names[1:8]

  run.p.gp <- apply(run.p[,1:4], 1, function(x) sapply(times, gpspline,
                                                       T0 = T0,
                                                       T1 = T1,
                                                       parms = as.list(x)))
  run.p.gp.r <- run.p.gp/GAMMA
  run.p.msm <- apply(run.p[,5:8], 1, function(x) sapply(times, msmspline,
                                                        T0 = T0,
                                                        T1 = T1,
                                                        parms = as.list(x)))
  # Here numbers are transformerd to R0 (basic reproduction number)
  run.p.msm.r <- run.p.msm/GAMMA

  # Calculates median and quantiles for R0 using the posterior distribution
  run.sp <- mq_r0(run.p.gp.r, run.p.msm.r, times)
  run.sp["MAP"] <- map.all

  return(run.sp)

}


#' Calculates median and quantiles for basic reproduction number
#'
#' @param data.gp matrix for basic reproduction number for general population
#' @param data.msm matrix for basic reproduction number for msm
#' @inheritParams df_r0
#'
#' @return dataframe of median, and upper and lower bounds for the posterior
#' @details in both matrices (data.gp and data.msm), each column
#'    represents a different replicate and each row represents a time point.
#'
#' @examples
#' #TO DO
mq_r0 <- function(data.gp, data.msm, times){
    #calculates median
    median.gp <- as.data.frame(apply(data.gp, 1, median))
    names(median.gp) <- "median"

    median.msm <- as.data.frame(apply(data.msm, 1, median))
    names(median.msm) <- "median"

    #calculates quantiles
    quantiles.gp <- as.data.frame(t(apply(data.gp, 1,
                                          function(x)
                                            quantile(x, probs=c(0.025, 0.975)))))
    names(quantiles.gp) <- c("lower", "upper")


    quantiles.msm <- as.data.frame(t(apply(data.msm, 1,
                                           function(x)
                                             quantile(x, probs=c(0.025, 0.975)))))
    names(quantiles.msm) <- c("lower", "upper")

    #bind dataframes together
    gp.c <- cbind(median.gp, quantiles.gp)
    gp.c["group"] <- "gp"
    msm.c <- cbind(median.msm, quantiles.msm)
    msm.c["group"] <- "msm"

    all.c <- rbind(gp.c, msm.c)
    all.c <- cbind(times, all.c)

    all.c$group <- as.factor(all.c$group)

    return(all.c)
}

#' Gets the proportion of infections in deme1 atributable to deme2
#'
#' Gets the proportion of infections in one deme attributable to another deme,
#' and calculates the median, lower and upper bounds.
#'
#'
#' @inheritParams df_pafs
#' @param r row number for deme2
#' @param c column number for deme1
#'
#' @details row and column numbers will determine which proportion of infections
#'    in one deme is attributable to onother deme. For example, for our analysis
#'    for the Senegal data, \eqn{r = 3} and \eqn{c = 2} represent the proportion
#'    of infections in gpf attributable to msm
#'
#' @return dataframe of median, and lower and upper bounds for the proportion of
#'    infections in one deme attributable to another deme
#' @export
#'
#' @examples
#' #TO DO
deme2deme <- function(births.p, times, r, c){

  # gets element that corresponds to the interested deme 2 deme, for example,
  # msm to gpf (proportion of infections in gpf attributable to msm)
  deme.n <- purrr::map(births.p, function(x) purrr::map(x, function(y)
                                      round(y[r,c], 6)/round(sum(y[1:3,1:3]), 6)))

  # convert deme.n to matrix
  # matrix will have 1000 row, each row corresponds to a time point
  # each column corresponds to a replicate
  deme.l <- map(deme.n, unlist)

  # in this matrix, each row is a time point
  # and each column is a replicate (from a MCMC posterior)
  deme.m <- do.call("cbind", deme.l)

  #calculates the median
  deme.median <- as.data.frame(apply(deme.m, 1, median))
  names(deme.median) <- "median"
  #calculates the quantiles
  deme.qt <- as.data.frame(t(apply(deme.m, 1, function(x)
                                          quantile(x, probs=c(0.025, 0.975)))))
  names(deme.qt) <- c("lower", "upper")

  #dataframe in which each row corresponds to a time point.
  deme.df <- cbind(times, deme.median, deme.qt)

  return(deme.df)
}

#' Calculates the proportion of infected individuals
#'
#' @param values dataframe of absolute numbers for median, lower and upper
#'    bound for each deme
#' @inheritParams df_sizes
#'
#' @return dataframe of the proportions of infected as median, lower and
#'    upper bounds
#' @export
#'
#' @examples
#' # TO DO
prop_infected <- function(values, times){

  # sum the number of gpm, gpf, msm by time and by replicate
  # so this object can then be used to calculate the proportions of
  # infected individuals by time.
  total_by_time <- Reduce("+", values)

  # get the proportions of infected gpm, gpf, and msm by time and by replicate
  # each column in each matrix is a time point
  # each row in each matrix is a replicate (MCMC posterior)
  prop_infect <- lapply(values, function(x) x/total_by_time)

  #median
  median_all <- lapply(prop_infect, function(x) apply(x, 2, median))
  median_all <- lapply(median_all, function(x) matrix(x))

  #credible interval
  #lower
  l_quantiles <- lapply(prop_infect, function(x) apply(x, 2, quantile, probs=0.025))
  l_quantiles <- lapply(l_quantiles, function(x) matrix(x))
  #upper
  u_quantiles <- lapply(prop_infect, function(x) apply(x, 2, quantile, probs=0.975))
  u_quantiles <- lapply(u_quantiles, function(x) matrix(x))

  # separates median, lower and upper bounds by deme (gpm, gpf, and msm)
  gpm <- as.data.frame(cbind(median_all$gpm, l_quantiles$gpm, u_quantiles$gpm))
  gpm["group"] <- "gpm"
  names(gpm) <-c("median", "lower", "upper", "group")

  gpf <- as.data.frame(cbind(median_all$gpf, l_quantiles$gpf, u_quantiles$gpf))
  gpf["group"] <- "gpf"
  names(gpf) <-c("median", "lower", "upper", "group")

  msm <- as.data.frame(cbind(median_all$msm, l_quantiles$msm, u_quantiles$msm))
  msm["group"] <- "msm"
  names(msm) <-c("median", "lower", "upper", "group")

  all.info <- rbind(gpm, gpf, msm)
  all.info <- cbind(times, all.info)


  return(all.info)
}
