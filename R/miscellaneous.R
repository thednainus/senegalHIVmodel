#' Setup Model Equations
#'
#' Function to setup the components of the mathematical model. See \code{\link[phydynR]{build.demographic.process}}
#' for more details
#'
#' @param demes a character vector naming the demes of the mathematical model.
#' @param nondemes a character vector naming the non demes of the mathematical model.
#' @param rcpp if TRUE, the expressions are interpreted as C code using the Rcpp package.
#'
#' @return this function returns a list containing the empty components
#'  (represented by zeros) to build the mathematical model.
#'  These components are the birth, death, migrations, total number of demes and
#'  non-demes of the model.
#'  \itemize{
#'      \item Birth is a matrix describing the model birth rates;
#'      \item Death is a vector describing the model death rates;
#'      \item Migration is a matrix describing the model migration rates.
#'  }
#'
#'
#' @seealso \code{\link[phydynR]{build.demographic.process}}
#'
#' @export
#'
#' @examples
#' demes <- c("gpm", "gpf", "msm", "src")
#' eqns <- setup.model.equations(demes)
setup.model.equations <- function(demes, nondemes = NULL, rcpp = FALSE)
{
  m <- length(demes)
  mm <- length(nondemes)
  b <- matrix('0.', nrow = m, ncol = m)
  migs <- matrix('0.', nrow = m, ncol = m)

  rownames(b) = rownames(migs) = colnames(b) = colnames(migs) <- demes

  dths <- setNames(rep('0.', m ), demes)

  ndd <- setNames(rep('0.', mm ), nondemes)

  list(births = b, migs = migs, deaths = dths,
       nonDemeDynamics = ndd, m = m, mm = mm ,
       demes = demes, nondemes = nondemes)
}


#' Organize HIV metadata
#'
#' This function aims to organize the HIV metadata in a format that will be more
#' useful for the analysis of HIV transmission in Senegal using coalescent model.
#' It mainly gets information on different dataframes and organize them so as to
#' create a matrix for the states (demes) on the phylogenetic tree
#'
#' @param metadata_CGR data.frame object that contains metadata for the CGR sequences.
#'    These sequeces will receive the state "src", a deme in our mathematical model.
#' @param metadata_SN data.frame object that contain all metadata for the Senegal (SN)
#'    sequences. Sequences will receive the states: "gpf", "gpm", or "msm".
#'
#' @return a data.frame object containing 2 columns. The first column reprsent the
#'    the sequences name, and second column represent
#'    the state of the sequences. In our case, state can be any of the demes (gpf, gpm,
#'    msm, and src).
#'
#' @export
#'
#' @examples
#' all.data.cgr <- read.csv(system.file("data/HIV_subtypes_summary_CGR.csv",
#'                                      package = "senegalHIVmodel"))
#' all.data.SN <- read.csv(system.file("data/HIV_subtypes_summary_SENEGAL_noDups.csv",
#'                                      package = "senegalHIVmodel"))
#'
#' all_data <- organize_metadata(all.data.cgr, all.data.SN)
organize_metadata <- function(metadata_CGR, metadata_SN){

  # Organize metadata for CGR sequences
  metadata_CGR["tip.name"] <- paste(metadata_CGR$Accession_number, "CGR", sep='_')
  metadata_CGR.2 <- metadata_CGR[c("tip.name", "Risk_group")]
  metadata_CGR.2["Sex"] <- NA
  metadata_CGR.2["States"] <- "src"
  metadata_CGR.3 <- metadata_CGR.2[c("tip.name", "States")]

  # Organize metadata for the Senegal sequences
  # It creates a dataframe that will not have metadata with missing information
  metadata_SN$Risk_group <- as.character(metadata_SN$Risk_group)
  metadata_SN$Sex <- as.character(metadata_SN$Sex)


  # Read all metadata that necessary information is missing
  # These are information related to the demes of our model, for example,
  # some sequences we don't have information whether it is a male or female from
  # the general population, or they are from Children.
  metadata_SN.2 <- subset(metadata_SN, is.na(Risk_group) == FALSE & Risk_group != "Children" & is.na(Sex) == FALSE)
  metadata_SN.2["tip.name"] <- paste(metadata_SN.2$Accession_number, metadata_SN.2$Subtype, "SN", metadata_SN.2$Year, sep='.')
  metadata_SN.3 <- metadata_SN.2[c("tip.name", "Risk_group", "Sex")]
  metadata_SN.3["States"] <- tolower(paste(metadata_SN.3$Risk_group, metadata_SN.3$Sex, sep = ""))
  metadata_SN.3$States[metadata_SN.3$Risk_group == "MSM"] <- "msm"
  metadata_SN.4 <- metadata_SN.3[c("tip.name", "States")]

  # bind together both dataframes (the one containing metadata on Senegal samples,
  # and the other containing metadata on CGR sequences)
  all_data <- rbind(metadata_CGR.3, metadata_SN.4)
  all_data$States <- as.factor(all_data$States)

  return(all_data)

}

#' Calculate the trajectories using posterior distribution
#'
#' Solves the demographic model for each combination of parameter values
#'
#' @param parameters list of parameter estimation from the posterior distribution
#' @param THETA list of parameter values from the Model. Here the functions
#'  for the spline function will also de loaded
#'
#' @return the solved object #to do
#' @export
#'
#' @examples
#' #TO DO
post_traj <- function(parameters, THETA){
  # we use unname here because "parameters" can be as vectors or matrix, and
  # sometimes it comes with column names, which I chose to remove these column names
  # in here.
  parameters <- unname(parameters)

  # add the values of THETA to a new variable named THETA.new
  THETA.new <- THETA

  # change the values in THETA.new to the new proposals that will be evaluated
  THETA.new$gpsp0 <- parameters[1]
  THETA.new$gpsp1 <- parameters[2]
  THETA.new$gpsp2 <- parameters[3]
  THETA.new$gpsploc <- parameters[4]
  THETA.new$msmsp0 <- parameters[5]
  THETA.new$msmsp1 <- parameters[6]
  THETA.new$msmsp2 <- parameters[7]
  THETA.new$msmsploc <- parameters[8]
  THETA.new$import <- parameters[9]
  THETA.new$srcNe <- parameters[10]
  THETA.new$pmsm2msm <- parameters[11]
  THETA.new$pgpf2gpm <- parameters[12]
  THETA.new$initmsm <- parameters[13]
  THETA.new$initgp <- parameters[14]

  # X0 is the initial conditions for the 4 demes (gpf, gpm, msm, src)
  X0 <- c(gpm = unname(THETA.new$initgp/2),
          gpf = unname(THETA.new$initgp/2),
          msm = unname(THETA.new$initmsm) ,
          src = 1e5)

  o <- dm(x0 = X0, t0 = 1980, t1 = 2014, theta = THETA.new, integrationMethod='lsoda')

  return(o)
}

#' Calculate the trajectories using posterior distrubution when also estimating
#'  maleX
#'
#' Solves the demographic model for each combination of parameter values
#'
#' @param parameters list of parameter estimation from the posterior distribution
#' @param THETA list of parameter values from the Model. Here the functions
#'  for the spline function will also de loaded
#'
#' @return the solved object #to do
#' @export
#'
#' @examples
#' #TO DO
post_traj_mx <- function(parameters, THETA){
  # we use unname here because "parameters" can be as vectors or matrix, and
  # sometimes it comes with column names, which I chose to remove these column names
  # in here.
  parameters <- unname(parameters)

  # add the values of THETA to a new variable named THETA.new
  THETA.new <- THETA

  # change the values in THETA.new to the new proposals that will be evaluated
  THETA.new$gpsp0 <- parameters[1]
  THETA.new$gpsp1 <- parameters[2]
  THETA.new$gpsp2 <- parameters[3]
  THETA.new$gpsploc <- parameters[4]
  THETA.new$msmsp0 <- parameters[5]
  THETA.new$msmsp1 <- parameters[6]
  THETA.new$msmsp2 <- parameters[7]
  THETA.new$msmsploc <- parameters[8]
  THETA.new$maleX <- parameters[9]
  THETA.new$import <- parameters[10]
  THETA.new$srcNe <- parameters[11]
  THETA.new$pmsm2msm <- parameters[12]
  THETA.new$pgpf2gpm <- parameters[13]
  THETA.new$initmsm <- parameters[14]
  THETA.new$initgp <- parameters[15]

  # X0 is the initial conditions for the 4 demes (gpf, gpm, msm, src)
  X0 <- c(gpm = unname(THETA.new$initgp/2),
          gpf = unname(THETA.new$initgp/2),
          msm = unname(THETA.new$initmsm),
          src = 1e5)

  o <- dm(x0 = X0, t0 = 1980, t1 = 2014, theta = THETA.new, integrationMethod='lsoda')

  return(o)
}


#' Reorganize by deme size
#'
#' Reorganizes by deme based on the element size in the solved demographic model
#'
#' @param Nrep integer for the number of replicates (number of combination of
#'  parameter values based on the posterior distribution)
#' @param Ntime integer for the number of time points
#' @param sizes list with the sizes from the solved demographic model (TO DO)
#'
#' @return a list for each deme (gpm, gpf, and msm)
#' @export
#'
#' @examples
#'  #TO DO
reorganize_deme_sizes <- function(Nrep, Ntime, sizes){
  Ntime <- Ntime; Nrep <- Nrep
  gpm.v <- gpf.v <- msm.v <- matrix(0, nrow=Nrep, ncol=Ntime)

  for (i in 1:Nrep) {
    for (j in 1:Ntime)
    {
      gpm.v[i,j] <- sizes[[i]][[j]][[1]]
      gpf.v[i,j] <- sizes[[i]][[j]][[2]]
      msm.v[i,j] <- sizes[[i]][[j]][[3]]
    }
  }
  return(list(gpm = gpm.v, gpf = gpf.v, msm = msm.v))
}

#' Calculates the median and quantiles for each deme size
#'
#' Calculates the median and quantiles (0.025 and 0.975) for each deme
#'
#' @param sizes_list list containg the size element from each element
#'  size is the 4th element of the solved demographic model (#to do)
#' @param times vector containig the time points used in the simulations
#'
#' @return dataframe for median and quantiles for each deme (gpm, gpmf and msm)
#' @export
#'
#' @examples
#' #TO DO
median_and_quantiles <- function(sizes_list, times){
  # after mapping the gpm, gpf and src
  # get the median by column
  median.gpm <- as.data.frame(apply(sizes_list[["gpm"]], 2, median))
  names(median.gpm) <- "median"
  median.gpf <- as.data.frame(apply(sizes_list[["gpf"]], 2, median))
  names(median.gpf) <- "median"
  median.msm <- as.data.frame(apply(sizes_list[["msm"]], 2, median))
  names(median.msm) <- "median"

  #get quantiles
  quantiles.gpm <- as.data.frame(t(apply(sizes_list[["gpm"]], 2,
                                         function(x)
                                           quantile(x, probs=c(0.025, 0.975)))))
  names(quantiles.gpm) <- c("lower", "upper")
  quantiles.gpf <- as.data.frame(t(apply(sizes_list[["gpf"]], 2,
                                         function(x)
                                           quantile(x, probs=c(0.025, 0.975)))))
  names(quantiles.gpf) <- c("lower", "upper")
  quantiles.msm <- as.data.frame(t(apply(sizes_list[["msm"]], 2,
                                         function(x)
                                           quantile(x, probs=c(0.025, 0.975)))))
  names(quantiles.msm) <- c("lower", "upper")

  gpm.c <- cbind(median.gpm, quantiles.gpm)
  gpm.c["group"] <- "gpm"
  gpf.c <- cbind(median.gpf, quantiles.gpf)
  gpf.c["group"] <- "gpf"
  msm.c <- cbind(median.msm, quantiles.msm)
  msm.c["group"] <- "msm"

  all.c <- rbind(gpm.c, gpf.c, msm.c)
  all.c <- cbind(times, all.c)

  all.c$group <- as.factor(all.c$group)

  return(all.c)
}

#' Organizes the solved births and calculate pafs
#'
#' @param birth_list list for births
#' @param times time points associated to the birth list
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # TO DO
births_pafs <- function(birth_list, times){

  #calculate pafs
  pafs <- lapply(birth_list, function(f) t(sapply(f, calculate_pafs)))
  all_data <- births_mq(pafs, times)

  return(all_data)

}


#' Organize the solved "births" objects and calculate new HIV cases
#'
#' Funtion that get the list of "births" for more than one combination of
#' parameter values and caculate the new HIV cases for each of these combinations.
#' It calls another function that will calculate the median, upper and lower
#' quantiles for each time point. See \code{\link{births_mq}}
#'
#' @param birth_list list containg the object "births" from the solved demographic
#'   model.
#' @param times time points associated to the birth list
#'
#' @return data.frame object in which columns correspond to times, median,
#'   upper and lower quantiles for the new HIV cases.
#' @export
#'
#' @examples
#' # TO DO
births_newCases <- function(birth_list, times){

  #calculate new HIV cases
  new_cases <- lapply(birth_list, function(f) t(sapply(f, calculate_newCases)))
  all_data <- births_mq(new_cases, times)

  return(all_data)

}

#' Calculate median and quantiles when using processed object "births".
#'
#' @param birth_list list for information processed from births object from the
#'   solved demographic object. This can be processed either
#'   by \code{\link{calculate_pafs}} or \code{\link{calculate_newCases}}
#' @param times time points associated to the birth list
#'
#' @return data.frame object in which columns correspond to times, median,
#'   lower and upper quantiles. Two additional columns are also provided
#'   to group data by gpm, gpmf, and msm; and by gp and msm (for plotting
#'   purposes)
#' @export
#'
#' @examples
#' # TO DO
births_mq <- function(birth_list, times){
  #calculate median
  # I used the rationale descrbed here
  # https://stackoverflow.com/questions/19218475/element-wise-mean-over-list-of-matrices/19218617#19218617
  median.b <- apply(simplify2array(birth_list), 1:2, median)
  #calculate quantiles
  b.q.025 <- apply(simplify2array(birth_list), 1:2, function(x) quantile(x, probs=0.025))
  b.q.975 <- apply(simplify2array(birth_list), 1:2, function(x) quantile(x, probs=0.975))

  #create dataframe for gpm (general population males)
  gpm <- data.frame(median=median.b[,1], lower=b.q.025[,1], upper=b.q.975[,1])
  gpm["group"] <- "gpm"
  gpm["group2"] <- "gp"

  #create dataframe for gpf (general population females)
  gpf <- data.frame(median=median.b[,2], lower=b.q.025[,2], upper=b.q.975[,2])
  gpf["group"] <- "gpf"
  gpf["group2"] <- "gp"

  #create dataframe for msm (men that have sex with other men)
  msm <- data.frame(median=median.b[,3], lower=b.q.025[,3], upper=b.q.975[,3])
  msm["group"] <- "msm"
  msm["group2"] <- "msm"

  all.data <- rbind(gpm, gpf, msm)
  all.data["times"] <-times

  return(all.data)
}


#' Calculate population attributable fraction (PAF) of transmissions.
#'
#'
#' @param f list of object "births" as returned by the solved demographic model.
#'   (see dm function in phydynR)
#'
#' @return the sum of rows in each matrix, which will correspond to PAFs.
#'   Here "births" object is a list in which each element correspont to a time
#'   point and is a 4 by 4 matrix. See vignette for more details.
#' @export
#'
#' @examples
#' # TO DO
calculate_pafs <- function(f){
  paf <- rowSums(f)[1:3]; paf <- paf /sum(paf)
}


#' Calculate new cases
#'
#' @param f list of object "births" as returned by the solved demographic model.
#'   (see dm function in phydynR)
#'
#' @return the sum of columns in each matrix, which will correspond to new HIV
#'   cases. Here "births" object is a list in which each element correspont to
#'   a time point and is a 4 by 4 matrix. See vignette for more details.
#' @export
#'
#' @examples
#' # TO DO
calculate_newCases <- function(f){
  newCases <- colSums(f)[1:3]
  #newCases <- newCases /sum(newCases)
}

#' Read MCMC runs that is saved as RDS
#'
#' Read the MCMC runs
#'
#' @param fileNames vector containg the file names
#'
#' @return as global variables the rds files assigned to a r object
#' @export
#'
#' @examples
#' #TO DO
read_mcmc_rds <- function(fileNames){
  n = 1
  while (n < length(fileNames) + 1){
    print(n)
    # read data:
    job <- readRDS(fileNames[n])
    assign(paste("r", n, sep=""), job, envir = .GlobalEnv)
    n = n + 1
  }
}
