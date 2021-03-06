# Script used to run a Markov Chain Monte Carlo analysis to estimate the
# parameters of the HIV model
# It used the R package BayesianTools for the MCMC analysis
# It used the R package phydynR to calculate the likelihood

# laad the mathematical model
source("1.model_osg.02_AG_m2.R")
#load the data that will be used in the subsequent analysis
source("2.load_data_osg.02_AG_m2.R")

# This object function will receive the proposals of the MCMC (Markov chain Monte Carlo).
# The reason of using an object function is to make it easier to change the
# values of the parameters to be estimated in THETA.
# Note that not all parameters listed in THETA will be estimated
obj_fun <- function(parameters){
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
          msm = unname(THETA.new$initmsm) ,
          src = 1e5)

  # After changing the parameter values to the new proposals, a likelihood is
  # calculated with the funtion colik.
  # Note that this function uses several global variables, such as, dated.tree, dm, and X0
  mll <- colik(tree = dated.tree02_AG,
               theta = THETA.new,
               demographic.process.model = dm,
               x0 = X0,
               t0 = 1978,
               res = 1e3, #TODO
               timeOfOriginBoundaryCondition = FALSE,
               AgtY_penalty = 1,
               maxHeight = 35)

  return(mll)

}

# Specify a density function to be used in the prior especification (see below)
densities <-  function(par){
  # d1 to d3 and d5 to d7 I am using a lognormal distribution with mean = R0 = 1.1 and sigma = 1
  # d4 and d8 uniform distribution between the start time and the most recent sample
  # d9 exponential distribution with mean around 1/30
  # d10 exponential distribution with mean around 1/20
  d1 = dgamma(par[1], shape = 3, rate = 3/0.1, log = TRUE) #gpsp0
  d2 = dgamma(par[2], shape = 3, rate = 3/0.1, log = TRUE) #gpsp1
  d3 = dgamma(par[3], shape = 3, rate = 3/0.1, log = TRUE) #gpsp2
  d4 = dunif(par[4], min = 1978, max = 2014, log = TRUE) #gpsploc
  d5 = dgamma(par[5], shape = 3, rate = 3/0.1, log = TRUE) #msmsp0
  d6 = dgamma(par[6], shape = 3, rate = 3/0.1, log = TRUE) #msmsp1
  d7 = dgamma(par[7], shape = 3, rate = 3/0.1, log = TRUE) #msmsp2
  d8 = dunif(par[8], min = 1978, max = 2014, log = TRUE) #msmsploc
  d9 = dunif(par[9], min = 0.5, max = 2.0, log = TRUE) #maleX
  d10 = dexp(par[10], rate = 30, log = TRUE) #import
  d11 = dexp(par[11], rate = 1/100, log = TRUE) #srcNe
  d12 = dbeta(par[12], shape1 = 16, shape2 = 4, log = TRUE) #pmsm2msm
  d13 = dbeta(par[13], shape1 = 16, shape2 = 4, log = TRUE) #pgpf2gpm
  d14 = dexp(par[14], rate = 1/3, log = TRUE) #initmsm
  d15 = dexp(par[15], rate = 1/3, log = TRUE) #initgp

  return(d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12 + d13 + d14 + d15)
}


# Create sampling, this is optional but recommended because the MCMCs can generate automatic starting
# conditions if this is provided
sampler <-  function(n=1){
  d1 = rgamma(n, shape = 4, rate = 4/0.6) #gpsp0
  d2 = rgamma(n, shape = 4, rate = 4/0.4) #gpsp1
  d3 = rgamma(n, shape = 4, rate = 4/0.1) #gpsp2
  d4 = runif(n, min = 1985, max = 2000) #gpsploc
  d5 = rgamma(n, shape = 4, rate = 4/0.4) #msmsp0
  d6 = rgamma(n, shape = 4, rate = 4/0.4) #msmsp1
  d7 = rgamma(n, shape = 4, rate = 4/0.2) #msmsp2
  d8 = runif(n, min = 1985, max = 2005) #msmsploc
  d9 = runif(n, min = 0.5, max = 2.0) #maleX
  d10 = runif(n, 1/40, 1/5) #import
  d11 = runif(n, 5, 1000) #srcNe
  d12 = rbeta(n, shape1 = 16, shape2 = 4) #pmsm2msm
  d13 = rbeta(n, shape1 = 16, shape2 = 4) #pgpf2gpm
  d14 = runif(n, 1, 3) #initmsm
  d15 = runif(n, 1, 3) #initgp

  return(cbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15))
}

# Create prior (necessary for the BayesianTools package)
# Create prior (necessary for the BayesianTools package)
prior <- createPrior(density = densities,
                     sampler = sampler,
                     lower = c(0.05, 0.05, 0.05, 1978, 0.05, 0.05, 0.05, 1978, 0.5, 0, 1, 0, 0, 1, 1),
                     upper = c(1, 1, 1, 2014, 1, 1, 1, 2014, 2, 0.30, 5000, 1, 1, 300, 300))



# We first run several mcmc runs in order to get an ok run to create a
# z-matrix (for more details see Braak and Vrugt 2008)
# First, we run the following lines of code and ignoring everything else:
# Implementing the linear chain kebab job for the OSG
# loads the value for the i in the while loop below
load("iter.rdata")

while(i < 101){

  if(!file.exists("out.RDS")){
    settings = list(iterations = 90, nrChains = 1, thin = 1)
    bayesianSetup <- createBayesianSetup(likelihood = obj_fun , prior = prior)
    out <- runMCMC(bayesianSetup = bayesianSetup, sampler = "DEzs", settings = settings)
    saveRDS(out, "out.RDS")
    save(i, file="iter.rdata")
    i = i + 1
  }else{
    out <- readRDS("out.RDS")
    out1 <- out
    out <- runMCMC(bayesianSetup = out1)
    saveRDS(out, "out.RDS")
    save(i, file="iter.rdata")
    i = i + 1
  }
}

