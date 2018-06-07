# Script used to run a Markov Chain Monte Carlo analysis to estimate the
# parameters of the HIV model
# It used the R package BayesianTools for the MCMC analysis
# It used the R package phydynR to calculate the likelihood

# laad the mathematical model
source("analyses/scripts/1.model.v2.R")
#load the data that will be used in the subsequent analysis
source("analyses/scripts/2.load_data.v2.R")

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

  # After changing the parameter values to the new proposals, a likelihood is
  # calculated with the funtion colik.
  # Note that this function uses several global variables, such as, dated.tree, dm, and X0
  mll <- colik(tree = dated.tree.dakar,
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
  d9 = dexp(par[9], rate = 30, log = TRUE) #import
  d10 = dexp(par[10], rate = 1/100, log = TRUE) #srcNe
  d11 = dbeta(par[11], shape1 = 16, shape2 = 4, log = TRUE) #pmsm2msm
  d12 = dbeta(par[12], shape1 = 16, shape2 = 4, log = TRUE) #pgpf2gpm
  d13 = dexp(par[13], rate = 1/3, log = TRUE) #initmsm
  d14 = dexp(par[14], rate = 1/3, log = TRUE) #initgp

  return(d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12 + d13 + d14)
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
  d9 = runif(n, 1/40, 1/5) #import
  d10 = runif(n, 5, 1000) #srcNe
  d11 = rbeta(n, shape1 = 16, shape2 = 4) #pmsm2msm
  d12 = rbeta(n, shape1 = 16, shape2 = 4) #pgpf2gpm
  d13 = runif(n, 1, 3) #initmsm
  d14 = runif(n, 1, 3) #initgp

  return(cbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14))
}

# Create prior (necessary for the BayesianTools package)
prior <- createPrior(density = densities,
                     sampler = sampler,
                     lower = c(0.05, 0.05, 0.05, 1978, 0.05, 0.05, 0.05, 1978, 0, 1., 0, 0, 1, 1),
                     upper = c(1, 1, 1, 2014, 1, 1, 1, 2014, 0.30, 5000, 1, 1, 10, 10))


load("fits.ordered.rda")
value1 <-  fits.ordered[[3]]$par
value2 <-  fits.ordered[[4]]$par
value3 <-  fits.ordered[[5]]$par
initialValues <- rbind(unname(value1), unname(value2), unname(value3))

# After we had a run to create a z-matrix we did the follow:
# Read a previous run for creating starting values for the Z matrix
# runZ below is the run used for previous model (when not estimating initial population size, and using maleX=2)
#runZ <- readRDS(system.file("data/outDEZs_37147513_0_18000_1.rds", package = "senegalHIVmodel"))
# runZ below is the run used for new model (when estimating initial population size, and using maleX=1.02)
runZ <- readRDS("analyses/scripts/Model2/Preliminary_results/forZmatrix.rds")

# Get a good sample (the run above is not good, however it can provide a good Z matrix)
# For more information on this: https://github.com/florianhartig/BayesianTools/issues/79
x <- getSample(runZ, start=800)

# Get the range for the parameter estimates for the previous run
rangePost = apply(x, 2, range)

#get unique values of x
u_x <- unique(x)

#cretae new Z matrix based on previous run
# now I am estimating 14 parameters (hence ncol=14)
newZ = matrix(runif(1960, rangePost[1,], rangePost[2,]), ncol = 14, byrow = T)

# Because I will run several analysis in parallel, and to avoid the initial values to be identical
# I will provide as argument position 1 (pos1), position 2 (pos2), and position 3 (pos3)
# from the unique values of x (u_x)
#pos1=72
#pos2=73
#pos3=74
iter=6000 # number of iterations
#settings = list(Z = newZ, startValue =  u_x[c(pos1, pos2, pos3), ], nrChains = 1, iterations = iter, thin = 1)
settings = list(Z = newZ, startValue =  initialValues, nrChains = 1, iterations = iter, thin = 1)

# Create bayesianSetup
bayesianSetup <- createBayesianSetup(likelihood = obj_fun , prior = prior)

outZ <- runMCMC(bayesianSetup = bayesianSetup,  sampler = "DEzs", settings = settings )
#saveRDS(outZ, "z_andSuperHighLn2.RDS")
#outZ.2 <- runMCMC(bayesianSetup = outZ)



# save output file for later analysis, i.e. check convergence, plot samples, etc
# give the name that you wish, i.e. mcmc_run.RDS
#saveRDS(out, "newPrior_with_zMatrix.RDS")
