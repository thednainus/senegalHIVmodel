# Script used to run a Markov Chain Monte Carlo analysis to estimate the
# parameters of the HIV model
# It used the R package BayesianTools for the MCMC analysis
# It used the R package phydynR to calculate the likelihood

# laad the mathematical model
source("analyses/scripts/1.model.R")
#load the data that will be used in the subsequent analysis
source("analyses/scripts/2.load_data.R")


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
  mll <- colik(tree = dated.tree,
               theta = THETA.new,
               demographic.process.model = dm,
               x0 = X0,
               t0 = 1978,
               res = 1e3,
               timeOfOriginBoundaryCondition = FALSE,
               AgtY_penalty = 1,
               maxHeight = 41)

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
  d10 = dexp(par[10], rate = 20, log = TRUE) #srcNe
  d11 = dbeta(par[11], shape1 = 16, shape2 = 4, log = TRUE) #pmsm2msm
  d12 = dbeta(par[12], shape1 = 16, shape2 = 4, log = TRUE) #pgpf2gpm
  d13 = dexp(par[13], rate = 1/10, log = TRUE) #initmsm
  d14 = dexp(par[14], rate = 1/10, log = TRUE) #initgp

  return(d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12 + d13 + d14)
}


# Create sampling, this is optional but recommended because the MCMCs can generate automatic starting
# conditions if this is provided
sampler <-  function(n=1){
  d1 = rgamma(n, shape = 3, rate = 3/0.1) #gpsp0
  d2 = rgamma(n, shape = 3, rate = 3/0.1) #gpsp1
  d3 = rgamma(n, shape = 3, rate = 3/0.1) #gpsp2
  d4 = runif(n, min = 1978, max = 2014) #gpsploc
  d5 = rgamma(n, shape = 3, rate = 3/0.1) #msmsp0
  d6 = rgamma(n, shape = 3, rate = 3/0.1) #msmsp1
  d7 = rgamma(n, shape = 3, rate = 3/0.1) #msmsp2
  d8 = runif(n, min = 1978, max = 2014) #msmsploc
  d9 = rexp(n, rate = 30) #import
  d10 = rexp(n, rate = 20) #srcNe
  d11 = rbeta(n, shape1 = 16, shape2 = 4) #pmsm2msm
  d12 = rbeta(n, shape1 = 16, shape2 = 4) #pgpf2gpm
  d13 = rexp(n, rate = 1/10) #initmsm
  d14 = rexp(n, rate = 1/10) #initgp

  return(cbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14))
}

# Create prior (necessary for the BayesianTools package)
prior <- createPrior(density = densities,
                     sampler = sampler,
                     lower = c(0, 0, 0, 1978, 0, 0, 0, 1978, 0, 0.0001, 0, 0, 1, 1),
                     upper = c(1, 1, 1, 2014, 1, 1, 1, 2014, 0.30, 0.30, 1, 1, 300, 300))



# Note that before proceding to reading a previous run, we first run several mcmc
# runs in order to get an ok run to create a z-matrix (for more details see Braak and Vrugt 2008)
# This run was generated by the following lines of commented code and ignoring everything else:
bayesianSetup <- createBayesianSetup(likelihood = obj_fun , prior = prior)
startValue =  u_x[c(444, 445, 446), ]
settings = list(iterations = 6000, startValue = startValue ,nrChains = 1, thin = 1)
out <- runMCMC(bayesianSetup = bayesianSetup, sampler = "DEzs", settings = settings)
saveRDS(runz2, "runz2.rds")
out2 <- runMCMC(bayesianSetup = out)
out3 <- runMCMC(bayesianSetup = out2)
saveRDS(out2, "newPrior.rds")
saveRDS(out3, "newPrior.rds")

# After we had a run to create a z-matrix we did the follow:
# Read a previous run for creating starting values for the Z matrix
# runZ below is the run used for previous model (when not estimating initial population size, and using maleX=2)
#runZ <- readRDS(system.file("data/outDEZs_37147513_0_18000_1.rds", package = "senegalHIVmodel"))
# runZ below is the run used for new model (when estimating initial population size, and using maleX=1.02)
#runZ <- readRDS("run_for_zMatriz.rds")
#runZ <- readRDS("mayberun_for_zMatriz.rds")
#The run below was generated using a zMatrix populated with the run mayberun_for_zMatriz.rds
# and using a sample starting from 15000
#runZ <- readRDS("run2_with_zMatrix.RDS")
# Trying this new run for a z matrix
#runZ <- readRDS("~/Box Sync/tests/linearJobs/out_38046926.rds")
#runZ <- readRDS("newPrior.rds")
#runZ <- readRDS("runz2.rds")
#runZ in my home desktop
#runZ <- readRDS("~/Box Sync/Senegal/mcmc_new_model/new_prior/new_version_phydyn/out_38148573.rds")
#runZ using the run that I provided some of the starting value
#runZ=readRDS("newPrior_runWithStartValue1.rds")
# trying two different runs that have very high likelihoods to provide z matrix
#runZ1 <- readRDS("~/Box Sync/Senegal/mcmc_new_model/new_prior/new_version_phydyn/initialValues/out_38256824.rds")
#runZ2 <- readRDS("~/Box Sync/Senegal/mcmc_new_model/new_prior/new_version_phydyn/initialValues/out_38256831.rds")

#runZ <- readRDS("z_andSuperHighLn.RDS")

#runZ using a run that seems to be stuck in 2 peaks for gpsp1 and pmsms2msm
# Here I chose the values more likely to be true based on the plots
runZ <- readRDS("~/Box Sync/Senegal/mcmc_new_model/new_prior/new_version_phydyn/initialValues/out_38256820.rds")
# Get a good sample (the run above is not good, however it can provide a good Z matrix)
# For more information on this: https://github.com/florianhartig/BayesianTools/issues/79
x <- getSample(runZ, start=2000)
#x in my home Desktop
#x <- getSample(runZ, start=3000)
#get samples for both runs runZ1 and runZ2
#x1 <- getSample(runZ1)
#x2 <- getSample(runZ2, start=1000)

#x <- getSample(runZ, start=1000)
#merged x1 and x2
#x <- rbind(x1, x2)
# Get the range for the parameter estimates for the previous run
rangePost = apply(x, 2, range)
rangePost[2,1] <- 0.4820079

rangePost[2,2] <- 0.3420202
#get unique values of x
u_x <- unique(x)


startValue =  u_x[c(444, 445, 446), ]
#cretae new Z matrix based on previous run
# before I was estimating 12 parameters (hence ncol=12)
#newZ = matrix(runif(1500, rangePost[1,], rangePost[2,]), ncol = 12, byrow = T)
# now I am estimating 14 parameters (hence ncol=14)
newZ = matrix(runif(1960, rangePost[1,], rangePost[2,]), ncol = 14, byrow = T)

# Because I will run several analysis in parallel, and to avoid the initial values to be identical
# I will provide as argument position 1 (pos1), position 2 (pos2), and position 3 (pos3)
# from the unique values of x (u_x)
pos1=72
pos2=73
pos3=74
iter=6000 # number of iterations
settings = list(Z = newZ, startValue =  u_x[c(pos1, pos2, pos3), ], nrChains = 1, iterations = iter, thin = 1)
settings = list(Z = newZ, startValue =  startValue, nrChains = 1, iterations = iter, thin = 1)

# Create bayesianSetup
bayesianSetup <- createBayesianSetup(likelihood = obj_fun , prior = prior)

outZ <- runMCMC(bayesianSetup = bayesianSetup,  sampler = "DEzs", settings = settings )
saveRDS(outZ, "z_andSuperHighLn2.RDS")
outZ.2 <- runMCMC(bayesianSetup = outZ)



# save output file for later analysis, i.e. check convergence, plot samples, etc
# give the name that you wish, i.e. mcmc_run.RDS
saveRDS(out, "newPrior_with_zMatrix.RDS")
