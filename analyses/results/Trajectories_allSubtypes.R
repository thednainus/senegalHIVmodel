# Generate samples of posterior trajectories
# and save the solved objects for generating plots for
# PAF, sizes and new cases.

# Note that the data we load here are the MCMC runs after removing burnin.
# Look at script compare_mcmc_runs_AllSubtypes.R at directory
# analyses/results/FINAL/ for how that was done.

library(BayesianTools)

# load data for subtypes combined
load(system.file("data/mcmc_runs/Model2.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/Model3.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/Model4.rda", package = "senegalHIVmodel"))


# source model.R script to have access to the values of THETA
# and to dm object (which builds the demographic process)
source("analyses/scripts/Models/Subtypes_Combined/Model2/1.model.v2.R")

# calculate the demographic model (dm) for each variation of the mathematical
# model used for subtype C.
# Note that the demographic model is calculated using the function
# build.demographic.process in the phydynR package.

# MODEL 2
dm_m2.1 <- posterior_trajectories(run = m2.1,
                                   burnin = 2000,
                                   n = 1000,
                                   THETA = THETA)

save(dm_m2.1, file="dm_m2.rda")

dm_m2.2 <- posterior_trajectories(run = m2.2,
                                  burnin = 2000,
                                  n = 1000,
                                  THETA = THETA)

save(dm_m2.2, file="dm_m2.2.rda")

# MODEL 3
dm_m3.1 <- posterior_trajectories(run = m3.1,
                                  burnin = 1200,
                                  n = 1000,
                                  THETA = THETA)

save(dm_m3.1, file="dm_m3.rda")

dm_m3.2 <- posterior_trajectories(run = m3.2,
                                  burnin = 1200,
                                  n = 1000,
                                  THETA = THETA)

save(dm_m3.2, file="dm_m3.2.rda")


# MODEL 4
dm_m4.1 <- posterior_trajectories(run = m4.1,
                                  burnin = 4000,
                                  n = 1000,
                                  THETA = THETA)

save(dm_m4.1, file="dm_m4.rda")

dm_m4.2 <- posterior_trajectories(run = m4.2,
                                  burnin = 4000,
                                  n = 1000,
                                  THETA = THETA)

save(dm_m4.2, file="dm_m4.2.rda")

# PREVALENCE
rm(list=ls())
detach(eqns)


load(system.file("data/mcmc_runs/Model5.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/Model6.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/Model7.rda", package = "senegalHIVmodel"))

# source model.R script to have access to the values of THETA
source("analyses/scripts/Models/Subtypes_Combined/Model5/1.model.v5.R")


# calculate the demographic model (dm) for each variation of the mathematical
# model used for all subtypes

# MODEL 5
dm_m5.1 <- posterior_trajectories(run = m5.1,
                                  burnin = 4000,
                                  n = 1000,
                                  THETA = THETA)

save(dm_m5.1, file="dm_m5.rda")

# MODEL 6
dm_m6.1 <- posterior_trajectories(run = m6.1,
                                  burnin = 1200,
                                  n = 1000,
                                  THETA = THETA)

save(dm_m6.1, file="dm_m6.rda")

# MODEL 7
dm_m7.1 <- posterior_trajectories(run = m7.1,
                                  burnin = 2100,
                                  n = 1000,
                                  THETA = THETA)

save(dm_m7.1, file="dm_m7.rda")

dm_m7.2 <- posterior_trajectories(run = m7.2,
                                  burnin = 2100,
                                  n = 1000,
                                  THETA = THETA)

save(dm_m7.2, file="dm_m7.2.rda")
