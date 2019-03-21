# Generate samples of posterior trajectories
# and save the solved objects for generating plots for
# PAF, sizes and new cases.

# Note that the data we load here are the MCMC runs after removing burnin.
# Look at script compare_mcmc_runs_bySubtype.R at directory
# analyses/results/ for how that was done.

library(BayesianTools)

# load data for subtype C
load(system.file("data/mcmc_runs/C_m1.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/C_m2.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/C_m3.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/C_m4.rda", package = "senegalHIVmodel"))

# source model.R script to have access to the values of THETA
# and to dm object (which builds the demographic process)
source("analyses/scripts/R-scripts/Models/BySubtype/C_m1/1.model.C.R")

# calculate the demographic model (dm) for each variation of the mathematical
# model used for subtype C.
# Note that the demographic model is calculated using the function
# build.demographic.process in the phydynR package.

# MODEL 1
dmC_m1.1 <- posterior_trajectories(run = C_m1.1,
                                   burnin = 1200,
                                   n = 1000,
                                   THETA = THETA)

save(dmC_m1.1, file="dmC_m1.rda")

dmC_m1.2 <- posterior_trajectories(run = C_m1.2,
                                   burnin = 1200,
                                   n = 1000,
                                   THETA = THETA)

save(dmC_m1.2, file="dmC_m1.2.rda")

# MODEL 2
dmC_m2.1 <- posterior_trajectories(run = C_m2.1,
                                   burnin = 1500,
                                   n = 1000,
                                   THETA = THETA)

dmC_m2.2 <- posterior_trajectories(run = C_m2.2,
                                   burnin = 1500,
                                   n = 1000,
                                   THETA = THETA)

save(dmC_m2.1, dmC_m2.2, file="dmC_m2.rda")


# PREVALENCE
rm(list=ls())
detach(eqns)


load(system.file("data/mcmc_runs/C_m3.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/C_m4.rda", package = "senegalHIVmodel"))

# source model.R script to have access to the values of THETA
source("analyses/scripts/Models/BySubtype/C_m3/1.model.C_m3.R")


# calculate the demographic model (dm) for each variation of the mathematical
# model used for subtype C

# MODEL 3
dmC_m3.1 <- posterior_trajectories(run = C_m3.1,
                                   burnin = 1000,
                                   n = 1000,
                                   THETA = THETA)

save(dmC_m3.1, file="dmC_m3.rda")

# MODEL 4
dmC_m4.1 <- posterior_trajectories(run = C_m4.1,
                                   burnin = 1200,
                                   n = 1000,
                                   THETA = THETA)

save(dmC_m4.1, file="dmC_m4.rda")

dmC_m4.2 <- posterior_trajectories(run = C_m4.2,
                                   burnin = 1200,
                                   n = 1000,
                                   THETA = THETA)

save(dmC_m4.2, file="dmC_m4.2.rda")


#########################################################################
# SUBTYPE 02_AG
rm(list=ls())
detach(eqns)

# load data for subtype 02_AG
load(system.file("data/mcmc_runs/AG_m1.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/AG_m2.rda", package = "senegalHIVmodel"))

# source model.R script to have access to the values of THETA
source("analyses/scripts/Models/BySubtype/02_AG_m1/1.model.02_AG.R")

# calculate the demographic model (dm) for each variation of the mathematical
# model used for subtype C

# MODEL 1
dmAG_m1.1 <- posterior_trajectories(run = AG_m1.1,
                                    burnin = 4000,
                                    n = 1000,
                                    THETA = THETA)

save(dmAG_m1.1, file="dmAG_m1.rda")

# MODEL 2
dmAG_m2.1 <- posterior_trajectories(run = AG_m2.1,
                                    burnin = 4000,
                                    n = 1000,
                                    THETA = THETA)

save(dmAG_m2.1, file="dmAG_m2.rda")


# PREVALENCE
rm(list=ls())
detach(eqns)

load(system.file("data/mcmc_runs/AG_m3.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/AG_m4.rda", package = "senegalHIVmodel"))

# source model.R script to have access to the values of THETA
source("analyses/scripts/Models/BySubtype/02_AG_m3/1.model.02_AG_m3.R")


# calculate the demographic model (dm) for each variation of the mathematical
# model used for subtype C

# MODEL 3
dmAG_m3.1 <- posterior_trajectories(run = AG_m3.1,
                                    burnin = 1200,
                                    n = 1000,
                                    THETA = THETA)

save(dmAG_m3.1, file="dmAG_m3.rda")

dmAG_m3.2 <- posterior_trajectories(run = AG_m3.2,
                                    burnin = 1000,
                                    n = 1000,
                                    THETA = THETA)

save(dmAG_m3.2, file="dmAG_m3.2.rda")

# MODEL 4
dmAG_m4.1 <- posterior_trajectories(run = AG_m4.1,
                                    burnin = 1000,
                                    n = 1000,
                                    THETA = THETA)

save(dmAG_m4.1, file="dmAG_m4.rda")

dmAG_m4.2 <- posterior_trajectories(run = AG_m4.2,
                                    burnin = 1000,
                                    n = 1000,
                                    THETA = THETA)

save(dmAG_m4.2, file="dmAG_m4.2.rda")
