# Generate samples of posterior trajectories
# and saved the solved objects for generating plots for trajectories
# This analysis includes information for prevalence
library(BayesianTools)

# load MCMC runs (merged runs for the different models)
#load(system.file("data/mcmc_runs/mergedRuns_model5_mx.rda", package = "senegalHIVmodel"))
#load(system.file("data/mcmc_runs/mergedRuns_model7_mx.rda", package = "senegalHIVmodel"))
#load(system.file("data/mcmc_runs/mergedRuns_subtype_prevalence.rda", package = "senegalHIVmodel"))

load(system.file("data/mcmc_runs/new_mcmc_results.rda", package = "senegalHIVmodel"))


# load model.R to have access to object THETA
source("~/Box Sync/my_R_packages/senegalHIVmodel/analyses/scripts/Models/BySubtype/C_m3/1.model.C_m3.R")

# Estimation of total of 15 parameters, including maleX
############################## Model 5 ########################################
m5.s <- getSample(m5, start=1200)
m5_map <- MAP(m5, start=1200)$parametersMAP

# sampling 1000 combination of parameters from the posterior
m5.1000 <- m5.s[sample(nrow(m5.s),size=1000,replace=FALSE),]


# solve the demographic model for 1000 combination of parameter values
m5_o.1000 <- do.call("cbind", apply(m5.1000, 1, post_traj_mx, THETA))
# solve the demographic model for MAP (maximum a posteriori)
m5_map_o <- post_traj_mx(parameters = m5_map, THETA = THETA)

############################## Model 6 ########################################
m6.s <- getSample(m6, start=1200)
m6_map <- MAP(m6, start=1200)$parametersMAP

# sampling 1000 combination of parameters from the posterior
m6.1000 <- m6.s[sample(nrow(m6.s),size=1000,replace=FALSE),]


# solve the demographic model for 1000 combination of parameter values
m6_o.1000 <- do.call("cbind", apply(m6.1000, 1, post_traj_mx, THETA))
# solve the demographic model for MAP (maximum a posteriori)
m6_map_o <- post_traj_mx(parameters = m6_map, THETA = THETA)


############################## Model 7 ########################################
m7.s <- getSample(m7, start=1000)
m7_map <- MAP(m7, start=1000)$parametersMAP

# sampling 1000 combination of parameters from the posterior
m7.1000 <- m7.s[sample(nrow(m7.s),size=1000,replace=FALSE),]

# solve the demographic model for 1000 combination of parameter values
m7_o.1000 <- do.call("cbind", apply(m7.1000, 1, post_traj_mx, THETA))
# solve the demographic model for MAP
m7_map_o <- post_traj_mx(parameters = m7_map, THETA = THETA)


######################### by subtype 02_AG: Model 3 ###########################
m02_AG.m3.s <- getSample(AG_m3, start=1000)
m02_AG.m3_map <- MAP(AG_m3, start=1000)$parametersMAP

# sampling 1000 combination of parameters from the posterior
m02_AG.m3.1000 <- m02_AG.m3.s[sample(nrow(m02_AG.m3.s),size=1000,replace=FALSE),]

# solve the demographic model for 1000 combination of parameter values
m02_AG_m3_o.1000 <- do.call("cbind", apply(m02_AG.m3.1000, 1, post_traj_mx, THETA))
# solve the demographic model for MAP
m02_AG_m3_map_o <- post_traj_mx(parameters = m02_AG.m3_map, THETA = THETA)


######################### by subtype 02_AG: Model 4 ###########################
m02_AG.m4.s <- getSample(AG_m4.2, start=1000)
m02_AG.m4_map <- MAP(AG_m4.2, start=1000)$parametersMAP

# sampling 1000 combination of parameters from the posterior
m02_AG.m4.1000 <- m02_AG.m4.s[sample(nrow(m02_AG.m4.s),size=1000,replace=FALSE),]

# solve the demographic model for 1000 combination of parameter values
m02_AG_m4_o.1000 <- do.call("cbind", apply(m02_AG.m4.1000, 1, post_traj_mx, THETA))
# solve the demographic model for MAP
m02_AG_m4_map_o <- post_traj_mx(parameters = m02_AG.m4_map, THETA = THETA)


######################### by subtype C: Model 3 ##############################
mC.m3.s <- getSample(C_m3.m, start=1000)
mC.m3_map <- MAP(C_m3.m, start=1000)$parametersMAP

# sampling 1000 combination of parameters from the posterior
mC.m3.1000 <- mC.m3.s[sample(nrow(mC.m3.s),size=1000,replace=FALSE),]

# solve the demographic model for 1000 combination of parameter values
C_m3_o.1000 <- do.call("cbind", apply(mC.m3.1000, 1, post_traj_mx, THETA))
# solve the demographic model for MAP
mC_m3_map_o <- post_traj_mx(parameters = mC.m3_map, THETA = THETA)


######################### by subtype C: Model 4 ################################
mC.m4.s <- getSample(C_m4.m, start=1000)
mC.m4_map <- MAP(C_m4.m, start=1000)$parametersMAP

# sampling 1000 combination of parameters from the posterior
mC.m4.1000 <- mC.m4.s[sample(nrow(mC.m4.s),size=1000,replace=FALSE),]

# solve the demographic model for 1000 combination of parameter values
C_m4_o.1000 <- do.call("cbind", apply(mC.m4.1000, 1, post_traj_mx, THETA))
# solve the demographic model for MAP
mC_m4_map_o <- post_traj_mx(parameters = mC.m4_map, THETA = THETA)


################################################################################
#save solved objects
save(m5_o.1000, m7_o.1000, m02_AG_m3_o.1000, m02_AG_m4_o.1000, C_m3_o.1000, C_m4_o.1000,
     file="analyses/plots/solved_objects/solved_objects_prevalence_1000reps.rda")
save(m5_map_o, m7_map_o, m02_AG_m3_map_o, m02_AG_m4_map_o, mC_m3_map_o, mC_m4_map_o,
     file="analyses/plots/solved_objects/solved_objects_prevalence_maps.rda")



save(m5_o.1000, m6_o.1000, m02_AG_m3_o.1000, m02_AG_m4_o.1000, C_m3_o.1000, C_m4_o.1000,
     file="analyses/plots/solved_objects/solved_objects_prevalence_1000reps_new.rda")
save(m5_map_o, m6_map_o, m02_AG_m3_map_o, m02_AG_m4_map_o, mC_m3_map_o, mC_m4_map_o,
     file="analyses/plots/solved_objects/solved_objects_prevalence_maps_new.rda")
