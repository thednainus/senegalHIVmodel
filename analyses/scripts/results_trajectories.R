# Generate samples of posterior trajectories
# and saved the solved objects for generating plots for trajectories
library(BayesianTools)

#load MCMC runs (merged runs for the different models)
load("mergedRuns_newModel.rda")
# load model.R
source("analyses/scripts/Model2/1.model.v2.R")

# without maleX estimation
############################## Model 2 ########################################
m2.s <- getSample(m2.l.m, start=3000)
m2_map <- MAP(m2.l.m, start=3000)$parametersMAP

# sampling 100 combination of parameters from the posterior
m2.100 <- m2.s[sample(nrow(m2.s),size=100,replace=FALSE),]
m2.1000 <- m2.s[sample(nrow(m2.s),size=1000,replace=FALSE),]

# solve the demographic model for 100 combination of parameter values
m2_o <- do.call("cbind", apply(m2.100, 1, post_traj, THETA))
m2_o.1000 <- do.call("cbind", apply(m2.1000, 1, post_traj, THETA))
m2_map_o <- post_traj(parameters = m2_map, THETA = THETA)


############################## Model 3 ########################################
m3.s <- getSample(m3.m, start=800)
m3_map <- MAP(m3.m, start=800)$parametersMAP

# sampling 100 combination of parameters from the posterior
m3.100 <- m3.s[sample(nrow(m3.s),size=100,replace=FALSE),]
m3.1000 <- m3.s[sample(nrow(m3.s),size=1000,replace=FALSE),]

# solve the demographic model for 100 combination of parameter values
m3_o <- do.call("cbind", apply(m3.100, 1, post_traj, THETA))
m3_o.1000 <- do.call("cbind", apply(m3.1000, 1, post_traj, THETA))
m3_map_o <- post_traj(parameters = m3_map, THETA = THETA)


############################## Model 4 ########################################
m4.s <- getSample(m4.l.m, start=2000)
m4_map <- MAP(m4.l.m, start=2000)$parametersMAP

# sampling 1000 combination of parameters from the posterior
m4.100 <- m4.s[sample(nrow(m4.s),size=100,replace=FALSE),]
m4.1000 <- m4.s[sample(nrow(m4.s),size=1000,replace=FALSE),]

# solve the demographic model for 1000 combination of parameter values
m4_o <- do.call("cbind", apply(m4.100, 1, post_traj, THETA))
m4_o.1000 <- do.call("cbind", apply(m4.1000, 1, post_traj, THETA))
m4_map_o <- post_traj(parameters = m4_map, THETA = THETA)

######################### by subtype 02_AG: Model 1 ###########################
m02_AG.m1.s <- getSample(m02_AG.m, start=800)
m02_AG.m1_map <- MAP(m02_AG.m, start=800)$parametersMAP

# sampling 100 combination of parameters from the posterior
m02_AG.m1.100 <- m02_AG.m1.s[sample(nrow(m02_AG.m1.s),size=100,replace=FALSE),]
m02_AG.m1.1000 <- m02_AG.m1.s[sample(nrow(m02_AG.m1.s),size=1000,replace=FALSE),]

#posterior trajectories
m02_AG_m1_o <- do.call("cbind", apply(m02_AG.m1.100, 1, post_traj, THETA))
m02_AG_m1_o.1000 <- do.call("cbind", apply(m02_AG.m1.1000, 1, post_traj, THETA))
m02_AG_m1_map_o <- post_traj(parameters = m02_AG.m1_map, THETA = THETA)

######################### by subtype 02_AG: Model 2 ###########################
m02_AG.m2.s <- getSample(m02_AG.m2, start=800)
m02_AG.m2_map <- MAP(m02_AG.m2, start=800)$parametersMAP

# sampling 100 combination of parameters from the posterior
m02_AG.m2.100 <- m02_AG.m2.s[sample(nrow(m02_AG.m2.s),size=100,replace=FALSE),]
m02_AG.m2.1000 <- m02_AG.m2.s[sample(nrow(m02_AG.m2.s),size=1000,replace=FALSE),]

#posterior trajectories
m02_AG_m2_o <- do.call("cbind", apply(m02_AG.m2.100, 1, post_traj, THETA))
m02_AG_m2_o.1000 <- do.call("cbind", apply(m02_AG.m2.1000, 1, post_traj, THETA))
m02_AG_m2_map_o <- post_traj(parameters = m02_AG.m2_map, THETA = THETA)

######################### by subtype C: Model 1 ##############################
mC.m1.s <- getSample(mC.m, start=800)
mC.m1_map <- MAP(mC.m, start=800)$parametersMAP

# sampling 100 combination of parameters from the posterior
mC.m1.100 <- mC.m1.s[sample(nrow(mC.m1.s),size=100,replace=FALSE),]
mC.m1.1000 <- mC.m1.s[sample(nrow(mC.m1.s),size=1000,replace=FALSE),]

#posterior trajectories
C_m1_o <- do.call("cbind", apply(mC.m1.100, 1, post_traj, THETA))
C_m1_o.1000 <- do.call("cbind", apply(mC.m1.1000, 1, post_traj, THETA))
mC_m1_map_o <- post_traj(parameters = mC.m1_map, THETA = THETA)


######################### by subtype C: Model 2 ################################
mC.m2.s <- getSample(m2.mC.m, start=800)
mC.m2_map <- MAP(m2.mC.m, start=800)$parametersMAP

# sampling 100 combination of parameters from the posterior
mC.m2.100 <- mC.m2.s[sample(nrow(mC.m2.s),size=100,replace=FALSE),]
mC.m2.1000 <- mC.m2.s[sample(nrow(mC.m2.s),size=1000,replace=FALSE),]

#posterior trajectories
C_m2_o <- do.call("cbind", apply(mC.m2.100, 1, post_traj, THETA))
C_m2_o.1000 <- do.call("cbind", apply(mC.m2.1000, 1, post_traj, THETA))
mC_m2_map_o <- post_traj(parameters = mC.m2_map, THETA = THETA)


#save solved objects
save(m2_o, m3_o, m4_o, m02_AG_m1_o, m02_AG_m2_o, C_m1_o, C_m2_o,
     file="analyses/plots/solved_objects_noMalex.rda")
save(m2_o.1000, m3_o.1000, m4_o.1000, m02_AG_m1_o.1000, m02_AG_m2_o.1000, C_m1_o.1000, C_m2_o.1000,
     file="analyses/plots/solved_objects_noMalex_1000reps.rda")
save(m2_map_o, m3_map_o, m4_map_o, m02_AG_m1_map_o, m02_AG_m2_map_o, mC_m1_map_o, mC_m2_map_o,
     file="analyses/plots/solved_objects_noMalex_maps.rda")
