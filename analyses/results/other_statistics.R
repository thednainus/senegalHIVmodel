# Script that calculates the proportion of infected individuals that are gpm,
# gpf, and msm
# It uses the element sizes from the solved trajectory. To understand how
# trajectories were solved see Trajectories.R or Trajectories_allSubtypes.R at
# /analyses/results/FINAL/

library(phydynR)
library(reshape2)
library(senegalHIVmodel)


# SUBTYPE C
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmC_m1.2.rda")
load("analyses/plots/solved_objects/FINAL/dmC_m2.rda")

# MODEL 1
Cm1 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dmC_m1.2$run[4,])
Cm1.p <- prop_infected(Cm1, times = dmC_m1.2$run[[1]])

# get values for 2014
Cm1.values <- Cm1.p[Cm1.p$times >= 2014,]
Cm1.values["Model"] <- "Model 1"

# MODEL 2
Cm2 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dmC_m2.2$run[4,])
Cm2.p <- prop_infected(Cm2, times = dmC_m2.2$run[[1]])

# get values for 2014
Cm2.values <- Cm2.p[Cm2.p$times >= 2014,]
Cm2.values["Model"] <- "Model 2"

rm(dmC_m1.2, dmC_m2.1, dmC_m2.2)

#PREVALENCE
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmC_m3.rda")
load("analyses/plots/solved_objects/FINAL/dmC_m4.2.rda")

# MODEL 3
Cm3 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dmC_m3.1$run[4,])
Cm3.p <- prop_infected(Cm3, times = dmC_m3.1$run[[1]])

# get values for 2014
Cm3.values <- Cm3.p[Cm3.p$times >= 2014,]
Cm3.values["Model"] <- "Model 3"


# MODEL 4
Cm4 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dmC_m4.2$run[4,])
Cm4.p <- prop_infected(Cm4, times = dmC_m4.2$run[[1]])

# get values for 2014
Cm4.values <- Cm4.p[Cm4.p$times >= 2014,]
Cm4.values["Model"] <- "Model 4"

infections_C <- rbind(Cm1.values, Cm2.values, Cm3.values, Cm4.values)

rm(dmC_m3.1, dmC_m4.2)


#############################################################################
# SUBTYPE 02_AG
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmAG_m1.rda")
load("analyses/plots/solved_objects/FINAL/dmAG_m2.rda")

# MODEL 1
AGm1 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dmAG_m1.1$run[4,])
AGm1.p <- prop_infected(AGm1, times = dmAG_m1.1$run[[1]])

# get values for 2014
AGm1.values <- AGm1.p[AGm1.p$times >= 2014,]
AGm1.values["Model"] <- "Model 1"



# MODEL 2
AGm2 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dmAG_m2.1$run[4,])
AGm2.p <- prop_infected(AGm2, times = dmAG_m2.1$run[[1]])

# get values for 2014
AGm2.values <- AGm2.p[AGm2.p$times >= 2014,]
AGm2.values["Model"] <- "Model 2"

rm(dmAG_m1.1, dmAG_m2.1)

#PREVALENCE
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmAG_m3.2.rda")
load("analyses/plots/solved_objects/FINAL/dmAG_m4.2.rda")

# MODEL 3
AGm3 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dmAG_m3.2$run[4,])
AGm3.p <- prop_infected(AGm3, times = dmAG_m3.2$run[[1]])

# get values for 2014
AGm3.values <- AGm3.p[AGm3.p$times >= 2014,]
AGm3.values["Model"] <- "Model 3"


# MODEL 4
AGm4 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dmAG_m4.2$run[4,])
AGm4.p <- prop_infected(AGm4, times = dmAG_m4.2$run[[1]])

# get values for 2014
AGm4.values <- AGm4.p[AGm4.p$times >= 2014,]
AGm4.values["Model"] <- "Model 4"

infections_AG <- rbind(AGm1.values, AGm2.values, AGm3.values, AGm4.values)

rm(dmAG_m3.2, dmAG_m4.2)

#############################################################################
# SUBTYPES COMBINED

# Load solved objects
load("analyses/plots/solved_objects/FINAL/dm_m2.2.rda")
load("analyses/plots/solved_objects/FINAL/dm_m3.2.rda")
load("analyses/plots/solved_objects/FINAL/dm_m4.2.rda")

#PREVALENCE
load("analyses/plots/solved_objects/FINAL/dm_m5.rda")
load("analyses/plots/solved_objects/FINAL/dm_m6.rda")
load("analyses/plots/solved_objects/FINAL/dm_m7.2.rda")

# MODEL 2
m2 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dm_m2.2$run[4,])
m2.p <- prop_infected(m2, times = dm_m2.2$run[[1]])

# get values for 2014
m2.values <- m2.p[m2.p$times >= 2014,]
m2.values["Model"] <- "Model 2"

# MODEL 5
m5 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dm_m5.1$run[4,])
m5.p <- prop_infected(m5, times = dm_m5.1$run[[1]])

# get values for 2014
m5.values <- m5.p[m5.p$times >= 2014,]
m5.values["Model"] <- "Model 5"

# MODEL 3
m3 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dm_m3.2$run[4,])
m3.p <- prop_infected(m3, times = dm_m3.2$run[[1]])

# get values for 2014
m3.values <- m3.p[m3.p$times >= 2014,]
m3.values["Model"] <- "Model 3"


# MODEL 6
m6 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dm_m6.1$run[4,])
m6.p <- prop_infected(m6, times = dm_m6.1$run[[1]])

# get values for 2014
m6.values <- m6.p[m6.p$times >= 2014,]
m6.values["Model"] <- "Model 6"

# MODEL 4
m4 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dm_m4.2$run[4,])
m4.p <- prop_infected(m4, times = dm_m4.2$run[[1]])

# get values for 2014
m4.values <- m4.p[m4.p$times >= 2014,]
m4.values["Model"] <- "Model 4"

# MODEL 7
m7 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000, sizes = dm_m7.2$run[4,])
m7.p <- prop_infected(m7, times = dm_m7.2$run[[1]])

# get values for 2014
m7.values <- m7.p[m7.p$times >= 2014,]
m7.values["Model"] <- "Model 7"

infections_All <- rbind(m2.values, m3.values, m4.values,
                        m5.values, m6.values, m7.values)

rm(dm_m2.2, dm_m3.2, dm_m4.2, dm_m5.1, dm_m6.1, dm_m7.2)

save(infections_C, infections_AG, infections_All, file = "infections_correct.rda")

