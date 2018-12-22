# Script that calculates the population attributable fraction (PAF) for 2014
# for gpf, gpm and msm
# It uses the element births from the solved trajectory. To understand how
# trajectories were solved see Trajectories.R or Trajectories_allSubtypes.R at
# /analyses/results/

library(phydynR)
library(reshape2)
library(senegalHIVmodel)


# SUBTYPE C
# Load solved objects
load("analyses/plots/solved_objects/dmC_m1.2.rda")
load("analyses/plots/solved_objects/dmC_m2.rda")

# MODEL 1
Cm1_pafs <- df_pafs(births.p = dmC_m1.2$run[2,],
                    births.map = dmC_m1.2$MAP[[2]],
                    times = dmC_m1.2$run[[1]])
Cm1_pafs["Model"] <- "Model 1"

# get values for 2014
Cm1_pafs.2014 <- Cm1_pafs[Cm1_pafs$times >= 2014,]

# MODEL 2
Cm2_pafs <- df_pafs(births.p = dmC_m2.2$run[2,],
                    births.map = dmC_m2.2$MAP[[2]],
                    times = dmC_m2.2$run[[1]])
Cm2_pafs["Model"] <- "Model 2"

# get values for 2014
Cm2_pafs.2014 <- Cm2_pafs[Cm2_pafs$times >= 2014,]

rm(dmC_m1.2, dmC_m2.1, dmC_m2.2)

# PREVALENCE
# Load solved objects
load("analyses/plots/solved_objects/dmC_m3.rda")
load("analyses/plots/solved_objects/dmC_m4.2.rda")

# MODEL 3
Cm3_pafs <- df_pafs(births.p = dmC_m3.1$run[2,],
                    births.map = dmC_m3.1$MAP[[2]],
                    times = dmC_m3.1$run[[1]])
Cm3_pafs["Model"] <- "Model 3"

# get values for 2014
Cm3_pafs.2014 <- Cm3_pafs[Cm3_pafs$times >= 2014,]

# MODEL 4
Cm4_pafs <- df_pafs(births.p = dmC_m4.2$run[2,],
                    births.map = dmC_m4.2$MAP[[2]],
                    times = dmC_m4.2$run[[1]])
Cm4_pafs["Model"] <- "Model 4"

# get values for 2014
Cm4_pafs.2014 <- Cm4_pafs[Cm4_pafs$times >= 2014,]


PAF_2014_C <- rbind(Cm1_pafs.2014, Cm2_pafs.2014, Cm3_pafs.2014, Cm4_pafs.2014)

rm(dmC_m3.1, dmC_m4.2)


#############################################################################
# SUBTYPE 02_AG
# Load solved objects
load("analyses/plots/solved_objects/dmAG_m1.rda")
load("analyses/plots/solved_objects/dmAG_m2.rda")


# MODEL 1
AGm1_pafs <- df_pafs(births.p = dmAG_m1.1$run[2,],
                     births.map = dmAG_m1.1$MAP[[2]],
                     times = dmAG_m1.1$run[[1]])
AGm1_pafs["Model"] <- "Model 1"

# get values for 2014
AGm1.2014 <- AGm1_pafs[AGm1_pafs$times >= 2014,]

# MODEL 2
AGm2_pafs <- df_pafs(births.p = dmAG_m2.1$run[2,],
                     births.map = dmAG_m2.1$MAP[[2]],
                     times = dmAG_m2.1$run[[1]])
AGm2_pafs["Model"] <- "Model 2"

# get values for 2014
AGm2.2014 <- AGm2_pafs[AGm2_pafs$times >= 2014,]

rm(dmAG_m1.1, dmAG_m2.1)

# PREVALENCE
# Load solved objects
load("analyses/plots/solved_objects/dmAG_m3.2.rda")
load("analyses/plots/solved_objects/dmAG_m4.2.rda")

# MODEL 3
AGm3_pafs <- df_pafs(births.p = dmAG_m3.2$run[2,],
                     births.map = dmAG_m3.2$MAP[[2]],
                     times = dmAG_m3.2$run[[1]])
AGm3_pafs["Model"] <- "Model 3"

# get values for 2014
AGm3.2014 <- AGm3_pafs[AGm3_pafs$times >= 2014,]


# MODEL 4
AGm4_pafs <- df_pafs(births.p = dmAG_m4.2$run[2,],
                     births.map = dmAG_m4.2$MAP[[2]],
                     times = dmAG_m4.2$run[[1]])
AGm4_pafs["Model"] <- "Model 4"

# get values for 2014
AGm4.2014 <- AGm4_pafs[AGm4_pafs$times >= 2014,]


PAF_2014_AG <- rbind(AGm1.2014, AGm2.2014, AGm3.2014, AGm4.2014)

rm(dmAG_m3.2, dmAG_m4.2)

#############################################################################
# SUBTYPES COMBINED

# Load solved objects
load("analyses/plots/solved_objects/dm_m2.2.rda")
load("analyses/plots/solved_objects/dm_m3.2.rda")
load("analyses/plots/solved_objects/dm_m4.2.rda")

#PREVALENCE
load("analyses/plots/solved_objects/dm_m5.rda")
load("analyses/plots/solved_objects/dm_m6.rda")
load("analyses/plots/solved_objects/dm_m7.2.rda")

# Model 2
m2_pafs <- df_pafs(births.p = dm_m2.2$run[2,],
                   births.map = dm_m2.2$MAP[[2]],
                   times = dm_m2.2$run[[1]])
m2_pafs["Model"] <- "Model 2"

# get values for 2014
m2.2014 <- m2_pafs[m2_pafs$times >= 2014,]


# Model 3
m3_pafs <- df_pafs(births.p = dm_m3.2$run[2,],
                   births.map = dm_m3.2$MAP[[2]],
                   times = dm_m3.2$run[[1]])
m3_pafs["Model"] <- "Model 3"

# get values for 2014
m3.2014 <- m3_pafs[m3_pafs$times >= 2014,]


# Model 4
m4_pafs <- df_pafs(births.p = dm_m4.2$run[2,],
                   births.map = dm_m4.2$MAP[[2]],
                   times = dm_m4.2$run[[1]])
m4_pafs["Model"] <- "Model 4"

# get values for 2014
m4.2014 <- m4_pafs[m4_pafs$times >= 2014,]


# Model 5 (prevalence) = Model 2
m5_pafs <- df_pafs(births.p = dm_m5.1$run[2,],
                   births.map = dm_m5.1$MAP[[2]],
                   times = dm_m5.1$run[[1]])
m5_pafs["Model"] <- "Model 5"

# get values for 2014
m5.2014 <- m5_pafs[m5_pafs$times >= 2014,]

# Model 6 (prevalence) = Model 3
m6_pafs <- df_pafs(births.p = dm_m6.1$run[2,],
                   births.map = dm_m6.1$MAP[[2]],
                   times = dm_m6.1$run[[1]])
m6_pafs["Model"] <- "Model 6"

# get values for 2014
m6.2014 <- m6_pafs[m6_pafs$times >= 2014,]

# Model 7 (prevalence) = Model 4
m7_pafs <- df_pafs(births.p = dm_m7.2$run[2,],
                   births.map = dm_m7.2$MAP[[2]],
                   times = dm_m7.2$run[[1]])
m7_pafs["Model"] <- "Model 7"

# get values for 2014
m7.2014 <- m7_pafs[m7_pafs$times >= 2014,]



PAF_2014_combined <- rbind(m2.2014, m3.2014, m4.2014, m5.2014, m6.2014, m7.2014)

rm(dm_m2.2, dm_m3.2, dm_m4.2, dm_m5.1, dm_m6.1, dm_m7.2)

save(PAF_2014_C, PAF_2014_AG, PAF_2014_combined, file = "PAF_2014.rda")

