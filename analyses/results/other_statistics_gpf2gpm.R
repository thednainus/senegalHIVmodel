# Calculates the proportion of infections in gpm attributable to gpf.
# To understand how trajectories were solved see Trajectories.R or
# Trajectories_allSubtypes.R at /analyses/results/

library(senegalHIVmodel)
library(ggplot2)
library(purrr)


# SUBTYPE C
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmC_m1.2.rda")
load("analyses/plots/solved_objects/FINAL/dmC_m2.rda")

# MODEL 1
gpf2gpm_Cm1 <- deme2deme(births.p = dmC_m1.2$run[2,],
                           times = dmC_m1.2$run[[1]],
                           r = 2,
                           c = 1)
gpf2gpm_Cm1["Model"] <- "Model 1"


# MODEL 2
gpf2gpm_Cm2 <- deme2deme(births.p = dmC_m2.2$run[2,],
                           times = dmC_m2.2$run[[1]],
                           r = 2,
                           c = 1)
gpf2gpm_Cm2["Model"] <- "Model 2"

# PREVALENCE
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmC_m3.rda")
load("analyses/plots/solved_objects/FINAL/dmC_m4.2.rda")

# MODEL 3
gpf2gpm_Cm3 <- deme2deme(births.p = dmC_m3.1$run[2,],
                           times = dmC_m3.1$run[[1]],
                           r = 2,
                           c = 1)
gpf2gpm_Cm3["Model"] <- "Model 3"


# MODEL 4
gpf2gpm_Cm4 <- deme2deme(births.p = dmC_m4.2$run[2,],
                         times = dmC_m4.2$run[[1]],
                         r = 2,
                         c = 1)
gpf2gpm_Cm4["Model"] <- "Model 4"

# merge dataframes
gpf2gpm_C <- rbind(gpf2gpm_Cm1[1,], gpf2gpm_Cm2[1,],
                   gpf2gpm_Cm3[1,], gpf2gpm_Cm4[1,])

rm(dmC_m1.2, dmC_m2.1, dmC_m2.2, dmC_m3.1, dmC_m4.2)

###############################################################################
# SUBTYPE 02AG
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmAG_m1.rda")
load("analyses/plots/solved_objects/FINAL/dmAG_m2.rda")

# MODEL 1
gpf2gpm_AGm1 <- deme2deme(births.p = dmAG_m1.1$run[2,],
                          times = dmAG_m1.1$run[[1]],
                          r = 2,
                          c = 1)
gpf2gpm_AGm1["Model"] <- "Model 1"

# MODEL 2
gpf2gpm_AGm2 <- deme2deme(births.p = dmAG_m2.1$run[2,],
                          times = dmAG_m2.1$run[[1]],
                          r = 2,
                          c = 1)
gpf2gpm_AGm2["Model"] <- "Model 2"


# PREVALENCE
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmAG_m3.2.rda")
load("analyses/plots/solved_objects/FINAL/dmAG_m4.2.rda")

# MODEL 3
gpf2gpm_AGm3 <- deme2deme(births.p = dmAG_m3.2$run[2,],
                          times = dmAG_m3.2$run[[1]],
                          r = 2,
                          c = 1)
gpf2gpm_AGm3["Model"] <- "Model 3"

# MODEL 4
gpf2gpm_AGm4 <- deme2deme(births.p = dmAG_m4.2$run[2,],
                          times = dmAG_m4.2$run[[1]],
                          r = 2,
                          c = 1)
gpf2gpm_AGm4["Model"] <- "Model 4"

gpf2gpm_AG <- rbind(gpf2gpm_AGm1[1,], gpf2gpm_AGm2[1,],
                   gpf2gpm_AGm3[1,], gpf2gpm_AGm4[1,])

rm(dmAG_m1.1, dmAG_m2.1, dmAG_m3.2, dmAG_m4.2)

##############################################################################
# SUBTYPES COMBINED

# Load solved objects
load("analyses/plots/solved_objects/FINAL/dm_m2.2.rda")
load("analyses/plots/solved_objects/FINAL/dm_m3.2.rda")
load("analyses/plots/solved_objects/FINAL/dm_m4.2.rda")

#PREVALENCE
load("analyses/plots/solved_objects/FINAL/dm_m5.rda")
load("analyses/plots/solved_objects/FINAL/dm_m6.rda")
load("analyses/plots/solved_objects/FINAL/dm_m7.2.rda")

# Model 2
gpf2gpm_m2 <- deme2deme(births.p = dm_m2.2$run[2,],
                          times = dm_m2.2$run[[1]],
                          r = 2,
                          c = 1)
gpf2gpm_m2["Model"] <- "Model 2"


# Model 5 (prevalence) = Model 2
gpf2gpm_m5 <- deme2deme(births.p = dm_m5.1$run[2,],
                        times = dm_m5.1$run[[1]],
                        r = 2,
                        c = 1)
gpf2gpm_m5["Model"] <- "Model 5"


# Model 3
gpf2gpm_m3 <- deme2deme(births.p = dm_m3.2$run[2,],
                        times = dm_m3.2$run[[1]],
                        r = 2,
                        c = 1)
gpf2gpm_m3["Model"] <- "Model 3"


# Model 6 (prevalence) = Model 3
gpf2gpm_m6 <- deme2deme(births.p = dm_m6.1$run[2,],
                        times = dm_m6.1$run[[1]],
                        r = 2,
                        c = 1)
gpf2gpm_m6["Model"] <- "Model 6"


# Model 4
gpf2gpm_m4 <- deme2deme(births.p = dm_m4.2$run[2,],
                        times = dm_m4.2$run[[1]],
                        r = 2,
                        c = 1)
gpf2gpm_m4["Model"] <- "Model 4"


# Model 7 (prevalence) = Model 4
gpf2gpm_m7 <- deme2deme(births.p = dm_m7.2$run[2,],
                        times = dm_m7.2$run[[1]],
                        r = 2,
                        c = 1)
gpf2gpm_m7["Model"] <- "Model 7"


gpf2gpm_All <- rbind(gpf2gpm_m2[1,], gpf2gpm_m3[1,],gpf2gpm_m4[1,],
                     gpf2gpm_m5[1,], gpf2gpm_m6[1,],gpf2gpm_m7[1,])


save(gpf2gpm_C, gpf2gpm_AG, gpf2gpm_All, file="gpf2gpm.rda")

rm(dm_m2.2, dm_m3.2, dm_m4.2, dm_m5.1, dm_m6.1, dm_m7.2)
