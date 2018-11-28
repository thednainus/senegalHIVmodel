####### create plots for PAFs for subtypes C, 02_AG and subtypes combined
library(ggplot2)
library(phydynR)
library(reshape2)
library(senegalHIVmodel)

# SUBTYPE C
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmC_m1.2.rda")
load("analyses/plots/solved_objects/FINAL/dmC_m2.rda")

# MODEL 1
Cm1_pafs <- df_pafs(births.p = dmC_m1.2$run[2,],
                    births.map = dmC_m1.2$MAP[[2]],
                    times = dmC_m1.2$run[[1]])
Cm1_pafs["Model"] <- "Model 1"

# MODEL 2
Cm2_pafs <- df_pafs(births.p = dmC_m2.2$run[2,],
                    births.map = dmC_m2.2$MAP[[2]],
                    times = dmC_m2.2$run[[1]])
Cm2_pafs["Model"] <- "Model 2"


# PREVALENCE
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmC_m3.rda")
load("analyses/plots/solved_objects/FINAL/dmC_m4.2.rda")

# MODEL 3
Cm3_pafs <- df_pafs(births.p = dmC_m3.1$run[2,],
                    births.map = dmC_m3.1$MAP[[2]],
                    times = dmC_m3.1$run[[1]])
Cm3_pafs["Model"] <- "Model 3"

# MODEL 4
Cm4_pafs <- df_pafs(births.p = dmC_m4.2$run[2,],
                    births.map = dmC_m4.2$MAP[[2]],
                    times = dmC_m4.2$run[[1]])
Cm4_pafs["Model"] <- "Model 4"

# bind together the different dataframes
#merge dataframes
SN_C.paf <- rbind(Cm1_pafs, Cm2_pafs, Cm3_pafs, Cm4_pafs)
SN_C.paf["subtitle"] <- paste(SN_C.paf$Model, SN_C.paf$group2, sep=" - ")
SN_C.paf.l <- melt(SN_C.paf, id.vars = c("times", "lower", "upper", "group",
                                         "group2", "Model", "subtitle"))

#PLOT
C_pafs.p1 <- ggplot(SN_C.paf.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour=group, linetype=variable)) +
  facet_wrap(subtitle ~ ., scales = "free", ncol = 2) +
  ggtitle("Subtype C") + ylab("PAF") + theme_bw()

###############################################################################
# SUBTYPE 02AG
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmAG_m1.rda")
load("analyses/plots/solved_objects/FINAL/dmAG_m2.rda")

# MODEL 1
AGm1_pafs <- df_pafs(births.p = dmAG_m1.1$run[2,],
                     births.map = dmAG_m1.1$MAP[[2]],
                     times = dmAG_m1.1$run[[1]])
AGm1_pafs["Model"] <- "Model 1"

# MODEL 2
AGm2_pafs <- df_pafs(births.p = dmAG_m2.1$run[2,],
                     births.map = dmAG_m2.1$MAP[[2]],
                     times = dmAG_m2.1$run[[1]])
AGm2_pafs["Model"] <- "Model 2"


# PREVALENCE
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmAG_m3.2.rda")
load("analyses/plots/solved_objects/FINAL/dmAG_m4.2.rda")

# MODEL 3
AGm3_pafs <- df_pafs(births.p = dmAG_m3.2$run[2,],
                     births.map = dmAG_m3.2$MAP[[2]],
                     times = dmAG_m3.2$run[[1]])
AGm3_pafs["Model"] <- "Model 3"

# MODEL 4
AGm4_pafs <- df_pafs(births.p = dmAG_m4.2$run[2,],
                     births.map = dmAG_m4.2$MAP[[2]],
                     times = dmAG_m4.2$run[[1]])
AGm4_pafs["Model"] <- "Model 4"

# bind together the different dataframes
#merge dataframes
SN_AG.paf <- rbind(AGm1_pafs, AGm2_pafs, AGm3_pafs, AGm4_pafs)
SN_AG.paf["subtitle"] <- paste(SN_AG.paf$Model, SN_AG.paf$group2, sep=" - ")
SN_AG.paf.l <- melt(SN_AG.paf, id.vars = c("times", "lower", "upper", "group",
                                          "group2", "Model", "subtitle"))

#PLOT
AG_pafs.p1 <- ggplot(SN_AG.paf.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour=group, linetype=variable)) +
  facet_wrap(subtitle ~ ., scales = "free", ncol = 2) +
  ggtitle("Subtype 02AG") + ylab("PAF") + theme_bw()

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
m2_pafs <- df_pafs(births.p = dm_m2.2$run[2,],
                     births.map = dm_m2.2$MAP[[2]],
                     times = dm_m2.2$run[[1]])
m2_pafs["Model"] <- "Model 2"


# Model 5 (prevalence) = Model 2
m5_pafs <- df_pafs(births.p = dm_m5.1$run[2,],
                   births.map = dm_m5.1$MAP[[2]],
                   times = dm_m5.1$run[[1]])
m5_pafs["Model"] <- "Model 5"


# Model 3
m3_pafs <- df_pafs(births.p = dm_m3.2$run[2,],
                   births.map = dm_m3.2$MAP[[2]],
                   times = dm_m3.2$run[[1]])
m3_pafs["Model"] <- "Model 3"


# Model 6 (prevalence) = Model 3
m6_pafs <- df_pafs(births.p = dm_m6.1$run[2,],
                   births.map = dm_m6.1$MAP[[2]],
                   times = dm_m6.1$run[[1]])
m6_pafs["Model"] <- "Model 6"


# Model 4
m4_pafs <- df_pafs(births.p = dm_m4.2$run[2,],
                   births.map = dm_m4.2$MAP[[2]],
                   times = dm_m4.2$run[[1]])
m4_pafs["Model"] <- "Model 4"


# Model 7 (prevalence) = Model 4
m7_pafs <- df_pafs(births.p = dm_m7.2$run[2,],
                   births.map = dm_m7.2$MAP[[2]],
                   times = dm_m7.2$run[[1]])
m7_pafs["Model"] <- "Model 7"


# bind together the different dataframes
#merge dataframes
SN_m2and5.paf <- rbind(m2_pafs, m5_pafs)
SN_m2and5.paf["subtitle"] <- paste(SN_m2and5.paf$Model, SN_m2and5.paf$group2,
                                   sep=" - ")
SN_m2and5.paf.l <- melt(SN_m2and5.paf, id.vars = c("times", "lower", "upper",
                                                   "group", "group2", "Model",
                                                   "subtitle"))

SN_m3and6.paf <- rbind(m3_pafs, m6_pafs)
SN_m3and6.paf["subtitle"] <- paste(SN_m3and6.paf$Model, SN_m3and6.paf$group2,
                                   sep=" - ")
SN_m3and6.paf.l <- melt(SN_m3and6.paf, id.vars = c("times", "lower", "upper",
                                                   "group", "group2", "Model",
                                                   "subtitle"))


SN_m4and7.paf <- rbind(m4_pafs, m7_pafs)
SN_m4and7.paf["subtitle"] <- paste(SN_m4and7.paf$Model, SN_m4and7.paf$group2,
                                   sep=" - ")
SN_m4and7.paf.l <- melt(SN_m4and7.paf, id.vars = c("times", "lower", "upper",
                                                   "group", "group2", "Model",
                                                   "subtitle"))

#PLOT
m2and5_pafs.p1 <- ggplot(SN_m2and5.paf.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour=group, linetype=variable)) +
  facet_wrap(subtitle ~ ., scales = "free", ncol = 2) +
  ggtitle("Subtypes Combined") + ylab("PAF") + theme_bw()

m3and6_pafs.p1 <- ggplot(SN_m3and6.paf.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour=group, linetype=variable)) +
  facet_wrap(subtitle ~ ., scales = "free", ncol = 2) +
  ggtitle("Subtypes Combined") + ylab("PAF") + theme_bw()

m4and7_pafs.p1 <- ggplot(SN_m4and7.paf.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour=group, linetype=variable)) +
  facet_wrap(subtitle ~ ., scales = "free", ncol = 2) +
  ggtitle("Subtypes Combined") + ylab("PAF") + theme_bw()

