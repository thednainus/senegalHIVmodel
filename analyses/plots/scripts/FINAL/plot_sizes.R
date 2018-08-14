####### create plots for the effective number of infections
# for subtypes C and 02_AG, and for subtypes combined
library(ggplot2)
library(phydynR)
library(reshape2)

# SUBTYPE C
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmC_m1.2.rda")
load("analyses/plots/solved_objects/FINAL/dmC_m2.rda")

# MODEL 1
Cm1_sizes <- df_sizes(sizes.p = dmC_m1.2$run[4,],
                      sizes.map = dmC_m1.2$MAP[4],
                      times = dmC_m1.2$run[[1]], Nrep = 1000, Ntime = 1000)
Cm1_sizes["Model"] <- "Model 1"


# MODEL 2
Cm2_sizes <- df_sizes(sizes.p = dmC_m2.2$run[4,],
                        sizes.map = dmC_m2.2$MAP[4],
                        times = dmC_m2.2$run[[1]], Nrep = 1000, Ntime = 1000)
Cm2_sizes["Model"] <- "Model 2"


#PREVALENCE
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmC_m3.rda")
load("analyses/plots/solved_objects/FINAL/dmC_m4.2.rda")

# MODEL 3

Cm3_sizes <- df_sizes(sizes.p = dmC_m3.1$run[4,],
                      sizes.map = dmC_m3.1$MAP[4],
                      times = dmC_m3.1$run[[1]], Nrep = 1000, Ntime = 1000)
Cm3_sizes["Model"] <- "Model 3"

# MODEL 4

Cm4_sizes <- df_sizes(sizes.p = dmC_m4.2$run[4,],
                      sizes.map = dmC_m4.2$MAP[4],
                      times = dmC_m4.2$run[[1]], Nrep = 1000, Ntime = 1000)
Cm4_sizes["Model"] <- "Model 4"

#merge dataframes
SN_C <- rbind(Cm1_sizes, Cm2_sizes, Cm3_sizes, Cm4_sizes)
SN_C["subtitle"] <- paste(SN_C$Model, SN_C$group2, sep=" - ")
SN_C.l <- melt(SN_C, id.vars = c("times", "lower", "upper", "group", "group2",
                                 "Model", "subtitle"))
#PLOT
Cp1 <- ggplot(SN_C.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour=group, linetype=variable)) +
  facet_wrap(subtitle ~ ., scales = "free", ncol = 2) +
  ggtitle("Subtype C") + ylab("Effective number of infections") + theme_bw()

###############################################################################
# SUBTYPE 02_AG
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmAG_m1.rda")
load("analyses/plots/solved_objects/FINAL/dmAG_m2.rda")

AGm1_sizes <- df_sizes(sizes.p = dmAG_m1.1$run[4,],
                       sizes.map = dmAG_m1.1$MAP[4],
                       times = dmAG_m1.1$run[[1]], Nrep = 1000, Ntime = 1000)
AGm1_sizes["Model"] <- "Model 1"


# MODEL 2

AGm2_sizes <- df_sizes(sizes.p = dmAG_m2.1$run[4,],
                       sizes.map = dmAG_m2.1$MAP[4],
                       times = dmAG_m2.1$run[[1]], Nrep = 1000, Ntime = 1000)
AGm2_sizes["Model"] <- "Model 2"

#PREVALENCE
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmAG_m3.2.rda")
load("analyses/plots/solved_objects/FINAL/dmAG_m4.2.rda")

AGm3_sizes <- df_sizes(sizes.p = dmAG_m3.2$run[4,],
                       sizes.map = dmAG_m3.2$MAP[4],
                       times = dmAG_m3.2$run[[1]], Nrep = 1000, Ntime = 1000)
AGm3_sizes["Model"] <- "Model 3"


# MODEL 4

AGm4_sizes <- df_sizes(sizes.p = dmAG_m4.2$run[4,],
                       sizes.map = dmAG_m4.2$MAP[4],
                       times = dmAG_m4.2$run[[1]], Nrep = 1000, Ntime = 1000)
AGm4_sizes["Model"] <- "Model 4"



#merge dataframes
SN_AG <- rbind(AGm1_sizes, AGm2_sizes, AGm3_sizes, AGm4_sizes)
SN_AG["subtitle"] <- paste(SN_AG$Model, SN_AG$ group2, sep=" - ")
SN_AG.l <- melt(SN_AG, id.vars = c("times", "lower", "upper", "group", "group2",
                                   "Model", "subtitle"))

#PLOT
AGp1 <- ggplot(SN_AG.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour=group, linetype=variable)) +
  facet_wrap(subtitle ~ ., scales = "free", ncol = 2) +
  ggtitle("Subtype 02AG") + ylab("Effective number of infections") + theme_bw()


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
m2_sizes <- df_sizes(sizes.p = dm_m2.2$run[4,],
                     sizes.map = dm_m2.2$MAP[4],
                     times = dm_m2.2$run[[1]], Nrep = 1000, Ntime = 1000)
m2_sizes["Model"] <- "Model 2"

# Model 5 (prevalence) = Model 2
m5_sizes <- df_sizes(sizes.p = dm_m5.1$run[4,],
                     sizes.map = dm_m5.1$MAP[4],
                     times = dm_m5.1$run[[1]], Nrep = 1000, Ntime = 1000)
m5_sizes["Model"] <- "Model 5"


# Model 3
m3_sizes <- df_sizes(sizes.p = dm_m3.2$run[4,],
                     sizes.map = dm_m3.2$MAP[4],
                     times = dm_m3.2$run[[1]], Nrep = 1000, Ntime = 1000)
m3_sizes["Model"] <- "Model 3"

# Model 6 (prevalence) = Model 3
m6_sizes <- df_sizes(sizes.p = dm_m6.1$run[4,],
                     sizes.map = dm_m6.1$MAP[4],
                     times = dm_m6.1$run[[1]], Nrep = 1000, Ntime = 1000)
m6_sizes["Model"] <- "Model 6"


# Model 4
m4_sizes <- df_sizes(sizes.p = dm_m4.2$run[4,],
                     sizes.map = dm_m4.2$MAP[4],
                     times = dm_m4.2$run[[1]], Nrep = 1000, Ntime = 1000)
m4_sizes["Model"] <- "Model 4"

# Model 7 (prevalence) = Model 4
m7_sizes <- df_sizes(sizes.p = dm_m7.2$run[4,],
                     sizes.map = dm_m7.2$MAP[4],
                     times = dm_m7.2$run[[1]], Nrep = 1000, Ntime = 1000)
m7_sizes["Model"] <- "Model 7"

#merge dataframes
SN_m2and5 <- rbind(m2_sizes, m5_sizes)
SN_m2and5["subtitle"] <- paste(SN_m2and5$Model, SN_m2and5$group2, sep=" - ")
SN_m2and5.l <- melt(SN_m2and5, id.vars = c("times", "lower", "upper", "group",
                                           "group2", "Model", "subtitle"))


SN_m3and6 <- rbind(m3_sizes, m6_sizes)
SN_m3and6["subtitle"] <- paste(SN_m3and6$Model, SN_m3and6$group2, sep=" - ")
SN_m3and6.l <- melt(SN_m3and6, id.vars = c("times", "lower", "upper", "group",
                                           "group2", "Model", "subtitle"))


SN_m4and7 <- rbind(m4_sizes, m7_sizes)
SN_m4and7["subtitle"] <- paste(SN_m4and7$Model, SN_m4and7$group2, sep=" - ")
SN_m4and7.l <- melt(SN_m4and7, id.vars = c("times", "lower", "upper", "group",
                                           "group2", "Model", "subtitle"))


#PLOT
m2and5.p1 <- ggplot(SN_m2and5.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour=group, linetype=variable)) +
  facet_wrap(subtitle ~ ., scales = "free", ncol = 2) +
  ggtitle("Subtypes Combined") + ylab("Effective number of infections") +
  theme_bw()

m3and6.p1 <- ggplot(SN_m3and6.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour=group, linetype=variable)) +
  facet_wrap(subtitle ~ ., scales = "free", ncol = 2) +
  ggtitle("Subtypes Combined") + ylab("Effective number of infections") +
  theme_bw()

m4and7.p1 <- ggplot(SN_m4and7.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour=group, linetype=variable)) +
  facet_wrap(subtitle ~ ., scales = "free", ncol = 2) +
  ggtitle("Subtypes Combined") + ylab("Effective number of infections") +
  theme_bw()
