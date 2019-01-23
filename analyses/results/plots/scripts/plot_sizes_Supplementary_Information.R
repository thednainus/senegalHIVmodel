####### create plots for the proportions of effective number of infections
# for subtypes C and 02_AG individually, and for all subtypes combined (B, C
# and 02_AG).

library(ggplot2)
library(phydynR)
library(reshape2)
library(senegalHIVmodel)

# SUBTYPE C
# Load solved objects
load("analyses/plots/solved_objects/dmC_m1.2.rda")
load("analyses/plots/solved_objects/dmC_m2.rda")

# MODEL 1
Cm1_sizes <- df_sizes_prop(sizes.p = dmC_m1.2$run[4,],
                           sizes.map = dmC_m1.2$MAP[4],
                           times = dmC_m1.2$run[[1]], Nrep = 1000, Ntime = 1000)
Cm1_sizes["Model"] <- "Model 1"

# MODEL 2
Cm2_sizes <- df_sizes_prop(sizes.p = dmC_m2.2$run[4,],
                           sizes.map = dmC_m2.2$MAP[4],
                           times = dmC_m2.2$run[[1]], Nrep = 1000, Ntime = 1000)
Cm2_sizes["Model"] <- "Model 2"


#PREVALENCE
# Load solved objects
load("analyses/plots/solved_objects/dmC_m3.rda")

# MODEL 3
Cm3_sizes <- df_sizes_prop(sizes.p = dmC_m3.1$run[4,],
                           sizes.map = dmC_m3.1$MAP[4],
                           times = dmC_m3.1$run[[1]], Nrep = 1000, Ntime = 1000)
Cm3_sizes["Model"] <- "Model 3"


#merge dataframes
SN_C <- rbind(Cm1_sizes, Cm2_sizes, Cm3_sizes)
SN_C["subtitle"] <- paste(SN_C$Model, SN_C$group2, sep=" - ")
SN_C.l <- melt(SN_C, id.vars = c("times", "lower", "upper", "group", "group2",
                                 "Model", "subtitle"))

colnames(SN_C.l)[4] <- "Deme"
colnames(SN_C.l)[8] <- "Linetype"

#PLOT
Cp1 <- ggplot(SN_C.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.60) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ggtitle("Subtype C") +
  ylab("Effective number of infections (proportion)") +
  xlab("Time (years)") +
  scale_fill_grey()+ scale_colour_grey() + theme_bw() +
  theme(legend.position="bottom")

###############################################################################
# SUBTYPE 02_AG
# Load solved objects
load("analyses/plots/solved_objects/dmAG_m1.rda")
load("analyses/plots/solved_objects/dmAG_m2.rda")

# Model 1
AGm1_sizes <- df_sizes_prop(sizes.p = dmAG_m1.1$run[4,],
                            sizes.map = dmAG_m1.1$MAP[4],
                            times = dmAG_m1.1$run[[1]], Nrep = 1000, Ntime = 1000)
AGm1_sizes["Model"] <- "Model 1"

# MODEL 2
AGm2_sizes <- df_sizes_prop(sizes.p = dmAG_m2.1$run[4,],
                            sizes.map = dmAG_m2.1$MAP[4],
                            times = dmAG_m2.1$run[[1]], Nrep = 1000, Ntime = 1000)
AGm2_sizes["Model"] <- "Model 2"

#PREVALENCE
# Load solved objects
load("analyses/plots/solved_objects/dmAG_m3.2.rda")

# Model 3
AGm3_sizes <- df_sizes_prop(sizes.p = dmAG_m3.2$run[4,],
                            sizes.map = dmAG_m3.2$MAP[4],
                            times = dmAG_m3.2$run[[1]], Nrep = 1000, Ntime = 1000)
AGm3_sizes["Model"] <- "Model 3"


#merge dataframes
SN_AG <- rbind(AGm1_sizes, AGm2_sizes, AGm3_sizes)
SN_AG["subtitle"] <- paste(SN_AG$Model, SN_AG$ group2, sep=" - ")
SN_AG.l <- melt(SN_AG, id.vars = c("times", "lower", "upper", "group", "group2",
                                   "Model", "subtitle"))

colnames(SN_AG.l)[4] <- "Deme"
colnames(SN_AG.l)[8] <- "Linetype"

#PLOT
AGp1 <- ggplot(SN_AG.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.60) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ggtitle("Subtype 02AG") +
  ylab("Effective number of infections (proportion)") +
  xlab("Time (years)") +
  scale_fill_grey()+ scale_colour_grey() + theme_bw() +
  theme(legend.position="bottom")


##############################################################################
# SUBTYPES COMBINED

# Load solved objects
load("analyses/plots/solved_objects/dm_m2.2.rda")
load("analyses/plots/solved_objects/dm_m3.2.rda")
load("analyses/plots/solved_objects/dm_m4.2.rda")

#PREVALENCE
load("analyses/plots/solved_objects/dm_m5.rda")
load("analyses/plots/solved_objects/dm_m7.2.rda")

# Model 2
m2_sizes <- df_sizes_prop(sizes.p = dm_m2.2$run[4,],
                          sizes.map = dm_m2.2$MAP[4],
                          times = dm_m2.2$run[[1]], Nrep = 1000, Ntime = 1000)
m2_sizes["Model"] <- "Model 2"

# Model 3
m3_sizes <- df_sizes_prop(sizes.p = dm_m3.2$run[4,],
                          sizes.map = dm_m3.2$MAP[4],
                          times = dm_m3.2$run[[1]], Nrep = 1000, Ntime = 1000)
m3_sizes["Model"] <- "Model 3"

# Model 4
m4_sizes <- df_sizes_prop(sizes.p = dm_m4.2$run[4,],
                          sizes.map = dm_m4.2$MAP[4],
                          times = dm_m4.2$run[[1]], Nrep = 1000, Ntime = 1000)
m4_sizes["Model"] <- "Model 4"


# Model 5 (prevalence) = Model 2
m5_sizes <- df_sizes_prop(sizes.p = dm_m5.1$run[4,],
                          sizes.map = dm_m5.1$MAP[4],
                          times = dm_m5.1$run[[1]], Nrep = 1000, Ntime = 1000)
m5_sizes["Model"] <- "Model 5"


# Model 7 (prevalence) = Model 4
m7_sizes <- df_sizes_prop(sizes.p = dm_m7.2$run[4,],
                          sizes.map = dm_m7.2$MAP[4],
                          times = dm_m7.2$run[[1]], Nrep = 1000, Ntime = 1000)
m7_sizes["Model"] <- "Model 7"

#merge dataframes (models 2, 3 and 4)
SN_m234 <- rbind(m2_sizes, m3_sizes, m4_sizes)
SN_m234.l <- melt(SN_m234, id.vars = c("times", "lower", "upper", "group",
                                       "group2", "Model"))

colnames(SN_m234.l)[4] <- "Deme"
colnames(SN_m234.l)[7] <- "Linetype"

#merge dataframes (subtypes 5 and 6)
SN_m57 <- rbind(m5_sizes, m7_sizes)
SN_m57.l <- melt(SN_m57, id.vars = c("times", "lower", "upper", "group",
                                     "group2", "Model"))

colnames(SN_m57.l)[4] <- "Deme"
colnames(SN_m57.l)[7] <- "Linetype"


#PLOT
m234.p1 <- ggplot(SN_m234.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.60) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ggtitle("Subtypes Combined: Models without the prevalence term") +
  ylab("Effective number of infections (proportion)") +
  xlab("Time (years)") +
  scale_fill_grey()+ scale_colour_grey() + theme_bw() +
  theme(legend.position="bottom")


m57.p1 <- ggplot(SN_m57.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.60) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ggtitle("Subtypes Combined: Models with the prevalence term") +
  ylab("Effective number of infections (proportion)") +
  xlab("Time (years)") +
  scale_fill_grey()+ scale_colour_grey() + theme_bw() +
  theme(legend.position="bottom")

