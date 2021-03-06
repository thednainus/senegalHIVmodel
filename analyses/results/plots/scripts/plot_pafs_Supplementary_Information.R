####### create plots for PAFs for subtypes C, 02_AG and subtypes combined
library(ggplot2)
library(phydynR)
library(reshape2)
library(senegalHIVmodel)

# SUBTYPE C
# Load solved objects
load("analyses/results/plots/solved_objects/dmC_m1.2.rda")
load("analyses/results/plots/solved_objects/dmC_m2.rda")

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
load("analyses/results/plots/solved_objects/dmC_m4.2.rda")

# MODEL 4
Cm4_pafs <- df_pafs(births.p = dmC_m4.2$run[2,],
                    births.map = dmC_m4.2$MAP[[2]],
                    times = dmC_m4.2$run[[1]])
Cm4_pafs["Model"] <- "Model 4"


# bind together the different dataframes
#merge dataframes
SN_C.paf <- rbind(Cm1_pafs, Cm2_pafs, Cm4_pafs)
SN_C.paf.l <- melt(SN_C.paf, id.vars = c("times", "lower", "upper", "group",
                                         "group2", "Model"))

colnames(SN_C.paf.l)[4] <- "Deme"
colnames(SN_C.paf.l)[7] <- "Linetype"

#PLOT
C_pafs.p1 <- ggplot(SN_C.paf.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.70) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ggtitle("Subtype C") +
  ylab("Population attributable fraction") +
  xlab("Time (years)") +
  scale_fill_brewer() + scale_colour_brewer() + theme_bw() +
  theme(legend.position="bottom", text = element_text(size = 20))

###############################################################################
# SUBTYPE 02AG
# Load solved objects
load("analyses/results/plots/solved_objects/dmAG_m1.rda")
load("analyses/results/plots/solved_objects/dmAG_m2.rda")

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
load("analyses/results/plots/solved_objects/dmAG_m4.2.rda")

# MODEL 4
AGm4_pafs <- df_pafs(births.p = dmAG_m4.2$run[2,],
                     births.map = dmAG_m4.2$MAP[[2]],
                     times = dmAG_m4.2$run[[1]])
AGm4_pafs["Model"] <- "Model 4"

# bind together the different dataframes
#merge dataframes
SN_AG.paf <- rbind(AGm1_pafs, AGm2_pafs, AGm4_pafs)
SN_AG.paf.l <- melt(SN_AG.paf, id.vars = c("times", "lower", "upper", "group",
                                          "group2", "Model"))

colnames(SN_AG.paf.l)[4] <- "Deme"
colnames(SN_AG.paf.l)[7] <- "Linetype"

#PLOT
AG_pafs.p1 <- ggplot(SN_AG.paf.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.70) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ggtitle("Subtype 02AG") +
  ylab("Population attributable fraction") +
  xlab("Time (years)") +
  scale_fill_brewer() + scale_colour_brewer() + theme_bw() +
  theme(legend.position="bottom", text = element_text(size = 20))

##############################################################################
# SUBTYPES COMBINED

# Load solved objects
load("analyses/results/plots/solved_objects/dm_m2.2.rda")
load("analyses/results/plots/solved_objects/dm_m3.2.rda")
load("analyses/results/plots/solved_objects/dm_m4.2.rda")

#PREVALENCE
load("analyses/results/plots/solved_objects/dm_m6.rda")
load("analyses/results/plots/solved_objects/dm_m7.2.rda")

# Model 2
m2_pafs <- df_pafs(births.p = dm_m2.2$run[2,],
                     births.map = dm_m2.2$MAP[[2]],
                     times = dm_m2.2$run[[1]])
m2_pafs["Model"] <- "Model 1"

# Model 3
m3_pafs <- df_pafs(births.p = dm_m3.2$run[2,],
                   births.map = dm_m3.2$MAP[[2]],
                   times = dm_m3.2$run[[1]])
m3_pafs["Model"] <- "Model 2"

# Model 4
m4_pafs <- df_pafs(births.p = dm_m4.2$run[2,],
                   births.map = dm_m4.2$MAP[[2]],
                   times = dm_m4.2$run[[1]])
m4_pafs["Model"] <- "Model 3"

# Model 6 (prevalence) = Model 3
m6_pafs <- df_pafs(births.p = dm_m6.1$run[2,],
                   births.map = dm_m6.1$MAP[[2]],
                   times = dm_m6.1$run[[1]])
m6_pafs["Model"] <- "Model 5"

# Model 7 (prevalence) = Model 4
m7_pafs <- df_pafs(births.p = dm_m7.2$run[2,],
                   births.map = dm_m7.2$MAP[[2]],
                   times = dm_m7.2$run[[1]])
m7_pafs["Model"] <- "Model 6"

# bind together the different dataframes
#merge dataframes
SN_m234.paf <- rbind(m2_pafs, m3_pafs, m4_pafs)
SN_m234.paf.l <- melt(SN_m234.paf, id.vars = c("times", "lower", "upper",
                                               "group", "group2", "Model"))

colnames(SN_m234.paf.l)[4] <- "Deme"
colnames(SN_m234.paf.l)[7] <- "Linetype"

SN_m67.paf <- rbind(m6_pafs, m7_pafs)
SN_m67.paf.l <- melt(SN_m67.paf, id.vars = c("times", "lower", "upper",
                                             "group", "group2", "Model"))

colnames(SN_m67.paf.l)[4] <- "Deme"
colnames(SN_m67.paf.l)[7] <- "Linetype"

# creating new dataframe to add points and confidence intervals for
# 1 year PAF estimated at Mukandavire et al. 2018's paper
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6055131/

d = data.frame(times=c(rep(1995, 9), rep(2005, 9), rep(2015, 9)),
               mean=c(NA, NA, 0.362, NA, NA, 0.362, NA, NA, 0.362,
                      NA, NA, 0.289, NA, NA, 0.289, NA, NA, 0.289,
                      NA, NA, 0.514, NA, NA, 0.514, NA, NA, 0.514),
               lower=c(NA, NA, 0.215, NA, NA, 0.215, NA, NA, 0.215,
                       NA, NA, 0.133, NA, NA, 0.133, NA, NA, 0.133,
                       NA, NA, 0.273, NA, NA, 0.273, NA, NA, 0.273),
               upper=c(NA, NA, 0.525, NA, NA, 0.525, NA, NA, 0.525,
                       NA, NA, 0.428, NA, NA, 0.428, NA, NA, 0.428,
                       NA, NA, 0.667, NA, NA, 0.667, NA, NA, 0.667),
               Deme=c("gpm", "gpf", "msm", "gpm", "gpf", "msm", "gpm", "gpf", "msm",
                      "gpm", "gpf", "msm", "gpm", "gpf", "msm", "gpm", "gpf", "msm",
                      "gpm", "gpf", "msm", "gpm", "gpf", "msm", "gpm", "gpf", "msm"),
               Model=c(rep("Model 1", 3), rep("Model 2", 3), rep("Model 3", 3),
                       rep("Model 1", 3), rep("Model 2", 3), rep("Model 3", 3),
                       rep("Model 1", 3), rep("Model 2", 3), rep("Model 3", 3)))


#PLOT
m234_pafs.p1 <- ggplot(SN_m234.paf.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.70) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  geom_errorbar(data = d, mapping = aes(x = times, ymin = upper, ymax = lower),
                width=0.8, size=0.5) +
  geom_point(data = d, mapping = aes(x = times, y = mean), size = 1) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ggtitle("Subtypes Combined: Models without the prevalence term") +
  ylab("Population attributable fraction") +
  xlab("Time (years)") +
  scale_fill_brewer() + scale_colour_brewer() + theme_bw() +
  theme(legend.position="bottom", text = element_text(size = 20))

# creating new dataframe to add points and confidence intervals for
# 1 year PAF estimated at Mukandavire et al. 2018's paper
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6055131/

d = data.frame(times=c(rep(1995, 6), rep(2005, 6), rep(2015, 6)),
               mean=c(NA, NA, 0.362, NA, NA, 0.362,
                      NA, NA, 0.289, NA, NA, 0.289,
                      NA, NA, 0.514, NA, NA, 0.514),
               lower=c(NA, NA, 0.215, NA, NA, 0.215,
                       NA, NA, 0.133, NA, NA, 0.133,
                       NA, NA, 0.273, NA, NA, 0.273),
               upper=c(NA, NA, 0.525, NA, NA, 0.525,
                       NA, NA, 0.428, NA, NA, 0.428,
                       NA, NA, 0.667, NA, NA, 0.667),
               Deme=c("gpm", "gpf", "msm", "gpm", "gpf", "msm",
                      "gpm", "gpf", "msm", "gpm", "gpf", "msm",
                      "gpm", "gpf", "msm", "gpm", "gpf", "msm"),
               Model=c(rep("Model 5", 3), rep("Model 6", 3),
                       rep("Model 5", 3), rep("Model 6", 3),
                       rep("Model 5", 3), rep("Model 6", 3)))


m67_pafs.p1 <- ggplot(SN_m67.paf.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.70) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  geom_errorbar(data = d, mapping = aes(x = times, ymin = upper, ymax = lower),
                width=0.8, size=0.5) +
  geom_point(data = d, mapping = aes(x = times, y = mean), size = 1) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ggtitle("Subtypes Combined: Models with the prevalence term") +
  ylab("Population attributable fraction") +
  xlab("Time (years)") +
  scale_fill_brewer() + scale_colour_brewer() + theme_bw() +
  theme(legend.position="bottom", text = element_text(size = 20))



