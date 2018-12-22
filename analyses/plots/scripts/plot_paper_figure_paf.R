####### create plots for paper
library(ggplot2)
library(phydynR)
library(reshape2)
library(senegalHIVmodel)

#### PAF ####

# SUBTYPE C: PAF
# Load solved objects
load("analyses/plots/solved_objects/dmC_m4.2.rda")

# MODEL 4
Cm4_pafs <- df_pafs(births.p = dmC_m4.2$run[2,],
                    births.map = dmC_m4.2$MAP[[2]],
                    times = dmC_m4.2$run[[1]])
Cm4_pafs["Model"] <- "Subtype C"

Cm4_pafs.l <- melt(Cm4_pafs, id.vars = c("times", "lower", "upper",
                                         "group", "group2", "Model"))
colnames(Cm4_pafs.l)[4] <- "Deme"
colnames(Cm4_pafs.l)[7] <- "Linetype"


# SUBTYPE 02AG: PAF
# Load solved objects
load("analyses/plots/solved_objects/dmAG_m4.2.rda")

# MODEL 2
AGm4_pafs <- df_pafs(births.p = dmAG_m4.2$run[2,],
                     births.map = dmAG_m4.2$MAP[[2]],
                     times = dmAG_m4.2$run[[1]])
AGm4_pafs["Model"] <- "Subtype 02_AG"

AGm4_pafs.l <- melt(AGm4_pafs, id.vars = c("times", "lower", "upper",
                                           "group", "group2", "Model"))
colnames(AGm4_pafs.l)[4] <- "Deme"
colnames(AGm4_pafs.l)[7] <- "Linetype"


# SUBTYPES COMBINED :PAF

#PREVALENCE
load("analyses/plots/solved_objects/dm_m6.rda")

# Model 6 (prevalence)
m6_pafs <- df_pafs(births.p = dm_m6.1$run[2,],
                   births.map = dm_m6.1$MAP[[2]],
                   times = dm_m6.1$run[[1]])
m6_pafs["Model"] <- "Subtypes combined"

m6_pafs.l <- melt(m6_pafs, id.vars = c("times", "lower", "upper",
                                       "group", "group2", "Model"))
colnames(m6_pafs.l)[4] <- "Deme"
colnames(m6_pafs.l)[7] <- "Linetype"


# Combine dataframes
all_data <- rbind(Cm4_pafs.l, AGm4_pafs.l, m6_pafs.l)

#PLOT
p1_paf <- ggplot(all_data, aes(x = times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.60) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ylab("Population attributable fraction") +
  xlab("Time (years)") +
  scale_fill_grey()+ scale_colour_grey() + theme_bw() +
  theme(legend.position="bottom")


# creating new dataframe to add points and confidence intervals for
# 1 year PAF estimated at Mukandavire et al. 2018's paper
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6055131/

d = data.frame(times=c(rep(1995, 9), rep(2005, 9), rep(2015, 9)),
               mean=c(NA, NA, NA, NA, NA, NA, NA, NA, 0.362,
                      NA, NA, NA, NA, NA, NA, NA, NA, 0.289,
                      NA, NA, NA, NA, NA, NA, NA, NA, 0.514),
               lower=c(NA, NA, NA, NA, NA, NA, NA, NA, 0.215,
                       NA, NA, NA, NA, NA, NA, NA, NA, 0.133,
                       NA, NA, NA, NA, NA, NA, NA, NA, 0.273),
               upper=c(NA, NA, NA, NA, NA, NA, NA, NA, 0.525,
                       NA, NA, NA, NA, NA, NA, NA, NA, 0.428,
                       NA, NA, NA, NA, NA, NA, NA, NA, 0.667),
               Deme=c("gpm", "gpf", "msm", "gpm", "gpf", "msm", "gpm", "gpf", "msm",
                      "gpm", "gpf", "msm", "gpm", "gpf", "msm", "gpm", "gpf", "msm",
                      "gpm", "gpf", "msm", "gpm", "gpf", "msm", "gpm", "gpf", "msm"),
               Model=c(rep("Subtype C", 3), rep("Subtype 02_AG", 3), rep("Subtypes combined", 3),
                       rep("Subtype C", 3), rep("Subtype 02_AG", 3), rep("Subtypes combined", 3),
                       rep("Subtype C", 3), rep("Subtype 02_AG", 3), rep("Subtypes combined", 3)))

p <- p1_paf +
  geom_errorbar(data=d, mapping=aes(x=times, ymin=upper, ymax=lower), width=0.8, size=0.5) +
  geom_point(data=d, mapping=aes(x=times, y=mean), size = 1)

