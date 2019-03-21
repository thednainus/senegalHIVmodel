####### create plots for paper
library(ggplot2)
library(phydynR)
library(reshape2)
library(senegalHIVmodel)

#### PAF ####

# SUBTYPE C: PAF
# Load solved objects
load("analyses/results/plots/solved_objects/dmC_m3.rda")

# MODEL 3
Cm3_pafs <- df_pafs(births.p = dmC_m3.1$run[2,],
                    births.map = dmC_m3.1$MAP[[2]],
                    times = dmC_m3.1$run[[1]])
Cm3_pafs["Model"] <- "Subtype C"

Cm3_pafs.l <- melt(Cm3_pafs, id.vars = c("times", "lower", "upper",
                                         "group", "group2", "Model"))
colnames(Cm3_pafs.l)[4] <- "Deme"
colnames(Cm3_pafs.l)[7] <- "Linetype"


# SUBTYPE 02AG: PAF
# Load solved objects
load("analyses/results/plots/solved_objects/dmAG_m3.2.rda")

# MODEL 3
AGm3_pafs <- df_pafs(births.p = dmAG_m3.2$run[2,],
                     births.map = dmAG_m3.2$MAP[[2]],
                     times = dmAG_m3.2$run[[1]])
AGm3_pafs["Model"] <- "Subtype 02_AG"

AGm3_pafs.l <- melt(AGm3_pafs, id.vars = c("times", "lower", "upper",
                                           "group", "group2", "Model"))
colnames(AGm3_pafs.l)[4] <- "Deme"
colnames(AGm3_pafs.l)[7] <- "Linetype"


# SUBTYPES COMBINED: PAF

#PREVALENCE
load("analyses/results/plots/solved_objects/dm_m5.rda")

# Model 5 (prevalence)
m5_pafs <- df_pafs(births.p = dm_m5.1$run[2,],
                   births.map = dm_m5.1$MAP[[2]],
                   times = dm_m5.1$run[[1]])
m5_pafs["Model"] <- "Subtypes combined"


m5_pafs.l <- melt(m5_pafs, id.vars = c("times", "lower", "upper",
                                       "group", "group2", "Model"))
colnames(m5_pafs.l)[4] <- "Deme"
colnames(m5_pafs.l)[7] <- "Linetype"


# Combine dataframes
all_data <- rbind(Cm3_pafs.l, AGm3_pafs.l, m5_pafs.l)

#PLOT
p1_paf <- ggplot(all_data, aes(x = times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.70) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ylab("Population attributable fraction") +
  xlab("Time (years)") +
  scale_fill_brewer() + scale_colour_brewer() + theme_bw() +
  theme(legend.position="bottom", text = element_text(size = 20))


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

