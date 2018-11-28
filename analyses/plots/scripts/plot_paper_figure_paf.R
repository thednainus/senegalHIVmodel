####### create plots for paper
library(ggplot2)
library(phydynR)
library(reshape2)
library(senegalHIVmodel)

#### PAF ####

# SUBTYPE C: PAF
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmC_m2.rda")

# MODEL 2
Cm2_pafs <- df_pafs(births.p = dmC_m2.2$run[2,],
                    births.map = dmC_m2.2$MAP[[2]],
                    times = dmC_m2.2$run[[1]])
Cm2_pafs["Model"] <- "Subtype C"

Cm2_pafs.l <- melt(Cm2_pafs, id.vars = c("times", "lower", "upper",
                                         "group", "group2", "Model"))
colnames(Cm2_pafs.l)[4] <- "Deme"
colnames(Cm2_pafs.l)[7] <- "Linetype"


# SUBTYPE 02AG: PAF
# Load solved objects
load("analyses/plots/solved_objects/FINAL/dmAG_m2.rda")

# MODEL 2
AGm2_pafs <- df_pafs(births.p = dmAG_m2.1$run[2,],
                     births.map = dmAG_m2.1$MAP[[2]],
                     times = dmAG_m2.1$run[[1]])
AGm2_pafs["Model"] <- "Subtype 02_AG"

AGm2_pafs.l <- melt(AGm2_pafs, id.vars = c("times", "lower", "upper",
                                           "group", "group2", "Model"))
colnames(AGm2_pafs.l)[4] <- "Deme"
colnames(AGm2_pafs.l)[7] <- "Linetype"


# SUBTYPES COMBINED :PAF

#PREVALENCE
load("analyses/plots/solved_objects/FINAL/dm_m6.rda")

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
all_data <- rbind(Cm2_pafs.l, AGm2_pafs.l, m6_pafs.l)

#PLOT
p1_paf <- ggplot(all_data, aes(x = times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.40) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ylab("Population attributable fraction") +
  xlab("Time (years)") +
  scale_fill_grey()+ scale_colour_grey() + theme_bw() +
  theme(legend.position="bottom")
