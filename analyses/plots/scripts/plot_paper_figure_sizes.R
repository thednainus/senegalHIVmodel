####### create plots for paper
library(ggplot2)
library(phydynR)
library(reshape2)
library(senegalHIVmodel)

#### Sizes ####

# SUBTYPE C: Sizes
# Load solved objects
load("analyses/plots/solved_objects/dmC_m4.2.rda")

# MODEL 4
Cm4_sizes <- df_sizes_prop(sizes.p = dmC_m4.2$run[4,],
                           sizes.map = dmC_m4.2$MAP[4],
                           times = dmC_m4.2$run[[1]], Nrep = 1000, Ntime = 1000)
Cm4_sizes["Model"] <- "Subtype C"

Cm4_sizes.l <- melt(Cm4_sizes, id.vars = c("times", "lower", "upper",
                                           "group", "group2", "Model"))
colnames(Cm4_sizes.l)[4] <- "Deme"
colnames(Cm4_sizes.l)[7] <- "Linetype"


# SUBTYPE 02AG: sizes
# Load solved objects
load("analyses/plots/solved_objects/dmAG_m4.2.rda")

# MODEL 4
AGm4_sizes <- df_sizes_prop(sizes.p = dmAG_m4.2$run[4,],
                            sizes.map = dmAG_m4.2$MAP[4],
                            times = dmAG_m4.2$run[[1]], Nrep = 1000, Ntime = 1000)
AGm4_sizes["Model"] <- "Subtype 02_AG"

AGm4_sizes.l <- melt(AGm4_sizes, id.vars = c("times", "lower", "upper",
                                             "group", "group2", "Model"))
colnames(AGm4_sizes.l)[4] <- "Deme"
colnames(AGm4_sizes.l)[7] <- "Linetype"


# SUBTYPES COMBINED: Sizes

#PREVALENCE
load("analyses/plots/solved_objects/dm_m6.rda")

# Model 6 (prevalence)
m6_sizes <- df_sizes_prop(sizes.p = dm_m6.1$run[4,],
                          sizes.map = dm_m6.1$MAP[4],
                          times = dm_m6.1$run[[1]], Nrep = 1000, Ntime = 1000)
m6_sizes["Model"] <- "Subtypes combined"

m6_sizes.l <- melt(m6_sizes, id.vars = c("times", "lower", "upper",
                                         "group", "group2", "Model"))
colnames(m6_sizes.l)[4] <- "Deme"
colnames(m6_sizes.l)[7] <- "Linetype"


# Combine dataframes
all_data <- rbind(Cm4_sizes.l, AGm4_sizes.l, m6_sizes.l)

#PLOT
p1_sizes <- ggplot(all_data, aes(x = times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.60) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ylab("Effective number of infections (proportion)") +
  xlab("Time (years)") +
  scale_fill_grey()+ scale_colour_grey() + theme_bw() +
  theme(legend.position="bottom")
