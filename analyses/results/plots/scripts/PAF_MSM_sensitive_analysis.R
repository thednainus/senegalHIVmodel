# Script to plot the sensitive analyses for MSM PAF for the combined
# analyses (using sequences from subtypes B, C and CRF 02_AG).

# SUBTYPES COMBINED

# Load solved objects
load("analyses/results/plots/solved_objects/dm_m2.2.rda")
load("analyses/results/plots/solved_objects/dm_m3.2.rda")
load("analyses/results/plots/solved_objects/dm_m4.2.rda")

#PREVALENCE
load("analyses/results/plots/solved_objects/dm_m5.rda")
load("analyses/results/plots/solved_objects/dm_m6.rda")
load("analyses/results/plots/solved_objects/dm_m7.2.rda")

# Model 2
m2_pafs <- df_pafs(births.p = dm_m2.2$run[2,],
                   births.map = dm_m2.2$MAP[[2]],
                   times = dm_m2.2$run[[1]])
m2_pafs["Model"] <- "Model 1"
m2_pafs_msm <- subset(m2_pafs, group == "msm")


# Model 3
m3_pafs <- df_pafs(births.p = dm_m3.2$run[2,],
                   births.map = dm_m3.2$MAP[[2]],
                   times = dm_m3.2$run[[1]])
m3_pafs["Model"] <- "Model 2"
m3_pafs_msm <- subset(m3_pafs, group == "msm")

# Model 4
m4_pafs <- df_pafs(births.p = dm_m4.2$run[2,],
                   births.map = dm_m4.2$MAP[[2]],
                   times = dm_m4.2$run[[1]])
m4_pafs["Model"] <- "Model 3"
m4_pafs_msm <- subset(m4_pafs, group == "msm")

# Model 5 (prevalence)
m5_pafs <- df_pafs(births.p = dm_m5.1$run[2,],
                   births.map = dm_m5.1$MAP[[2]],
                   times = dm_m5.1$run[[1]])
m5_pafs["Model"] <- "Model 4"
m5_pafs_msm <- subset(m5_pafs, group == "msm")


# Model 6 (prevalence) = Model 3
m6_pafs <- df_pafs(births.p = dm_m6.1$run[2,],
                   births.map = dm_m6.1$MAP[[2]],
                   times = dm_m6.1$run[[1]])
m6_pafs["Model"] <- "Model 5"
m6_pafs_msm <- subset(m6_pafs, group == "msm")

# Model 7 (prevalence) = Model 4
m7_pafs <- df_pafs(births.p = dm_m7.2$run[2,],
                   births.map = dm_m7.2$MAP[[2]],
                   times = dm_m7.2$run[[1]])
m7_pafs["Model"] <- "Model 6"
m7_pafs_msm <- subset(m7_pafs, group == "msm")


# merge msm paf dataframes
all_msm_pafs <- rbind(m2_pafs_msm, m3_pafs_msm, m4_pafs_msm,
                      m5_pafs_msm, m6_pafs_msm, m7_pafs_msm)
all_msm_pafs$Model <- as.factor(all_msm_pafs$Model)
all_msm_pafs$group <- as.factor(all_msm_pafs$group)
all_msm_pafs <- all_msm_pafs[c(1:4,6:8)]


all_msm_pafs.l <- melt(all_msm_pafs, id.vars = c("times", "lower", "upper",
                                                 "group", "Model"))
colnames(all_msm_pafs.l)[4] <- "Deme"
colnames(all_msm_pafs.l)[6] <- "Linetype"



#PLOT
msm_paf <- ggplot(all_msm_pafs.l, aes(x = times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.70) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(Model ~ ., scales = "free") +
  ylab("Population attributable fraction") +
  xlab("Time (years)") +
  scale_fill_manual(values = "#2171B5") + scale_colour_manual(values = "#2171B5") + theme_bw() +
  theme(legend.position="bottom", text = element_text(size = 20))


# creating new dataframe to add points and confidence intervals for
# 1 year PAF estimated at Mukandavire et al. 2018's paper
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6055131/
d = data.frame(times=c(rep(1995, 6), rep(2005, 6), rep(2015, 6)),
               mean=c(rep(0.362, 6), rep(0.289, 6), rep(0.514, 6)),
               lower=c(rep(0.215, 6), rep(0.133, 6), rep(0.273, 6)),
               upper=c(rep(0.525, 6), rep(0.428, 6), rep(0.667, 6)),
               Deme=c(rep("msm", 6)),
               Model=c("Model 1", "Model 2", "Model 3",
                       "Model 4", "Model 5", "Model 6"))

p <- msm_paf +
  geom_errorbar(data=d, mapping=aes(x=times, ymin=upper, ymax=lower), width=0.8, size=0.5) +
  geom_point(data=d, mapping=aes(x=times, y=mean), size = 1)



