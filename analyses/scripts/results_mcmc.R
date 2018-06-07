# Summary of the MCMC runs

# Model2
m2.r1 <- readRDS("analyses/scripts/Model2/Preliminary_results/newModel_comZmatrix2.RDS")
m2.r2 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/out_38485921.rds")
m2.r3 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/out_38485922.rds")
m2.r4 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/out_38485923.rds")
m2.r5 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/out_38485924.rds")
m2.r6 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/out_38485925.rds")
m2.r7 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/out_38485927.rds")
m2.r8 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/out_38485928.rds")
m2.r9 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/out_38485929.rds")
m2.r10 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/out_38485930.rds")
m2.r11 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/out_38485932.rds")

#Model2: longer runs
m2.l.r1 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/longerRuns/out_38537926_Model2LongerRuns.rds")
m2.l.r2 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/longerRuns/out_38537927_Model2LongerRuns.rds")
m2.l.r3 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/longerRuns/out_38537928_Model2LongerRuns.rds")
m2.l.r4 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/longerRuns/out_38537929_Model2LongerRuns.rds")
m2.l.r5 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/longerRuns/out_38537930_Model2LongerRuns.rds")
m2.l.r6 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/longerRuns/out_38537931_Model2LongerRuns.rds")
m2.l.r7 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/longerRuns/out_38537932_Model2LongerRuns.rds")
m2.l.r8 <- readRDS("analyses/scripts/Model2/Preliminary_results/withZmatrix/longerRuns/out_38537935_Model2LongerRuns.rds")


#merge runs
m2.m <- createMcmcSamplerList(list(m2.r2, m2.r3, m2.r4, m2.r7,
                                   m2.r8, m2.r10, m2.r11))


quartz()
par(ask=T)
plot(m2.m, parametersOnly=F, start=1500)
MAP(m2.m, start=1500)


# MAP after removing first 1,500 iterations
model2 <- c(gpsp0 = 0.7104566, gpsp1 = 0.05586399, gpsp2 = 0.1885455, gpsploc = 1993.244,
            msmsp0 = 0.1278168, msmsp1 = 0.4484997, msmsp2 = 0.07402333, msmsploc = 1984.945,
            import = 0.03334398, srcNe = 1710.705,
            pmsm2msm = 0.7177013, pgpf2gpm = 0.9950776,
            initmsm = 1.085454, initgp = 31.56574)


#merge runs: longer runs
m2.l.m <- createMcmcSamplerList(list(m2.l.r1, m2.l.r2, m2.l.r3))

plot(m2.l.m, parametersOnly=F, start=3000)
MAP(m2.l.m, start=3000)


# MAP after removing first 3,000 iterations
model2 <- c(gpsp0 = 0.6448513, gpsp1 = 0.06623077, gpsp2 = 0.1529883, gpsploc = 1994.696,
            msmsp0 = 0.06051696, msmsp1 = 0.4059004, msmsp2 = 0.1061849, msmsploc = 1984.972,
            import = 0.02935013, srcNe = 1539.906,
            pmsm2msm = 0.7601392, pgpf2gpm = 0.9937879,
            initmsm = 1.029341, initgp = 38.58127)



# Model 3

m3.r1 <- readRDS("analyses/scripts/Model3_noGPM/Preliminary_results/Model3_noGPM_withZmatrix.rds")
m3.r2 <- readRDS("analyses/scripts/Model3_noGPM/Preliminary_results/withZmatrix/out_38486100.rds")
m3.r3 <- readRDS("analyses/scripts/Model3_noGPM/Preliminary_results/withZmatrix/out_38486101.rds")
m3.r4 <- readRDS("analyses/scripts/Model3_noGPM/Preliminary_results/withZmatrix/out_38486102.rds")
m3.r5 <- readRDS("analyses/scripts/Model3_noGPM/Preliminary_results/withZmatrix/out_38486104.rds")
m3.r6 <- readRDS("analyses/scripts/Model3_noGPM/Preliminary_results/withZmatrix/out_38486105.rds")
m3.r7 <- readRDS("analyses/scripts/Model3_noGPM/Preliminary_results/withZmatrix/out_38486106.rds")
m3.r8 <- readRDS("analyses/scripts/Model3_noGPM/Preliminary_results/withZmatrix/out_38486109.rds")
m3.r9 <- readRDS("analyses/scripts/Model3_noGPM/Preliminary_results/withZmatrix/out_38486111.rds")
m3.r10 <- readRDS("analyses/scripts/Model3_noGPM/Preliminary_results/withZmatrix/out_38486115.rds")
m3.r11 <- readRDS("analyses/scripts/Model3_noGPM/Preliminary_results/withZmatrix/out_38486116.rds")

# merge runs
m3.m <- createMcmcSamplerList(list(m3.r2, m3.r3, m3.r4, m3.r5, m3.r6,
                                   m3.r7, m3.r8, m3.r9, m3.r10, m3.r11))
plot(m3.m, parametersOnly = F, start=800)
MAP(m3.m, start=800)

# MAP after removing first 800 iterations
model3 <- c(gpsp0 = 0.686333, gpsp1 = 0.05296735, gpsp2 = 0.1043768, gpsploc = 1994.59,
            msmsp0 = 0.08042791, msmsp1 = 0.3301369, msmsp2 = 0.06576226, msmsploc = 1983.756,
            import = 0.03033121, srcNe = 1339.857,
            pmsm2msm = 0.8878406, pgpf2gpm = 0.9926982,
            initmsm = 1.541551, initgp = 30.6608)

# Model 4
m4.r1 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/out_38446574.rds")
m4.r2 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/out_38446575.rds")
m4.r3 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/out_38446576.rds")
m4.r4 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/out_38446577.rds")
m4.r5 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/out_38446578.rds")
m4.r6 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/out_38446579.rds")
m4.r7 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/out_38446581.rds")
m4.r8 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/out_38446583.rds")
m4.r9 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/out_38446589.rds")
m4.r10 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/out_38446590.rds")


# Model 4: longer runs
m4.l.r1 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/longerRuns/out_38494853_Model4_2.rds")
m4.l.r2 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/longerRuns/out_38494854_Model4_2.rds")
m4.l.r3 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/longerRuns/out_38494856_Model4_2.rds")
m4.l.r4 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/longerRuns/out_38537697_Model4_2.rds")
m4.l.r5 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/longerRuns/out_38537698_Model4_2.rds")
m4.l.r6 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/longerRuns/out_38537703_Model4_2.rds")
m4.l.r7 <- readRDS("analyses/scripts/Model4/Preliminary_results/withZmatrix/longerRuns/out_38537706_Model4_2.rds")


# merge runs
m4.m <- createMcmcSamplerList(list(m4.r7, m4.r8, m4.r10))
summary(m4.m, start=800)
MAP(m4.m, start=800)

# MAP after removing first 800 iterations
model4 <- c(gpsp0 = 0.6681754, gpsp1 = 0.08804832, gpsp2 = 0.136869, gpsploc = 1997.847,
            msmsp0 = 0.4381736, msmsp1 = 0.1297124, msmsp2 = 0.09002261, msmsploc = 2008.351,
            import = 0.02729108, srcNe = 1765.251,
            pmsm2msm = 0.8277037, pgpf2gpm = 0.9980637,
            initmsm = 1.916651, initgp = 30.74283)

# merge runs: longer runs
m4.l.m <- createMcmcSamplerList(list(m4.l.r1, m4.l.r2, m4.l.r3))
summary(m4.l.m, start=2000)
MAP(m4.l.m, start=2000)

# MAP after removing first 800 iterations
model4 <- c(gpsp0 = 0.7197736, gpsp1 = 0.05697989, gpsp2 = 0.06800106, gpsploc = 1994.672,
            msmsp0 = 0.4378716, msmsp1 = 0.08487827, msmsp2 = 0.1016547, msmsploc = 2006.225,
            import = 0.0301082, srcNe = 1596.815,
            pmsm2msm = 0.8367927, pgpf2gpm = 0.994142,
            initmsm = 1.593987, initgp = 30.89011)

# by subtype: 02_AG: model 1

m02_AG.r1 <- readRDS("analyses/scripts/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38445797.rds")
m02_AG.r2 <- readRDS("analyses/scripts/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38445798.rds")
m02_AG.r3 <- readRDS("analyses/scripts/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38445799.rds")
m02_AG.r4 <- readRDS("analyses/scripts/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38445801.rds")
m02_AG.r5 <- readRDS("analyses/scripts/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38445802.rds")
m02_AG.r6 <- readRDS("analyses/scripts/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38445803.rds")
m02_AG.r7 <- readRDS("analyses/scripts/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38445804.rds")
m02_AG.r8 <- readRDS("analyses/scripts/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38445805.rds")
m02_AG.r9 <- readRDS("analyses/scripts/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38445806.rds")
m02_AG.r10 <- readRDS("analyses/scripts/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38445807.rds")
m02_AG.r11 <- readRDS("analyses/scripts/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38445808.rds")

m02_AG.m <- createMcmcSamplerList(list(m02_AG.r1, m02_AG.r2, m02_AG.r3, m02_AG.r4,
                                       m02_AG.r5, m02_AG.r6, m02_AG.r7, m02_AG.r8,
                                       m02_AG.r9, m02_AG.r10, m02_AG.r11))

# MAP after removing first 800 iter
MAP(m02_AG.m,start=800)

m02_AG_m1 <- c(gpsp0 = 0.9429633, gpsp1 = 0.06733696, gpsp2 = 0.05467714, gpsploc = 2002.191,
            msmsp0 = 0.06815399, msmsp1 = 0.4220679, msmsp2 = 0.09123317, msmsploc = 1999.867,
            import = 0.08035438, srcNe = 658.9549,
            pmsm2msm = 0.8968579, pgpf2gpm = 0.9999798,
            initmsm = 1.148144, initgp = 30.4639)

# by subtype: 02_AG: model 2
m2.m02_AG.r1 <- readRDS("analyses/scripts/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38494608_02_AG_m2.rds")
m2.m02_AG.r2 <- readRDS("analyses/scripts/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38494609_02_AG_m2.rds")
m2.m02_AG.r3 <- readRDS("analyses/scripts/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38494611_02_AG_m2.rds")
m2.m02_AG.r4 <- readRDS("analyses/scripts/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38494612_02_AG_m2.rds")
m2.m02_AG.r5 <- readRDS("analyses/scripts/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38494613_02_AG_m2.rds")
m2.m02_AG.r6 <- readRDS("analyses/scripts/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38494614_02_AG_m2.rds")
m2.m02_AG.r7 <- readRDS("analyses/scripts/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38494615_02_AG_m2.rds")
m2.m02_AG.r8 <- readRDS("analyses/scripts/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38494619_02_AG_m2.rds")

# merge runs
m02_AG.m2 <- createMcmcSamplerList(list(m2.m02_AG.r1, m2.m02_AG.r2, m2.m02_AG.r3,
                                        m2.m02_AG.r4, m2.m02_AG.r5, m2.m02_AG.r6,
                                        m2.m02_AG.r7, m2.m02_AG.r8))
# MAP after removing first 800 iter
MAP(m02_AG.m2,start=800)

m02_AG_m2 <- c(gpsp0 = 0.9292274, gpsp1 = 0.05659603, gpsp2 = 0.1042886, gpsploc = 2003.035,
               msmsp0 = 0.0778936, msmsp1 = 0.3119664, msmsp2 = 0.08045854, msmsploc = 2000.702,
               import = 0.07747372, srcNe = 688.8138,
               pmsm2msm = 0.8259524, pgpf2gpm = 0.9999288,
               initmsm = 1.498243, initgp = 28.91686)

# Subtype C: model 1
mC.r1 <- readRDS("analyses/scripts/bySubtype/C_m1/Preliminary_results/withzMatrix/out_38449357.rds")
mC.r2 <- readRDS("analyses/scripts/bySubtype/C_m1/Preliminary_results/withzMatrix/out_38449358.rds")
mC.r3 <- readRDS("analyses/scripts/bySubtype/C_m1/Preliminary_results/withzMatrix/out_38449359.rds")
mC.r4 <- readRDS("analyses/scripts/bySubtype/C_m1/Preliminary_results/withzMatrix/out_38449360.rds")
mC.r5 <- readRDS("analyses/scripts/bySubtype/C_m1/Preliminary_results/withzMatrix/out_38449361.rds")
mC.r6 <- readRDS("analyses/scripts/bySubtype/C_m1/Preliminary_results/withzMatrix/out_38449367.rds")
mC.r7 <- readRDS("analyses/scripts/bySubtype/C_m1/Preliminary_results/withzMatrix/out_38449368.rds")
mC.r8 <- readRDS("analyses/scripts/bySubtype/C_m1/Preliminary_results/withzMatrix/out_38449369.rds")
mC.r9 <- readRDS("analyses/scripts/bySubtype/C_m1/Preliminary_results/withzMatrix/out_38449370.rds")
mC.r10 <- readRDS("analyses/scripts/bySubtype/C_m1/Preliminary_results/withzMatrix/out_38449371.rds")
mC.r11 <- readRDS("analyses/scripts/bySubtype/C_m1/Preliminary_results/withzMatrix/out_38449377.rds")
mC.r12 <- readRDS("analyses/scripts/bySubtype/C_m1/Preliminary_results/withzMatrix/out_38449378.rds")

# merge runs
mC.m <- createMcmcSamplerList(list(mC.r1, mC.r2, mC.r3, mC.r4, mC.r5,
                                   mC.r6, mC.r7, mC.r8, mC.r9, mC.r10,
                                   mC.r11, mC.r12))

# MAP after removing 800 iterations
MAP(mC.m, start=800)

mC_m1 <- c(gpsp0 = 0.3312109, gpsp1 = 0.1047985, gpsp2 = 0.1856188, gpsploc = 2001.893,
            msmsp0 = 0.06279285, msmsp1 = 0.3889561, msmsp2 = 0.05299903, msmsploc = 1983.111,
            import = 0.01734584, srcNe = 152.4555,
            pmsm2msm = 0.8652, pgpf2gpm = 0.929348,
            initmsm = 1.116821, initgp = 4.901793)

# Subtype C: model 2
m2.mC.r1 <- readRDS("analyses/scripts/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38495292_C_m2.rds")
m2.mC.r2 <- readRDS("analyses/scripts/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38495293_C_m2.rds")
m2.mC.r3 <- readRDS("analyses/scripts/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38495294_C_m2.rds")
m2.mC.r4 <- readRDS("analyses/scripts/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38495295_C_m2.rds")
m2.mC.r5 <- readRDS("analyses/scripts/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38495296_C_m2.rds")
m2.mC.r6 <- readRDS("analyses/scripts/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38495297_C_m2.rds")
m2.mC.r7 <- readRDS("analyses/scripts/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38495298_C_m2.rds")
m2.mC.r8 <- readRDS("analyses/scripts/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38495299_C_m2.rds")
m2.mC.r9 <- readRDS("analyses/scripts/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38495300_C_m2.rds")
m2.mC.r10 <- readRDS("analyses/scripts/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38495301_C_m2.rds")
m2.mC.r11 <- readRDS("analyses/scripts/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38495302_C_m2.rds")
m2.mC.r12 <- readRDS("analyses/scripts/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38495303_C_m2.rds")

#merge runs
m2.mC.m <- createMcmcSamplerList(list(m2.mC.r1, m2.mC.r2, m2.mC.r3, m2.mC.r4,
                                      m2.mC.r5, m2.mC.r6, m2.mC.r7, m2.mC.r8,
                                      m2.mC.r9, m2.mC.r10, m2.mC.r11, m2.mC.r12))
#MAP for model 2 subtype C
MAP(m2.mC.m, start=800)

mC_m2 <- c(gpsp0 = 0.3936318, gpsp1 = 0.08124474, gpsp2 = 0.1578281, gpsploc = 2002.389,
           msmsp0 = 0.06337793, msmsp1 = 0.3387542, msmsp2 = 0.0706717, msmsploc = 1987.436,
           import = 0.01915205, srcNe = 165.741,
           pmsm2msm = 0.9218892, pgpf2gpm = 0.8959267,
           initmsm = 1.503166, initgp = 3.537323)

# save merged runs
save(m2.l.m, m3.m, m4.l.m, m02_AG.m, m02_AG.m2, mC.m, m2.mC.m, file="mergedRuns_newModel.rda")

# create dataframe to make plots
MAP_models <- data.frame(model2, model3, model4, m02_AG_m1, m02_AG_m2, mC_m1, mC_m2)
#MAP_models["parameter"] <- rownames(MAP_models)
library(reshape2)
MAP_models_t <- data.frame(t(MAP_models))
MAP_models_t["model"] <- rownames(MAP_models_t)

MAP_models_long <- melt(MAP_models_t, id.vars="model")


#plots
gpsp_values <- MAP_models_long[c(1:21),]
gpsp_values$model <- as.factor(gpsp_values$model)

ggplot(data=gpsp_values, aes(x=variable, y=value, group=model, shape=model, colour=model)) +
  geom_line(aes(linetype=model), size=0.5) +     # Set linetype by sex
  geom_point(size=2, fill="white") +         # Use larger points, fill with white
  expand_limits(y=0) +                       # Set y range to include 0
  scale_colour_hue(name="Model",      # Set legend title
                   l=30)  +                  # Use darker colors (lightness=30)
  scale_shape_manual(name="Model",
                     values=c(2, 18, 5, 1, 16, 0, 15)) +      # Use points with a fill color
  scale_linetype_discrete(name="Model") +
  xlab("parameter") + ylab("value") + # Set axis labels
  theme_bw() +
  theme(legend.position=c(.85, .8))           # Position legend inside
# This must go after theme_bw


msmsp_values <- MAP_models_long[c(29:49),]
msmsp_values$model <- as.factor(msmsp_values$model)

ggplot(data=msmsp_values, aes(x=variable, y=value, group=model, shape=model, colour=model)) +
  geom_line(aes(linetype=model), size=1) +     # Set linetype by sex
  geom_point(size=3, fill="white") +         # Use larger points, fill with white
  expand_limits(y=0) +                       # Set y range to include 0
  scale_colour_hue(name="Model",      # Set legend title
                   l=30)  +                  # Use darker colors (lightness=30)
  scale_shape_manual(name="Model",
                     values=c(2, 18, 5, 1, 16, 0, 15)) +      # Use points with a fill color
  scale_linetype_discrete(name="Model") +
  xlab("parameter") + ylab("value") + # Set axis labels
  theme_bw() +
  theme(legend.position=c(.85, .8))           # Position legend inside
# This must go after theme_bw


# other values
other_values <- MAP_models_long[c(16:20, 36:70),]
other_values$model <- as.factor(other_values$model)


ggplot(data=other_values, aes(x=variable, y=value, group=model, shape=model, colour=model)) +
  geom_point(size=3, fill="white") +         # Use larger points, fill with white
  expand_limits(y=0) +                       # Set y range to include 0
  scale_colour_hue(name="Model",      # Set legend title
                   l=30)  +                  # Use darker colors (lightness=30)
  xlab("parameter") + ylab("value") + # Set axis labels
  theme_bw() +
  theme(legend.position=c(.85, .8))           # Position legend inside
# This must go after theme_bw



########################################################
