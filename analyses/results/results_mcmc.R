# Summary of the MCMC runs
# Total of 14 parameters estimated


###########################################################################
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

# MAP after removing first 1,500 iterations
model2_map <- MAP(m2.m, start = 1500)
model2 <- model2_map$parametersMAP
names(model2) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                    "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                    "import", "srcNe",
                    "pmsm2msm", "pgpf2gpm",
                    "initmsm", "initgp"))



#merge runs: longer runs
m2.l.m <- createMcmcSamplerList(list(m2.l.r1, m2.l.r2, m2.l.r3))

# MAP adter removinf first 3,000 iterations
model2_map.l <- MAP(m2.l.m, start = 3000)
model2.l <- model2_map.l$parametersMAP
names(model2.l) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                      "import", "srcNe",
                      "pmsm2msm", "pgpf2gpm",
                      "initmsm", "initgp"))


###########################################################################
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

# MAP after removing first 800 iterations
model3_map <- MAP(m3.m, start = 800)
model3 <- model3_map$parametersMAP
names(model3) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                    "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                    "import", "srcNe",
                    "pmsm2msm", "pgpf2gpm",
                    "initmsm", "initgp"))



###########################################################################
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


# MAP after removing first 800 iterations
model4_map <- MAP(m4.m, start = 800)
model4 <- model4_map$parametersMAP
names(model4) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                    "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                    "import", "srcNe",
                    "pmsm2msm", "pgpf2gpm",
                    "initmsm", "initgp"))


# merge runs: longer runs
m4.l.m <- createMcmcSamplerList(list(m4.l.r1, m4.l.r2, m4.l.r3))


# MAP after removing first 2,000 iterations
model4_map.l <- MAP(m4.l.m, start = 2000)
model4.l <- model4_map.l$parametersMAP
names(model4.l) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                      "import", "srcNe",
                      "pmsm2msm", "pgpf2gpm",
                      "initmsm", "initgp"))



###########################################################################
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

# MAP after removing first 800 iterations
m02_AG_map <- MAP(m02_AG.m, start = 800)
m02_AG_m1 <- m02_AG_map$parametersMAP
names(m02_AG_m1) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                       "import", "srcNe",
                       "pmsm2msm", "pgpf2gpm",
                       "initmsm", "initgp"))



###########################################################################
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
# MAP after removing first 800 iterations
m02_AG_map_m2 <- MAP(m02_AG.m2, start = 800)
m02_AG_m2 <- m02_AG_map_m2$parametersMAP
names(m02_AG_m2) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                       "import", "srcNe",
                       "pmsm2msm", "pgpf2gpm",
                       "initmsm", "initgp"))



###########################################################################
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
mC_map <- MAP(mC.m, start = 800)
mC_m1 <- mC_map$parametersMAP
names(mC_m1) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                   "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                   "import", "srcNe",
                   "pmsm2msm", "pgpf2gpm",
                   "initmsm", "initgp"))



###########################################################################
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
mC_map_m1 <- MAP(m2.mC.m, start = 800)
mC_m2 <- mC_map_m1$parametersMAP
names(mC_m2) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                   "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                   "import", "srcNe",
                   "pmsm2msm", "pgpf2gpm",
                   "initmsm", "initgp"))

# save merged runs
save(m2.l.m, m3.m, m4.l.m, m02_AG.m, m02_AG.m2, mC.m, m2.mC.m, file="mergedRuns_newModel.rda")
