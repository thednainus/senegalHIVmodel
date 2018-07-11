# Summary of MCMC runs when also estimating maleX parameter
# Total of 15 parameters estimated

# For these analysis I have initially added an upper bound for the estimation
# of maleX to 2. However, posterior distribution was around 2.
# For second round of runs, I increased the upper bound for maleX. This will be
# eventually the set of runs I will leave in this script

library(BayesianTools)

###########################################################################
#Model2
m2.r1.mx <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38577460_Model2_maleX.rds")
m2.r2.mx <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38577461_Model2_maleX.rds")
m2.r3.mx <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38577462_Model2_maleX.rds")
m2.r4.mx <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38577464_Model2_maleX.rds")
m2.r5.mx <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38577466_Model2_maleX.rds")
m2.r6.mx <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38577467_Model2_maleX.rds")
m2.r7.mx <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38577468_Model2_maleX.rds")

#Model2: increasing upper limit for maleX
m2.mx.r1 <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38665455_Model2_maleX.rds")
m2.mx.r2 <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38665457_Model2_maleX.rds")
m2.mx.r3 <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38665461_Model2_maleX.rds")
m2.mx.r4 <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38665463_Model2_maleX.rds")
m2.mx.r5 <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38665468_Model2_maleX.rds")
m2.mx.r6 <- readRDS("analyses/scripts/MaleX/Model2/Preliminary_results/withZmatrix/out_38665453_Model2_maleX.rds")


#merge runs first round of runs and MAP
m2.mx.m <- createMcmcSamplerList(list(m2.r1.mx, m2.r2.mx, m2.r3.mx, m2.r4.mx,
                                      m2.r5.mx, m2.r6.mx, m2.r7.mx))

plot(m2.mx.m, parametersOnly = F, start = 1500)

model2_map <- MAP(m2.mx.m, start = 1500)
model2.mx <- model2_map$parametersMAP
names(model2.mx) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                       "maleX", "import", "srcNe",
                       "pmsm2msm", "pgpf2gpm",
                       "initmsm", "initgp"))


#merge runs: second round of runs and MAP
m2.mx.m_rd2 <- createMcmcSamplerList(list(m2.mx.r1, m2.mx.r2, m2.mx.r3, m2.mx.r4,
                                          m2.mx.r5, m2.mx.r6))

plot(m2.mx.m_rd2, parametersOnly = F, start = 1000)

model2_rd2_map <- MAP(m2.mx.m_rd2, start = 1000)
model2_rd2.mx <- model2_rd2_map$parametersMAP
names(model2_rd2.mx) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                       "maleX", "import", "srcNe",
                       "pmsm2msm", "pgpf2gpm",
                       "initmsm", "initgp"))


###########################################################################
# Model 3
m3.r1.mx <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38578203_Model3_maleX.rds")
m3.r2.mx <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38578206_Model3_maleX.rds")
m3.r3.mx <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38578207_Model3_maleX.rds")
m3.r4.mx <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38578211_Model3_maleX.rds")
m3.r5.mx <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38578212_Model3_maleX.rds")
m3.r6.mx <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38578214_Model3_maleX.rds")
m3.r7.mx <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38578215_Model3_maleX.rds")
m3.r8.mx <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38578217_Model3_maleX.rds")

#Model3: increasing upper limit for maleX
m3.mx.r1 <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38665471_Model3_maleX.rds")
m3.mx.r2 <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38665473_Model3_maleX.rds")
m3.mx.r3 <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38665475_Model3_maleX.rds")
m3.mx.r4 <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38665477_Model3_maleX.rds")
m3.mx.r5 <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38665478_Model3_maleX.rds")
m3.mx.r6 <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38665479_Model3_maleX.rds")
m3.mx.r7 <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38665480_Model3_maleX.rds")
m3.mx.r8 <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38665481_Model3_maleX.rds")
m3.mx.r9 <- readRDS("analyses/scripts/MaleX/Model3/Preliminary_results/withZmatrix/out_38665483_Model3_maleX.rds")

#merge runs: first round
m3.mx.m <- createMcmcSamplerList(list(m3.r2.mx, m3.r3.mx, m3.r4.mx, m3.r5.mx,
                                      m3.r6.mx, m3.r7.mx, m3.r8.mx))

plot(m3.mx.m, start=1500, parametersOnly = F)

model3_map <- MAP(m3.mx.m, start = 1500)
model3.mx <- model3_map$parametersMAP
names(model3.mx) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                       "maleX", "import", "srcNe",
                       "pmsm2msm", "pgpf2gpm",
                       "initmsm", "initgp"))

#merge runs: second round
m3.mx.m_rd2 <- createMcmcSamplerList(list(m3.mx.r1, m3.mx.r2, m3.mx.r3, m3.mx.r4,
                                          m3.mx.r5, m3.mx.r6, m3.mx.r7, m3.mx.r8,
                                          m3.mx.r9))

plot(m3.mx.m_rd2, start=1000, parametersOnly = F)

model3_rd2_map <- MAP(m3.mx.m_rd2, start = 1000)
model3_rd2.mx <- model3_rd2_map$parametersMAP
names(model3_rd2.mx) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                       "maleX", "import", "srcNe",
                       "pmsm2msm", "pgpf2gpm",
                       "initmsm", "initgp"))


###########################################################################
#Model 4

m4.r1.mx <- readRDS("analyses/scripts/MaleX/Model4/Preliminary_results/withZmatrix/out_38578371_Model4_maleX.rds")
m4.r2.mx <- readRDS("analyses/scripts/MaleX/Model4/Preliminary_results/withZmatrix/out_38578373_Model4_maleX.rds")
m4.r3.mx <- readRDS("analyses/scripts/MaleX/Model4/Preliminary_results/withZmatrix/out_38578374_Model4_maleX.rds")
m4.r4.mx <- readRDS("analyses/scripts/MaleX/Model4/Preliminary_results/withZmatrix/out_38578375_Model4_maleX.rds")
m4.r5.mx <- readRDS("analyses/scripts/MaleX/Model4/Preliminary_results/withZmatrix/out_38578376_Model4_maleX.rds")
m4.r6.mx <- readRDS("analyses/scripts/MaleX/Model4/Preliminary_results/withZmatrix/out_38578380_Model4_maleX.rds")

#Model3: increasing upper limit for maleX
m4.mx.r1 <- readRDS("analyses/scripts/MaleX/Model4/Preliminary_results/withZmatrix/out_38665430_Model4_maleX.rds")
m4.mx.r2 <- readRDS("analyses/scripts/MaleX/Model4/Preliminary_results/withZmatrix/out_38665432_Model4_maleX.rds")
m4.mx.r3 <- readRDS("analyses/scripts/MaleX/Model4/Preliminary_results/withZmatrix/out_38665434_Model4_maleX.rds")
m4.mx.r4 <- readRDS("analyses/scripts/MaleX/Model4/Preliminary_results/withZmatrix/out_38665429_Model4_maleX.rds")
m4.mx.r5 <- readRDS("analyses/scripts/MaleX/Model4/Preliminary_results/withZmatrix/out_38665440_Model4_maleX.rds")


#merge runs: first round
m4.mx.m <- createMcmcSamplerList(list(m4.r1.mx, m4.r2.mx, m4.r4.mx, m4.r5.mx,
                                      m4.r6.mx))

plot(m4.mx.m, parametersOnly = F, start=1500)
model4_map <- MAP(m4.mx.m, start = 1500)
model4.mx <- model4_map$parametersMAP
names(model4.mx) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                       "maleX", "import", "srcNe",
                       "pmsm2msm", "pgpf2gpm",
                       "initmsm", "initgp"))


#merge runs: second round
m4.mx.m_rd2 <- createMcmcSamplerList(list(m4.mx.r1, m4.mx.r2, m4.mx.r3, m4.mx.r4,
                                          m4.mx.r5))

plot(m4.mx.m_rd2, parametersOnly = F, start=1000)
model4_rd2_map <- MAP(m4.mx.m_rd2, start = 1000)
model4_rd2.mx <- model4_rd2_map$parametersMAP
names(model4_rd2.mx) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                       "maleX", "import", "srcNe",
                       "pmsm2msm", "pgpf2gpm",
                       "initmsm", "initgp"))


###########################################################################
# by subtype: 02_AG model 1

m02_AG.r1.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38578491_02_AG_m1_maleX.rds")
m02_AG.r2.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38578492_02_AG_m1_maleX.rds")
m02_AG.r3.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38578494_02_AG_m1_maleX.rds")
m02_AG.r4.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38578495_02_AG_m1_maleX.rds")
m02_AG.r5.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38578503_02_AG_m1_maleX.rds")
m02_AG.r6.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38578504_02_AG_m1_maleX.rds")
m02_AG.r7.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38578505_02_AG_m1_maleX.rds")
m02_AG.r8.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38578506_02_AG_m1_maleX.rds")

#by subtype: 02_AG model 1: increasing upper limit for maleX
m02_AG.mx.r1 <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38665487_02_AG_m1_maleX.rds")
m02_AG.mx.r2 <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38665489_02_AG_m1_maleX.rds")
m02_AG.mx.r3 <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38665490_02_AG_m1_maleX.rds")
m02_AG.mx.r4 <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38665491_02_AG_m1_maleX.rds")
m02_AG.mx.r5 <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38665493_02_AG_m1_maleX.rds")
m02_AG.mx.r6 <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38665494_02_AG_m1_maleX.rds")
m02_AG.mx.r7 <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38665495_02_AG_m1_maleX.rds")
m02_AG.mx.r8 <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m1/Preliminary_results/withZmatrix/out_38665497_02_AG_m1_maleX.rds")



#merge runs
m02_AG.mx.m <- createMcmcSamplerList(list(m02_AG.r1.mx, m02_AG.r2.mx, m02_AG.r3.mx, m02_AG.r4.mx,
                                          m02_AG.r5.mx, m02_AG.r6.mx, m02_AG.r7.mx, m02_AG.r8.mx))

plot(m02_AG.mx.m, parametersOnly=F, start=1500)

m02_AG_m1_map <- MAP(m02_AG.mx.m, start = 1500)
m02_AG_m1.mx <- m02_AG_m1_map$parametersMAP
names(m02_AG_m1.mx) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                       "maleX", "import", "srcNe",
                       "pmsm2msm", "pgpf2gpm",
                       "initmsm", "initgp"))

#merge runs: second round
m02_AG.mx.m_rd2 <- createMcmcSamplerList(list(m02_AG.mx.r1, m02_AG.mx.r2, m02_AG.mx.r3,
                                              m02_AG.mx.r4, m02_AG.mx.r5, m02_AG.mx.r6
                                              ))

plot(m02_AG.mx.m_rd2, parametersOnly=F, start=1300)

m02_AG_m1_rd2_map <- MAP(m02_AG.mx.m_rd2, start = 1300)
m02_AG_m1_rd2.mx <- m02_AG_m1_rd2_map$parametersMAP
names(m02_AG_m1_rd2.mx) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                          "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                          "maleX", "import", "srcNe",
                          "pmsm2msm", "pgpf2gpm",
                          "initmsm", "initgp"))


###########################################################################
# by subtype: 02_AG model 2

m2.m02_AG.r1.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38578647_02_AG_m2_maleX.rds")
m2.m02_AG.r2.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38578648_02_AG_m2_maleX.rds")
m2.m02_AG.r3.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38578649_02_AG_m2_maleX.rds")
m2.m02_AG.r4.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38578650_02_AG_m2_maleX.rds")
m2.m02_AG.r5.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38578651_02_AG_m2_maleX.rds")
m2.m02_AG.r6.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38578657_02_AG_m2_maleX.rds")
m2.m02_AG.r7.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38578658_02_AG_m2_maleX.rds")
m2.m02_AG.r8.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38578659_02_AG_m2_maleX.rds")
m2.m02_AG.r9.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38578660_02_AG_m2_maleX.rds")
m2.m02_AG.r10.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38578661_02_AG_m2_maleX.rds")
m2.m02_AG.r11.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38578662_02_AG_m2_maleX.rds")
m2.m02_AG.r12.mx <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38578663_02_AG_m2_maleX.rds")

#by subtype: 02_AG model 2: increasing upper limit for maleX
m2.m02_AG.mx.r1 <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38665501_02_AG_m2_maleX.rds")
m2.m02_AG.mx.r2 <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38665502_02_AG_m2_maleX.rds")
m2.m02_AG.mx.r3 <- readRDS("analyses/scripts/MaleX/bySubtype/02_AG_m2/Preliminary_results/withZmatrix/out_38665505_02_AG_m2_maleX.rds")


#merge runs:round 1
m2.m02_AG.mx.m <- createMcmcSamplerList(list(m2.m02_AG.r1.mx,m2.m02_AG.r2.mx,
                                             m2.m02_AG.r3.mx,m2.m02_AG.r4.mx,
                                             m2.m02_AG.r5.mx,m2.m02_AG.r6.mx,
                                             m2.m02_AG.r7.mx,m2.m02_AG.r8.mx,
                                             m2.m02_AG.r9.mx,m2.m02_AG.r10.mx,
                                             m2.m02_AG.r11.mx,m2.m02_AG.r12.mx))

plot(m2.m02_AG.mx.m, parametersOnly=F, start=2000)

m2.m02_AG_m1_map <- MAP(m2.m02_AG.mx.m, start = 1500)
m2.m02_AG_m1.mx <- m2.m02_AG_m1_map$parametersMAP
names(m2.m02_AG_m1.mx) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                          "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                          "maleX", "import", "srcNe",
                          "pmsm2msm", "pgpf2gpm",
                          "initmsm", "initgp"))

#merge runs: round 2
m2.m02_AG.mx.m_rd2 <- createMcmcSamplerList(list(m2.m02_AG.mx.r1, m2.m02_AG.mx.r2,
                                                 m2.m02_AG.mx.r3))

plot(m2.m02_AG.mx.m_rd2, parametersOnly=F, start=1500)

m2.m02_AG_m1_rd2_map <- MAP(m2.m02_AG.mx.m_rd2, start = 1500)
m2.m02_AG_m1_rd2.mx <- m2.m02_AG_m1_rd2_map$parametersMAP
names(m2.m02_AG_m1_rd2.mx) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                             "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                             "maleX", "import", "srcNe",
                             "pmsm2msm", "pgpf2gpm",
                             "initmsm", "initgp"))



###########################################################################
#Subtype C: model 1: round 1

mC.r1.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38578724_C_m1_maleX.rds")
mC.r2.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38578725_C_m1_maleX.rds")
mC.r3.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38578726_C_m1_maleX.rds")
mC.r4.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38578727_C_m1_maleX.rds")
mC.r5.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38578728_C_m1_maleX.rds")
mC.r6.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38578730_C_m1_maleX.rds")
mC.r7.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38578731_C_m1_maleX.rds")
mC.r8.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38578732_C_m1_maleX.rds")
mC.r9.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38578733_C_m1_maleX.rds")
mC.r10.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38578739_C_m1_maleX.rds")
mC.r11.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38578740_C_m1_maleX.rds")

#Subtype C: model 1: round 2

mC.mx.r1 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38665513_C_m1_maleX.rds")
mC.mx.r2 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38665514_C_m1_maleX.rds")
mC.mx.r4 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38665516_C_m1_maleX.rds")
mC.mx.r5 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38665517_C_m1_maleX.rds")
mC.mx.r6 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38665518_C_m1_maleX.rds")
mC.mx.r7 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38665519_C_m1_maleX.rds")
mC.mx.r8 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38665520_C_m1_maleX.rds")
mC.mx.r9 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38665521_C_m1_maleX.rds")
mC.mx.r10 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38665523_C_m1_maleX.rds")
mC.mx.r11 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m1/Preliminary_results/withZmatrix/out_38665524_C_m1_maleX.rds")


#merge runs
mC.mx.m_rd2 <- createMcmcSamplerList(list(mC.mx.r1, mC.mx.r2, mC.mx.r4,
                                          mC.mx.r5, mC.mx.r6, mC.mx.r7, mC.mx.r8,
                                          mC.mx.r9, mC.mx.r10, mC.mx.r11))

plot(mC.mx.m_rd2, parametersOnly=F, start=1000)

mC_m1_rd2_map <- MAP(mC.mx.m_rd2, start = 1000)
mC_m1_map_rd2.mx <- mC_m1_rd2_map$parametersMAP
names(mC_m1_map_rd2.mx) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                             "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                             "maleX", "import", "srcNe",
                             "pmsm2msm", "pgpf2gpm",
                             "initmsm", "initgp"))



###########################################################################
# Subtype C: model 2:round 1

m2.mC.r1.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38578859_C_m2_maleX.rds")
m2.mC.r2.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38578860_C_m2_maleX.rds")
m2.mC.r3.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38578861_C_m2_maleX.rds")
m2.mC.r4.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38578862_C_m2_maleX.rds")
m2.mC.r5.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38578863_C_m2_maleX.rds")
m2.mC.r6.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38578865_C_m2_maleX.rds")
m2.mC.r7.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38578866_C_m2_maleX.rds")
m2.mC.r8.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38578867_C_m2_maleX.rds")
m2.mC.r9.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38578868_C_m2_maleX.rds")
m2.mC.r10.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38578869_C_m2_maleX.rds")
m2.mC.r11.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38578870_C_m2_maleX.rds")
m2.mC.r12.mx <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38578871_C_m2_maleX.rds")

# Subtype C: model 2:round 2

m2.mC.mx.r1 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38665528_C_m2_maleX.rds")
m2.mC.mx.r2 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38665530_C_m2_maleX.rds")
m2.mC.mx.r3 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38665531_C_m2_maleX.rds")
m2.mC.mx.r4 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38665532_C_m2_maleX.rds")
m2.mC.mx.r5 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38665533_C_m2_maleX.rds")
m2.mC.mx.r6 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38665534_C_m2_maleX.rds")
m2.mC.mx.r7 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38665535_C_m2_maleX.rds")
m2.mC.mx.r8 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38665536_C_m2_maleX.rds")
m2.mC.mx.r9 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38665537_C_m2_maleX.rds")
m2.mC.mx.r10 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38665538_C_m2_maleX.rds")
m2.mC.mx.r11 <- readRDS("analyses/scripts/MaleX/bySubtype/C_m2/Preliminary_results/withZmatrix/out_38665539_C_m2_maleX.rds")




#merge runs:round 1
m2.mC.mx.m <- createMcmcSamplerList(list(m2.mC.r1.mx, m2.mC.r2.mx, m2.mC.r3.mx,
                                         m2.mC.r4.mx, m2.mC.r5.mx, m2.mC.r7.mx,
                                         m2.mC.r8.mx, m2.mC.r10.mx, m2.mC.r11.mx,
                                         m2.mC.r12.mx))

plot(m2.mC.mx.m, parametersOnly=F, start=1000)

m2.mC_m1_map <- MAP(m2.mC.mx.m, start = 1000)
m2.mC_m1_map.mx <- m2.mC_m1_map$parametersMAP
names(m2.mC_m1_map.mx) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                          "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                          "maleX", "import", "srcNe",
                          "pmsm2msm", "pgpf2gpm",
                          "initmsm", "initgp"))

#merge runs: round 2
m2.mC.mx.m_rd2 <- createMcmcSamplerList(list(m2.mC.mx.r1, m2.mC.mx.r2, m2.mC.mx.r3,
                                             m2.mC.mx.r4, m2.mC.mx.r5, m2.mC.mx.r6,
                                             m2.mC.mx.r7, m2.mC.mx.r8, m2.mC.mx.r9,
                                             m2.mC.mx.r10, m2.mC.mx.r11))

plot(m2.mC.mx.m_rd2, parametersOnly=F, start=1000)

m2.mC_m1_rd2_map <- MAP(m2.mC.mx.m_rd2, start = 1000)
m2.mC_m1_map.mx_rd2 <- m2.mC_m1_rd2_map$parametersMAP
names(m2.mC_m1_map.mx_rd2) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                             "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                             "maleX", "import", "srcNe",
                             "pmsm2msm", "pgpf2gpm",
                             "initmsm", "initgp"))



save(m2.mx.m, m3.mx.m, m4.mx.m, mC.mx.m, m2.mC.mx.m, m02_AG.mx.m, m2.m02_AG.mx.m, file="mergedRuns_newModels_maleX.rda")
save(m2.mx.m_rd2, m3.mx.m_rd2, m4.mx.m_rd2, mC.mx.m_rd2, m2.mC.mx.m_rd2, m02_AG.mx.m_rd2, m2.m02_AG.mx.m_rd2, file="mergedRuns_newModels_maleX_rd2.rda")

