library(BayesianTools)



load(system.file("data/mcmc_runs/mergedRuns_newModels_maleX_rd2.rda", package = "senegalHIVmodel"))

m02_AG.m1.s <- getSample(m02_AG.mx.m_rd2, start=1300)
m02_AG.m1_map <- MAP(m02_AG.mx.m_rd2, start=1300)$parametersMAP

m02_AGm1_q <- quantile(m02_AG.m1.s[,12], probs=c(0.025, 0.975))
m02_AGm1_median <- median(m02_AG.m1.s[,12])


mC.m1.s <- getSample(mC.mx.m_rd2, start=1000)
mC.m1_map <- MAP(mC.mx.m_rd2, start=1000)$parametersMAP

mCm1_q <- quantile(mC.m1.s[,12], probs=c(0.025, 0.975))
mCm1_median <- median(mC.m1.s[,12])
