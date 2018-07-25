library(phydynR)
library(senegalHIVmodel)
library(ggplot2)
library(reshape2)


load(system.file("data/mcmc_runs/new_mcmc_results.rda",
                 package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/mergedRuns_newModels_maleX_rd2.rda",
                 package = "senegalHIVmodel"))

# These are the values used for the simulations
# Initial time for simulations
T0 <- 1978
# Final time for simulations
T1 <- 2014
# Duration of infection. In our model we assumed 1 stage of infection
GAMMA <- 1/10

gpspline = function( t, parms ){
  if (t < T0 ) return( parms$gpsp0 )
  if (t > T1) return (parms$gpsp2)
  with(parms, pmax(0.025, approx( x = c(T0, gpsploc, T1), y=c(gpsp0, gpsp1, gpsp2), xout = t, rule = 2)$y) )
}
msmspline  = function( t, parms){
  if (t < T0 ) return( parms$msmsp0 )
  if (t > T1) return ( parms$msmsp2 )
  with(parms, pmax(0.025, approx( x = c(T0, msmsploc, T1), y=c(msmsp0, msmsp1, msmsp2), xout = t, rule = 2)$y) )
}

times <- seq(1978, 2014, length.out = 1000)


#Model 2
model2.mx <- BayesianTools::MAP(m2, start=1000)
model2.mx.p <- as.numeric(format(round(model2.mx$parametersMAP, 3), nsmall = 3))
names(model2.mx.p) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                         "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                         "maleX", "import", "srcNe",
                         "pmsm2msm", "pgpf2gpm",
                         "initmsm", "initgp"))

m2.gp <- sapply(times,gpspline,parms=as.list(model2.mx.p[1:4]))
m2.msm <- sapply(times,msmspline,parms=as.list(model2.mx.p[5:8]))

m2.sp <- as.data.frame(cbind(times, m2.gp=m2.gp/GAMMA, m2.msm=m2.msm/GAMMA))
m2.sp.l <- melt(m2.sp, id.vars="times")




#Model 5
# MAP after removing first 1,200 iterations
model5.mx <- BayesianTools::MAP(m5, start=1200)
model5.mx.p <- as.numeric(format(round(model5.mx$parametersMAP, 3), nsmall = 3))
names(model5.mx.p) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                         "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                         "maleX", "import", "srcNe",
                         "pmsm2msm", "pgpf2gpm",
                         "initmsm", "initgp"))

m5.gp <- sapply(times,gpspline,parms=as.list(model5.mx.p[1:4]))
m5.msm <- sapply(times,msmspline,parms=as.list(model5.mx.p[5:8]))

m5.sp <- as.data.frame(cbind(times, m5.gp=m5.gp/GAMMA, m5.msm=m5.msm/GAMMA))
m5.sp.l <- melt(m5.sp, id.vars="times")

#Model 4
model4.mx <- BayesianTools::MAP(m4, start=1200)
model4.mx.p <- as.numeric(format(round(model4.mx$parametersMAP, 3), nsmall = 3))
names(model4.mx.p) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                         "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                         "maleX", "import", "srcNe",
                         "pmsm2msm", "pgpf2gpm",
                         "initmsm", "initgp"))

m4.gp <- sapply(times,gpspline,parms=as.list(model4.mx.p[1:4]))
m4.msm <- sapply(times,msmspline,parms=as.list(model4.mx.p[5:8]))

m4.sp <- as.data.frame(cbind(times, m4.gp=m4.gp/GAMMA, m4.msm=m4.msm/GAMMA))
m4.sp.l <- melt(m4.sp, id.vars="times")

# Model 6
model6.mx <- BayesianTools::MAP(m6, start=1200)
model6.mx.p <- as.numeric(format(round(model6.mx$parametersMAP, 3), nsmall = 3))
names(model6.mx.p) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                         "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                         "maleX", "import", "srcNe",
                         "pmsm2msm", "pgpf2gpm",
                         "initmsm", "initgp"))

m6.gp <- sapply(times,gpspline,parms=as.list(model6.mx.p[1:4]))
m6.msm <- sapply(times,msmspline,parms=as.list(model6.mx.p[5:8]))

m6.sp <- as.data.frame(cbind(times, m6.gp=m6.gp/GAMMA, m6.msm=m6.msm/GAMMA))
m6.sp.l <- melt(m6.sp, id.vars="times")


#subtype 02_AG: model 1
m02_AG_m1.mx <- BayesianTools::MAP(m02_AG.mx.m_rd2, start=1300)
m02_AG_m1.mx.p <- as.numeric(format(round(m02_AG_m1.mx$parametersMAP, 3), nsmall = 3))
names(m02_AG_m1.mx.p) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                            "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                            "maleX", "import", "srcNe",
                            "pmsm2msm", "pgpf2gpm",
                            "initmsm", "initgp"))

m02AG_m1.gp <- sapply(times,gpspline,parms=as.list(m02_AG_m1.mx.p[1:4]))
m02AG_m1.msm <- sapply(times,msmspline,parms=as.list(m02_AG_m1.mx.p[5:8]))

m02AG_m1.sp <- as.data.frame(cbind(times, m02AG_m1.gp=m02AG_m1.gp/GAMMA, m02AG_m1.msm=m02AG_m1.msm/GAMMA))
m02AG_m1.sp.l <- melt(m02AG_m1.sp, id.vars="times")


#subtype 02_AG: model 3
# MAP after removing first 1,000 iter
m02_AG_m3.mx <- BayesianTools::MAP(AG_m3, start=1000)
m02_AG_m3.mx.p <- as.numeric(format(round(m02_AG_m3.mx$parametersMAP, 3), nsmall = 3))
names(m02_AG_m3.mx.p) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                            "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                            "maleX", "import", "srcNe",
                            "pmsm2msm", "pgpf2gpm",
                            "initmsm", "initgp"))

m02AG_m3.gp <- sapply(times,gpspline,parms=as.list(m02_AG_m3.mx.p[1:4]))
m02AG_m3.msm <- sapply(times,msmspline,parms=as.list(m02_AG_m3.mx.p[5:8]))

m02AG_m3.sp <- as.data.frame(cbind(times, m02AG_m3.gp=m02AG_m3.gp/GAMMA, m02AG_m3.msm=m02AG_m3.msm/GAMMA))
m02AG_m3.sp.l <- melt(m02AG_m3.sp, id.vars="times")

#subtype 02_AG: model 2
# MAP after removing first 1,500 iter
m02_AG_m2.mx <- BayesianTools::MAP(m2.m02_AG.mx.m_rd2, start=1500)
m02_AG_m2.mx.p <- as.numeric(format(round(m02_AG_m2.mx$parametersMAP, 3), nsmall = 3))
names(m02_AG_m2.mx.p) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                            "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                            "maleX", "import", "srcNe",
                            "pmsm2msm", "pgpf2gpm",
                            "initmsm", "initgp"))

m02AG_m2.gp <- sapply(times,gpspline,parms=as.list(m02_AG_m2.mx.p[1:4]))
m02AG_m2.msm <- sapply(times,msmspline,parms=as.list(m02_AG_m2.mx.p[5:8]))

m02AG_m2.sp <- as.data.frame(cbind(times, m02AG_m2.gp=m02AG_m2.gp/GAMMA, m02AG_m2.msm=m02AG_m2.msm/GAMMA))
m02AG_m2.sp.l <- melt(m02AG_m2.sp, id.vars="times")


#subtype 02_AG: model 4
# MAP after removing first 1,000 iter
m02_AG_m4.mx <- BayesianTools::MAP(AG_m4.2, start=1000)
m02_AG_m4.mx.p <- as.numeric(format(round(m02_AG_m4.mx$parametersMAP, 3), nsmall = 3))
names(m02_AG_m4.mx.p) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                            "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                            "maleX", "import", "srcNe",
                            "pmsm2msm", "pgpf2gpm",
                            "initmsm", "initgp"))

m02AG_m4.gp <- sapply(times,gpspline,parms=as.list(m02_AG_m4.mx.p[1:4]))
m02AG_m4.msm <- sapply(times,msmspline,parms=as.list(m02_AG_m4.mx.p[5:8]))

m02AG_m4.sp <- as.data.frame(cbind(times, m02AG_m4.gp=m02AG_m4.gp/GAMMA, m02AG_m4.msm=m02AG_m4.msm/GAMMA))
m02AG_m4.sp.l <- melt(m02AG_m4.sp, id.vars="times")

#subtype C: model 1
# MAP after removing 1000 iterations
mC_m1.mx <- BayesianTools::MAP(C_m1.m, start=1000)
mC_m1.mx.p <- as.numeric(format(round(mC_m1.mx$parametersMAP, 3), nsmall = 3))
names(mC_m1.mx.p) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                        "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                        "maleX", "import", "srcNe",
                        "pmsm2msm", "pgpf2gpm",
                        "initmsm", "initgp"))

mC_m1.gp <- sapply(times,gpspline,parms=as.list(mC_m1.mx.p[1:4]))
mC_m1.msm <- sapply(times,msmspline,parms=as.list(mC_m1.mx.p[5:8]))

mC_m1.sp <- as.data.frame(cbind(times, mC_m1.gp=mC_m1.gp/GAMMA, mC_m1.msm=mC_m1.msm/GAMMA))
mC_m1.sp.l <- melt(mC_m1.sp, id.vars="times")

#subtype C: model 3
# MAP after removing 1,000 iterations
mC_m3.mx <- BayesianTools::MAP(C_m3.m, start=1000)
mC_m3.mx.p <- as.numeric(format(round(mC_m3.mx$parametersMAP, 3), nsmall = 3))
names(mC_m3.mx.p) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                        "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                        "maleX", "import", "srcNe",
                        "pmsm2msm", "pgpf2gpm",
                        "initmsm", "initgp"))

mC_m3.gp <- sapply(times,gpspline,parms=as.list(mC_m3.mx.p[1:4]))
mC_m3.msm <- sapply(times,msmspline,parms=as.list(mC_m3.mx.p[5:8]))

mC_m3.sp <- as.data.frame(cbind(times, mC_m3.gp=mC_m3.gp/GAMMA, mC_m3.msm=mC_m3.msm/GAMMA))
mC_m3.sp.l <- melt(mC_m3.sp, id.vars="times")


#MAP for model 4 subtype C
mC_m4.mx <- BayesianTools::MAP(C_m4.m, start=1000)
mC_m4.mx.p <- as.numeric(format(round(mC_m4.mx$parametersMAP, 3), nsmall = 3))
names(mC_m4.mx.p) <- (c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                        "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                        "maleX", "import", "srcNe",
                        "pmsm2msm", "pgpf2gpm",
                        "initmsm", "initgp"))

mC_m4.gp <- sapply(times,gpspline,parms=as.list(mC_m4.mx.p[1:4]))
mC_m4.msm <- sapply(times,msmspline,parms=as.list(mC_m4.mx.p[5:8]))

mC_m4.sp <- as.data.frame(cbind(times, mC_m4.gp=mC_m4.gp/GAMMA, mC_m4.msm=mC_m4.msm/GAMMA))
mC_m4.sp.l <- melt(mC_m4.sp, id.vars="times")


p1 <- ggplot(m2.sp.l, aes(x=times)) +
  geom_line(aes(y = value, colour=variable)) +
  ggtitle("Model 2") + ylab("R") + theme_bw()


p2 <- ggplot(m5.sp.l, aes(x=times)) +
  geom_line(aes(y = value, colour=variable)) +
  ggtitle("Model 5:prevalence") + ylab("R") + theme_bw()

p3 <- ggplot(m4.sp.l, aes(x=times)) +
  geom_line(aes(y = value, colour=variable)) +
  ggtitle("Model 4") + ylab("R") + theme_bw()

p4 <- ggplot(m6.sp.l, aes(x=times)) +
  geom_line(aes(y = value, colour=variable)) +
  ggtitle("Model 6:prevalence") + ylab("R") + theme_bw()


p5 <- ggplot(m02AG_m1.sp.l, aes(x=times)) +
  geom_line(aes(y = value, colour=variable)) +
  ggtitle("02AG Model1") + ylab("R") + theme_bw()


p6 <- ggplot(m02AG_m3.sp.l, aes(x=times)) +
  geom_line(aes(y = value, colour=variable)) +
  ggtitle("02AG Model3:prevalence") + ylab("R") + theme_bw()


p7 <- ggplot(m02AG_m2.sp.l, aes(x=times)) +
  geom_line(aes(y = value, colour=variable)) +
  ggtitle("02AG Model2") + ylab("R") + theme_bw()


p8 <- ggplot(m02AG_m4.sp.l, aes(x=times)) +
  geom_line(aes(y = value, colour=variable)) +
  ggtitle("02AG Model4:prevalence") + ylab("R") + theme_bw()


p9 <- ggplot(mC_m1.sp.l, aes(x=times)) +
  geom_line(aes(y = value, colour=variable)) +
  ggtitle("C Model1") + ylab("R") + theme_bw()


p10 <- ggplot(mC_m3.sp.l, aes(x=times)) +
  geom_line(aes(y = value, colour=variable)) +
  ggtitle("C Model3:prevalence") + ylab("R") + theme_bw()


p11 <- ggplot(mC_m4.sp.l, aes(x=times)) +
  geom_line(aes(y = value, colour=variable)) +
  ggtitle("C Model4:prevalence") + ylab("R") + theme_bw()


quartz()
multiplot(p1,p3,p2,p4, cols=2)
multiplot(p5,p7,p6,p8, cols=2)
multiplot(p9,p11,p10, cols=2)
