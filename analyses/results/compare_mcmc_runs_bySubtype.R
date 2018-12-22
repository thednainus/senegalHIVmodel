# Script to compare whether 2 merged MCMC runs show similar posterior
# distributions.
# The data we load in this script are data that we generated after running
# several MCMC runs to estimate the parameters of our mathematical model.
# Here we only aim to compare the posterior distribution for each parameter
# to check whether they show similar distributions.

library(BayesianTools)
library(reshape2)
library(ggplot2)


# load data for subtype C
load(system.file("data/mcmc_runs/C_m1.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/C_m2.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/C_m3.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/C_m4.rda", package = "senegalHIVmodel"))


# get samples for each merged runs
# different burnin was used depending on the different model we analysed
# Model 1
Cm1_1.s <- get_posterior(run = C_m1.1,
                         burnin = 1200,
                         rep = "1",
                         par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                        "maleX", "import", "srcNe",
                                        "pmsm2msm", "pgpf2gpm",
                                        "initmsm", "initgp"))
Cm1_2.s <- get_posterior(run = C_m1.2,
                         burnin = 1200,
                         rep = "2",
                         par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))
Cm1_3.s <- get_posterior(run = C_m1.m,
                         burnin = 1000,
                         rep = "3",
                         par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))


# Model 2
Cm2_1.s <- get_posterior(run = C_m2.1,
                         burnin = 1500,
                         rep = "1",
                         par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))
Cm2_2.s <- get_posterior(run = C_m2.2,
                         burnin = 1500,
                         rep = "2",
                         par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))

# Model 3
Cm3_1.s <- get_posterior(run = C_m3.1,
                         burnin = 1000,
                         rep = "1",
                         par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))
Cm3_2.s <- get_posterior(run = C_m3.2,
                         burnin = 1200,
                         rep = "2",
                         par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))
Cm3_3.s <- get_posterior(run = C_m3.m,
                         burnin = 1000,
                         rep = "3",
                         par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))

# Model 4
Cm4_1.s <- get_posterior(run = C_m4.1,
                         burnin = 1200,
                         rep = "1",
                         par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))
Cm4_2.s <- get_posterior(run = C_m4.2,
                         burnin = 1200,
                         rep = "2",
                         par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))
Cm4_3.s <- get_posterior(run = C_m4.m,
                         burnin = 1000,
                         rep = "3",
                         par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))


# Bind by Model
Cm1 <- rbind(Cm1_1.s, Cm1_2.s, Cm1_3.s)
Cm2 <- rbind(Cm2_1.s, Cm2_2.s)
Cm3 <- rbind(Cm3_1.s, Cm3_2.s, Cm3_3.s)
Cm4 <- rbind(Cm4_1.s, Cm4_2.s, Cm4_3.s)

# convert to long format
Cm1.l <- melt(Cm1, id.vars = "Rep")
Cm2.l <- melt(Cm2, id.vars = "Rep")
Cm3.l <- melt(Cm3, id.vars = "Rep")
Cm4.l <- melt(Cm4, id.vars = "Rep")

#Plot
p1 <- ggplot(Cm1.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("Subtype C: Model 1") +  theme_bw()

p2 <- ggplot(Cm2.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("Subtype C: Model 2") +  theme_bw()

p3 <- ggplot(Cm3.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("Subtype C: Model 3") +  theme_bw()

p4 <- ggplot(Cm4.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("Subtype C: Model 4") +  theme_bw()



# SUBTYPE 02_AG
# load data for subtype 02_AG
load(system.file("data/mcmc_runs/AG_m1.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/AG_m2.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/AG_m3.rda", package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/AG_m4.rda", package = "senegalHIVmodel"))


# get samples for each merged runs
# different burnin was used depending on the different model we analysed
# Model 1
AGm1_1.s <- get_posterior(run = AG_m1.1,
                          burnin = 4000,
                          rep = "1",
                          par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))
AGm1_2.s <- get_posterior(run = AG_m1.2,
                          burnin = 4000,
                          rep = "2",
                          par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))

# Model 2
AGm2_1.s <- get_posterior(run = AG_m2.1,
                          burnin = 4000,
                          rep = "1",
                          par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))
AGm2_2.s <- get_posterior(run = AG_m2.2,
                          burnin = 4000,
                          rep = "2",
                          par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))

# Model 3
AGm3_1.s <- get_posterior(run = AG_m3.1,
                          burnin = 1200,
                          rep = "1",
                          par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))
AGm3_2.s <- get_posterior(run = AG_m3.2,
                          burnin = 1000,
                          rep = "2",
                          par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))
AGm3_3.s <- get_posterior(run = AG_m3.3,
                          burnin = 1000,
                          rep = "3",
                          par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))



# Model 4
AGm4_1.s <- get_posterior(run = AG_m4.1,
                          burnin = 1000,
                          rep = "1",
                          par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))
AGm4_2.s <- get_posterior(run = AG_m4.2,
                          burnin = 1000,
                          rep = "2",
                          par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))
AGm4_3.s <- get_posterior(run = AG_m4.3,
                          burnin = 1000,
                          rep = "3",
                          par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                       "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                       "maleX", "import", "srcNe",
                                       "pmsm2msm", "pgpf2gpm",
                                       "initmsm", "initgp"))

AGm4_4.s <- get_posterior(run = AG_m4.4,
                           burnin = 1000,
                           rep = "4",
                           par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                        "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                        "maleX", "import", "srcNe",
                                        "pmsm2msm", "pgpf2gpm",
                                        "initmsm", "initgp"))


# Bind by Model
AGm1 <- rbind(AGm1_1.s, AGm1_2.s)
AGm2 <- rbind(AGm2_1.s, AGm2_2.s)
AGm3 <- rbind(AGm3_1.s, AGm3_2.s, AGm3_3.s)
AGm4 <- rbind(AGm4_1.s, AGm4_2.s, AGm4_3.s, AGm4_4.s)

# convert to long format
AGm1.l <- melt(AGm1, id.vars = "Rep")
AGm2.l <- melt(AGm2, id.vars = "Rep")
AGm3.l <- melt(AGm3, id.vars = "Rep")
AGm4.l <- melt(AGm4, id.vars = "Rep")


#Plot
p1 <- ggplot(AGm1.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("Subtype 02_AG: Model 1") +  theme_bw()

p2 <- ggplot(AGm2.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("Subtype 02_AG: Model 2") +  theme_bw()

p3 <- ggplot(AGm3.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("Subtype 02_AG: Model 3") +  theme_bw()

p4 <- ggplot(AGm4.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("Subtype 02_AG: Model 4") +  theme_bw()
