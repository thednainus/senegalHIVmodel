# Script to compare whether 2 merged MCMC runs show similar posterior
# distributions.
# The data we load in this script are data that we generated after running
# several MCMC runs to estimate the parameters of our mathematical model.
# Here we only aim to compare the posterior distribution for each parameter
# to check whether they show similar distributions.

library(BayesianTools)
library(reshape2)
library(ggplot2)


# load data for combined subtypes
load(system.file("data/mcmc_runs/FINAL/Model2.rda",
                 package = "senegalHIVmodel"))

load(system.file("data/mcmc_runs/FINAL/Model3.rda",
                 package = "senegalHIVmodel"))

load(system.file("data/mcmc_runs/FINAL/Model4.rda",
                 package = "senegalHIVmodel"))



# get samples for each merged MCMC runs
# different burnin was used depending on the different model we analysed
# Model 2
m2_1.s <- get_posterior(run = m2.1,
                        burnin = 2000,
                        rep = "1",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))

m2_2.s <- get_posterior(run = m2.2,
                        burnin = 2000,
                        rep = "2",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))

m2_3.s <- get_posterior(run = m2.3,
                        burnin = 1500,
                        rep = "3",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))


# Model 3
m3_1.s <- get_posterior(run = m3.1,
                        burnin = 1200,
                        rep = "1",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))

m3_2.s <- get_posterior(run = m3.2,
                        burnin = 1200,
                        rep = "2",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))


# Model 4
m4_1.s <- get_posterior(run = m4.1,
                        burnin = 4000,
                        rep = "1",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))

m4_2.s <- get_posterior(run = m4.2,
                        burnin = 4000,
                        rep = "2",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))

m4_3.s <- get_posterior(run = m4.3,
                        burnin = 2000,
                        rep = "3",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))


# Bind by Model
m2 <- rbind(m2_1.s, m2_2.s, m2_3.s)
m3 <- rbind(m3_1.s, m3_2.s)
m4 <- rbind(m4_1.s, m4_2.s, m4_3.s)

# convert to long format
m2.l <- melt(m2, id.vars = "Rep")
m3.l <- melt(m3, id.vars = "Rep")
m4.l <- melt(m4, id.vars = "Rep")



#Plot
p1 <- ggplot(m2.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("All Subtypes: Model 2") +  theme_bw()

p2 <- ggplot(m3.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("All Subtypes: Model 3") +  theme_bw()

p3 <- ggplot(m4.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("All subtypes: Model 4") +  theme_bw()


#######################################################################
# PREVALENCE

# load data for combined subtypes
load(system.file("data/mcmc_runs/FINAL/Model5.rda",
                 package = "senegalHIVmodel"))

load(system.file("data/mcmc_runs/FINAL/Model6.rda",
                 package = "senegalHIVmodel"))

load(system.file("data/mcmc_runs/FINAL/Model7.rda",
                 package = "senegalHIVmodel"))



# get samples for each merged runs
# Model 5
m5_1.s <- get_posterior(run = m5.1,
                        burnin = 4000,
                        rep = "1",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))

m5_2.s <- get_posterior(run = m5.2,
                        burnin = 4000,
                        rep = "2",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))

m5_3.s <- get_posterior(run = m5.3,
                        burnin = 1500,
                        rep = "3",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))


# Model 6
m6_1.s <- get_posterior(run = m6.1,
                        burnin = 1200,
                        rep = "1",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))

m6_2.s <- get_posterior(run = m6.2,
                        burnin = 1000,
                        rep = "2",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))

m6_3.s <- get_posterior(run = m6.3,
                        burnin = 2000,
                        rep = "3",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))


# Model 7
m7_1.s <- get_posterior(run = m7.1,
                        burnin = 2100,
                        rep = "1",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))

m7_2.s <- get_posterior(run = m7.2,
                        burnin = 2100,
                        rep = "2",
                        par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                      "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                      "maleX", "import", "srcNe",
                                      "pmsm2msm", "pgpf2gpm",
                                      "initmsm", "initgp"))


# Bind by Model
m5 <- rbind(m5_1.s, m5_2.s, m5_3.s)
m6 <- rbind(m6_1.s, m6_2.s, m6_3.s)
m7 <- rbind(m7_1.s, m7_2.s)

# convert to long format
m5.l <- melt(m5, id.vars = "Rep")
m6.l <- melt(m6, id.vars = "Rep")
m7.l <- melt(m7, id.vars = "Rep")



#Plot
p1 <- ggplot(m5.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("All Subtypes: Model 5 - prevalence") +  theme_bw()

p2 <- ggplot(m6.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("All Subtypes: Model 6 - prevalence") +  theme_bw()

p3 <- ggplot(m7.l, aes(value, colour=Rep)) +
  geom_density() + facet_wrap(~ variable, scales = "free") +
  ggtitle("All subtypes: Model 7 - prevalence") +  theme_bw()
