library(phydynR)
library(senegalHIVmodel)
library(ggplot2)
library(reshape2)
library(BayesianTools)

# These are the values used for the simulations
# Initial time for simulations
T0 <- 1978
# Final time for simulations
T1 <- 2014
# Duration of infection. In our model we assumed 1 stage of infection
GAMMA <- 1/10

times <- seq(1978, 2014, length.out = 1000)

###############################################################################
# SUBTYPE C
load(system.file("data/mcmc_runs/FINAL/C_m1.rda",
                 package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/FINAL/C_m2.rda",
                 package = "senegalHIVmodel"))

# PREVALENCE
load(system.file("data/mcmc_runs/FINAL/C_m3.rda",
                 package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/FINAL/C_m4.rda",
                 package = "senegalHIVmodel"))

##############################################################################
# Subtype C MODEL 1
C_m1_r0 <- df_r0(run = C_m1.2,
               burnin = 1200,
               par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                             "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                             "maleX", "import", "srcNe",
                             "pmsm2msm", "pgpf2gpm",
                             "initmsm", "initgp"),
               times = times,
               T0 = T0,
               T1 = T1,
               GAMMA = GAMMA)
C_m1_r0["Model"] <- "Model 1"

##############################################################################
# Subtype C MODEL 2
C_m2_r0 <- df_r0(run = C_m2.2,
                 burnin = 1500,
                 par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                               "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                               "maleX", "import", "srcNe",
                               "pmsm2msm", "pgpf2gpm",
                               "initmsm", "initgp"),
                 times = times,
                 T0 = T0,
                 T1 = T1,
                 GAMMA = GAMMA)
C_m2_r0["Model"] <- "Model 2"

##############################################################################
# Subtype C MODEL 3
C_m3_r0 <- df_r0(run = C_m3.1,
                 burnin = 1000,
                 par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                               "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                               "maleX", "import", "srcNe",
                               "pmsm2msm", "pgpf2gpm",
                               "initmsm", "initgp"),
                 times = times,
                 T0 = T0,
                 T1 = T1,
                 GAMMA = GAMMA)
C_m3_r0["Model"] <- "Model 3"

##############################################################################
# Subtype C MODEL 4
C_m4_r0 <- df_r0(run = C_m4.2,
                 burnin = 1200,
                 par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                               "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                               "maleX", "import", "srcNe",
                               "pmsm2msm", "pgpf2gpm",
                               "initmsm", "initgp"),
                 times = times,
                 T0 = T0,
                 T1 = T1,
                 GAMMA = GAMMA)
C_m4_r0["Model"] <- "Model 4"

###############################################################################
#merge dataframes
r0_C <- rbind(C_m1_r0, C_m2_r0, C_m3_r0, C_m4_r0)
r0_C$Model <- as.factor(r0_C$Model)
r0_C.l <- melt(r0_C, id.vars = c("times", "lower", "upper", "group",
                                           "Model"))

#############################################################################
#PLOT
Cr0.p1 <- ggplot(r0_C.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour = group, linetype = variable)) +
  facet_wrap(~ Model, scales = "free") +
  ggtitle("Subtype C") +
  xlab("Time (years)") +
  ylab("Basic reproduction number") +
  theme_bw()

###############################################################################
###############################################################################
# SUBTYPE 02_AG
load(system.file("data/mcmc_runs/FINAL/AG_m1.rda",
                 package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/FINAL/AG_m2.rda",
                 package = "senegalHIVmodel"))

# PREVALENCE
load(system.file("data/mcmc_runs/FINAL/AG_m3.rda",
                 package = "senegalHIVmodel"))
load(system.file("data/mcmc_runs/FINAL/AG_m4.rda",
                 package = "senegalHIVmodel"))

##############################################################################
# Subtype 02_AG MODEL 1
AG_m1_r0 <- df_r0(run = AG_m1.1,
                 burnin = 4000,
                 par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                               "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                               "maleX", "import", "srcNe",
                               "pmsm2msm", "pgpf2gpm",
                               "initmsm", "initgp"),
                 times = times,
                 T0 = T0,
                 T1 = T1,
                 GAMMA = GAMMA)
AG_m1_r0["Model"] <- "Model 1"

##############################################################################
# Subtype 02_AG MODEL 2
AG_m2_r0 <- df_r0(run = AG_m2.1,
                  burnin = 4000,
                  par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                "maleX", "import", "srcNe",
                                "pmsm2msm", "pgpf2gpm",
                                "initmsm", "initgp"),
                  times = times,
                  T0 = T0,
                  T1 = T1,
                  GAMMA = GAMMA)
AG_m2_r0["Model"] <- "Model 2"

##############################################################################
# Subtype 02_AG MODEL 3
AG_m3_r0 <- df_r0(run = AG_m3.2,
                  burnin = 1200,
                  par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                "maleX", "import", "srcNe",
                                "pmsm2msm", "pgpf2gpm",
                                "initmsm", "initgp"),
                  times = times,
                  T0 = T0,
                  T1 = T1,
                  GAMMA = GAMMA)
AG_m3_r0["Model"] <- "Model 3"

##############################################################################
# Subtype 02_AG MODEL 4
AG_m4_r0 <- df_r0(run = AG_m4.2,
                  burnin = 1000,
                  par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                                "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                                "maleX", "import", "srcNe",
                                "pmsm2msm", "pgpf2gpm",
                                "initmsm", "initgp"),
                  times = times,
                  T0 = T0,
                  T1 = T1,
                  GAMMA = GAMMA)
AG_m4_r0["Model"] <- "Model 4"

###############################################################################
#merge dataframes
r0_AG <- rbind(AG_m1_r0, AG_m2_r0, AG_m3_r0, AG_m4_r0)
r0_AG$Model <- as.factor(r0_AG$Model)
r0_AG.l <- melt(r0_AG, id.vars = c("times", "lower", "upper", "group",
                                   "Model"))

#############################################################################
#PLOT
AGr0.p1 <- ggplot(r0_AG.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour = group, linetype = variable)) +
  facet_wrap(~ Model, scales = "free") +
  ggtitle("Subtype 02_AG") +
  xlab("Time (years)") +
  ylab("Basic reproduction number") +
  theme_bw()

###############################################################################
###############################################################################
# ALL SUBTYPES
# load data for subtypes combined
load(system.file("data/mcmc_runs/FINAL/Model2.rda",
                 package = "senegalHIVmodel"))

load(system.file("data/mcmc_runs/FINAL/Model3.rda",
                 package = "senegalHIVmodel"))

load(system.file("data/mcmc_runs/FINAL/Model4.rda",
                 package = "senegalHIVmodel"))

# PREVALENCE
load(system.file("data/mcmc_runs/FINAL/Model5.rda",
                 package = "senegalHIVmodel"))

load(system.file("data/mcmc_runs/FINAL/Model6.rda",
                 package = "senegalHIVmodel"))

load(system.file("data/mcmc_runs/FINAL/Model7.rda",
                 package = "senegalHIVmodel"))

##############################################################################
# MODEL 2
m2_r0 <- df_r0(run = m2.2,
               burnin = 2000,
               par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                             "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                             "maleX", "import", "srcNe",
                             "pmsm2msm", "pgpf2gpm",
                             "initmsm", "initgp"),
              times = times,
              T0 = T0,
              T1 = T1,
              GAMMA = GAMMA)
m2_r0["Model"] <- "Model 2"

##############################################################################
# MODEL 3
m3_r0 <- df_r0(run = m3.2,
               burnin = 1200,
               par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                             "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                             "maleX", "import", "srcNe",
                             "pmsm2msm", "pgpf2gpm",
                             "initmsm", "initgp"),
               times = times,
               T0 = T0,
               T1 = T1,
               GAMMA = GAMMA)
m3_r0["Model"] <- "Model 3"

##############################################################################
# MODEL 4
m4_r0 <- df_r0(run = m4.2,
               burnin = 4000,
               par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                             "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                             "maleX", "import", "srcNe",
                             "pmsm2msm", "pgpf2gpm",
                             "initmsm", "initgp"),
               times = times,
               T0 = T0,
               T1 = T1,
               GAMMA = GAMMA)
m4_r0["Model"] <- "Model 4"

##############################################################################
# MODEL 5: PREVALENCE
m5_r0 <- df_r0(run = m5.1,
               burnin = 4000,
               par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                             "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                             "maleX", "import", "srcNe",
                             "pmsm2msm", "pgpf2gpm",
                             "initmsm", "initgp"),
               times = times,
               T0 = T0,
               T1 = T1,
               GAMMA = GAMMA)
m5_r0["Model"] <- "Model 5"

##############################################################################
# MODEL 6: PREVALENCE
m6_r0 <- df_r0(run = m6.1,
               burnin = 1200,
               par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                             "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                             "maleX", "import", "srcNe",
                             "pmsm2msm", "pgpf2gpm",
                             "initmsm", "initgp"),
               times = times,
               T0 = T0,
               T1 = T1,
               GAMMA = GAMMA)
m6_r0["Model"] <- "Model 6"

##############################################################################
# MODEL 7: PREVALENCE
m7_r0 <- df_r0(run = m7.2,
               burnin = 2100,
               par_names = c("gpsp0", "gpsp1", "gpsp2", "gpsploc",
                             "msmsp0", "msmsp1", "msmsp2", "msmsploc",
                             "maleX", "import", "srcNe",
                             "pmsm2msm", "pgpf2gpm",
                             "initmsm", "initgp"),
               times = times,
               T0 = T0,
               T1 = T1,
               GAMMA = GAMMA)
m7_r0["Model"] <- "Model 7"

###############################################################################
#merge dataframes
r0_m2and5 <- rbind(m2_r0, m5_r0)
r0_m2and5$Model <- as.factor(r0_m2and5$Model)
r0_m2and5.l <- melt(r0_m2and5, id.vars = c("times", "lower", "upper", "group",
                                           "Model"))

r0_m3and6 <- rbind(m3_r0, m6_r0)
r0_m3and6$Model <- as.factor(r0_m3and6$Model)
r0_m3and6.l <- melt(r0_m3and6, id.vars = c("times", "lower", "upper", "group",
                                           "Model"))

r0_m4and7 <- rbind(m4_r0, m7_r0)
r0_m4and7$Model <- as.factor(r0_m4and7$Model)
r0_m4and7.l <- melt(r0_m4and7, id.vars = c("times", "lower", "upper", "group",
                                           "Model"))

#############################################################################
#PLOT
m2and5.r0 <- ggplot(r0_m2and5.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour = group, linetype = variable)) +
  facet_wrap(~ Model, scales = "free") +
  ggtitle("Subtypes Combined") +
  xlab("Time (years)") +
  ylab("Basic reproduction number") +
  theme_bw()


m3and6.r0 <- ggplot(r0_m3and6.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour = group, linetype = variable)) +
  facet_wrap(~ Model, scales = "free") +
  ggtitle("Subtypes Combined") +
  xlab("Time (years)") +
  ylab("Basic reproduction number") +
  theme_bw()

m4and7.r0 <- ggplot(r0_m4and7.l, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.20) +
  geom_line(aes(y = value, colour = group, linetype = variable)) +
  facet_wrap(~ Model, scales = "free") +
  ggtitle("Subtypes Combined") +
  xlab("Time (years)") +
  ylab("Basic reproduction number") +
  theme_bw()
