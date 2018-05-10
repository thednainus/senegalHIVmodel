library(BayesianTools)
library(akima)
library(phydynR)
library(senegalHIVmodel)


# print R session Info. List R version and loaded packages,
# and information o operational sytem (OS)
sessionInfo()

# Choose and set a seed for all analysis.
# Helpful for reproducing results
seed <- as.integer(runif(n = 1, min = 1, max = 10000))
message(seed)
set.seed(seed)


# gpm = general population males
# gpf = general population females
# msm = man that have sex with other man
# src = source (sequences closely related to population being studies by that are from other countries)
demes <- c('gpm', 'gpf', 'msm', 'src')

# Sets the equations of the model (birth, death and migration rates are pre-filled with zeros)
eqns <- setup.model.equations(demes)
attach(eqns)

# These are the values used for the simulations
# Initial time for simulations
T0 <- 1978
# Final time for simulations
T1 <- 2014
# Duration of infection. In our model we assumed 1 stage of infection
GAMMA <- 1/10

# parameter template:
# gpsp0, gpsp1, gpsp2, and gpsploc are the necessary parameters to estimate the
#    spline function (gpspline) for the general population (gp)
# msmsp0, msmsp1, msmsp2, msmsploc are the necessary parameters to estimate the
#    spline function (msmspline) for the msm risk group
# maleX is the ratio of infectiouness of males to females
# import is the importation rate of HIV from other countries to Senegal
# srcNe is the
# pmsm2msm is the probability of msm to infect another msm
# pgpf2gpm is the probability of a female from the gp to infect a male from de gp
# initmsm is the initial size of infected msm which is 1
# initgp is the initial size of infected gp which is 1
THETA <- list(
  gpsp0 = 6/10,
  gpsp1 = 4/10,
  gpsp2 = 1/10,
  gpsploc = 1987,
  msmsp0 = 4/10,
  msmsp1 = 4/10,
  msmsp2 = 2/10,
  msmsploc = 1995,
  maleX = 1.02,
  import = 1/20,
  srcNe = 1/10,
  gpspline = function( t, parms ){
    if (t < T0 ) return( parms$gpsp0 )
    if (t > T1) return (parms$gpsp2)
    with(parms, pmax(0, aspline( x = c(T0, gpsploc, T1), y=c(gpsp0, gpsp1, gpsp2) , xout = t)$y) )
  },
  msmspline  = function( t, parms){
    if (t < T0 ) return( parms$msmsp0 )
    if (t > T1) return ( parms$msmsp2 )
    with(parms, pmax(0, aspline( x = c(T0, msmsploc, T1), y=c(msmsp0, msmsp1, msmsp2) , xout = t)$y) )
  },
  pmsm2msm = 0.85,
  pgpf2gpm = 0.85,
  initmsm = 1,
  initgp = 1
)

# arbitrary large number > A(t) forall t
SRCSIZE <<- 1e5
# X0 is the initial conditions for the 4 demes (gpf, gpm, msm, src)
X0 <- c(gpm = unname(THETA$initgp/2),
        gpf = unname(THETA$initgp/2),
        msm = unname(THETA$initmsm),
        src = SRCSIZE)

# Because there are 4 demes in this model, the birth matrix is a 4 x 4 matrix
# Each element in the matrix is a string that will be passed as R code
births['msm', 'msm'] <- 'parms$msmspline(t, parms) * msm * parms$pmsm2msm'
births['msm', 'gpf'] <- 'parms$msmspline(t, parms) * msm * (1-parms$pmsm2msm)'

births['gpm', 'gpf'] <- 'parms$gpspline(t, parms) * gpm * parms$maleX'
births['gpf', 'gpm'] <- 'parms$gpspline(t, parms) * gpf * parms$pgpf2gpm'
births['gpf', 'msm'] <- 'parms$gpspline(t, parms) * gpf * (1-parms$pgpf2gpm)'

# f = (1/2)*(Y^2)/Ne
births['src', 'src'] <- '0.5 * SRCSIZE^2 / parms$srcNe'

# Migrations is also a 4 x 4 matrix because we have 4 demes
migs['src', 'gpm'] <- 'parms$import * gpm'
migs['src', 'gpf'] <- 'parms$import * gpf'
migs['src', 'msm'] <- 'parms$import * msm'

migs['gpm', 'src'] <- 'parms$import * gpm'
migs['gpf', 'src'] <- 'parms$import * gpf'
migs['msm', 'src'] <- 'parms$import * msm'

# Deaths is a vector that showed in which rate a lineage dies
deaths['msm'] <- 'GAMMA * msm'
deaths['gpf'] <- 'GAMMA * gpf'
deaths['gpm'] <- 'GAMMA * gpm'
deaths['src'] <- '0.5 * SRCSIZE^2 / parms$srcNe'

#sde = FALSE means that an ordinary differential equation model will be constructed
# build the demographic process to be used in the coalescent analysis
dm <- build.demographic.process(births = births,
                                deaths = deaths,
                                migrations = migs,
                                parameterNames = names(THETA),
                                rcpp = FALSE,
                                sde = FALSE)

#show.demographic.process( dm, x0 = X0, t0 = 1980, t1 = 2014, theta = THETA )
#o <- dm( x0 = X0, t0 = 1980, t1 = 2014, theta = THETA )[[5]]
#print(o)

