# First set of results for the MCMC
# When not estimating the initial gp and msm population size

library(phydynR)
library(akima)

demes <- c('gpm', 'gpf', 'msm', 'src')

eqns <- setup.model.equations(demes)
attach(eqns)

T0 <- 1978
T1 <- 2014
GAMMA <- 1/10


# MAP for parameters:
THETA <- list(
  gpsp0 = 0.56,
  gpsp1 = 0.80,
  gpsp2 = 0.31,
  gpsploc = 1980.78,
  msmsp0 = 0.91,
  msmsp1 = 0.30,
  msmsp2 = 0.12,
  msmsploc = 1998.029,
  maleX = 1.02,
  import = 0.06,
  srcNe = 0.29,
  gpspline = function( t, parms ){
    if (t < T0 ) return( parms$gpsp0 )
    if (t > T1) return (parms$gpsp2)
    with(parms, aspline( x = c(T0,gpsploc,T1), y=c(gpsp0,gpsp1,gpsp2) , xout = t)$y )
  },
  msmspline  = function( t, parms){
    if (t < T0 ) return( parms$msmsp0 )
    if (t > T1) return ( parms$msmsp2 )
    with(parms, aspline( x = c(T0,msmsploc,T1), y=c(msmsp0,msmsp1,msmsp2) , xout = t)$y )
  },
  pmsm2msm = 0.99,
  pgpf2gpm = 0.48,
  initmsm = 254.83,
  initgp = 16.37
)


show.demographic.process.mod <- function (demo.model, theta, x0, t0, t1, res = 1000, integrationMethod = "lsoda", ...)
{
  tfgy <- demo.model(theta, x0, t0, t1, res = 1000, integrationMethod = integrationMethod)
  o <- tfgy[[5]]
  t <- o[, 1]
  if (((ncol(o) - 1) == 2) & tail(colnames(o), 1) == "V2") {
    plot(t, o[, 2], type = "l", xlab = "Time", ylab = colnames(o)[2],
         ...)
  }
  else {
    matplot(t, o[, 2:ncol(o)], type = "l", xlab = "Time",
            ylab = "", ...)
    legend("topleft", inset = 0.05, legend = colnames(o)[2:ncol(o)],
           pch = 1, col = 1:(ncol(o) - 1), horiz = TRUE)
  }
}



SRCSIZE <<- 1e5 # arbitrary large number > A(t) forall t
X0 <- c( gpm = unname( THETA$initgp/2 ), gpf = unname( THETA$initgp/2), msm = unname( THETA$initmsm ) , src = SRCSIZE)

births['msm', 'msm'] <- 'parms$msmspline( t, parms ) * msm * parms$pmsm2msm'
births['msm', 'gpf'] <- 'parms$msmspline( t, parms ) * msm * (1-parms$pmsm2msm)'

births['gpm', 'gpf'] <- 'parms$gpspline( t, parms ) * gpm * parms$maleX'
births['gpf', 'gpm'] <- 'parms$gpspline( t, parms ) * gpf * parms$pgpf2gpm'
births['gpf', 'msm'] <- 'parms$gpspline( t, parms ) * gpf * (1-parms$pgpf2gpm)'

# f = (1/2)*(Y^2)/Ne
births['src', 'src'] <- '.5*SRCSIZE^2/parms$srcNe'

migs['src', 'gpm'] <- 'parms$import * gpm'
migs['src', 'gpf'] <- 'parms$import * gpf'
migs['src', 'msm'] <- 'parms$import * msm'

migs['gpm', 'src'] <- 'parms$import * gpm'
migs['gpf', 'src'] <- 'parms$import * gpf'
migs['msm', 'src'] <- 'parms$import * msm'

deaths['msm'] <- 'GAMMA * msm'
deaths['gpf'] <- 'GAMMA * gpf'
deaths['gpm'] <- 'GAMMA * gpm'
deaths['src'] <- '.5*SRCSIZE^2/parms$srcNe'

dm <- build.demographic.process(births, deaths = deaths, parameterNames = names(THETA), rcpp = FALSE, sde = FALSE)
show.demographic.process.mod( dm, x0 = X0, t0 = 1978, t1 = 2014, theta = THETA )
quartz()
