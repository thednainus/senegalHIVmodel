invisible('
* Fast MAP estimation from multiple starting conditions
* using modelv2 -- linear spline
* Update prior and bounds for src Ne (was too small!) :
* adjust lower bound transmission rates (correspond to R0=.5)
* use load data v2 - only count dakar seqs
')

source('1.model.v2.R')
source('2.load_data.v2.R')

# This object function will receive the proposals of the MCMC (Markov chain Monte Carlo).
# The reason of using an object function is to make it easier to change the
# values of the parameters to be estimated in THETA.
# Note that not all parameters listed in THETA will be estimated
obj_fun <- function(parameters){
  # we use unname here because "parameters" can be as vectors or matrix, and
  # sometimes it comes with column names, which I chose to remove these column names
  # in here.
  parameters <- unname(parameters)

  # add the values of THETA to a new variable named THETA.new
  THETA.new <- THETA

  # change the values in THETA.new to the new proposals that will be evaluated
  THETA.new$gpsp0 <- parameters[1]
  THETA.new$gpsp1 <- parameters[2]
  THETA.new$gpsp2 <- parameters[3]
  THETA.new$gpsploc <- parameters[4]
  THETA.new$msmsp0 <- parameters[5]
  THETA.new$msmsp1 <- parameters[6]
  THETA.new$msmsp2 <- parameters[7]
  THETA.new$msmsploc <- parameters[8]
  THETA.new$import <- parameters[9]
  THETA.new$srcNe <- parameters[10]
  THETA.new$pmsm2msm <- parameters[11]
  THETA.new$pgpf2gpm <- parameters[12]
  THETA.new$initmsm <- parameters[13]
  THETA.new$initgp <- parameters[14]

  # X0 is the initial conditions for the 4 demes (gpf, gpm, msm, src)
  X0 <- c(gpm = unname(THETA.new$initgp/2),
          gpf = unname(THETA.new$initgp/2),
          msm = unname(THETA.new$initmsm) ,
          src = 1e5)

  # After changing the parameter values to the new proposals, a likelihood is
  # calculated with the funtion colik.
  # Note that this function uses several global variables, such as, dated.tree, dm, and X0
  mll <- colik(tree = dated.tree.dakar,
               theta = THETA.new,
               demographic.process.model = dm,
               x0 = X0,
               t0 = 1978,
               res = 1e3, #TODO 
               timeOfOriginBoundaryCondition = FALSE,
               AgtY_penalty = 1,
               maxHeight = 35)

  return(mll)

}


# Specify a density function to be used in the prior especification (see below)
densities <-  function(par){
  # d1 to d3 and d5 to d7 I am using a lognormal distribution with mean = R0 = 1.1 and sigma = 1
  # d4 and d8 uniform distribution between the start time and the most recent sample
  # d9 exponential distribution with mean around 1/30
  # d10 exponential distribution with mean around 1/20
  d1 = dgamma(par[1], shape = 3, rate = 3/0.1, log = TRUE) #gpsp0
  d2 = dgamma(par[2], shape = 3, rate = 3/0.1, log = TRUE) #gpsp1
  d3 = dgamma(par[3], shape = 3, rate = 3/0.1, log = TRUE) #gpsp2
  d4 = dunif(par[4], min = 1978, max = 2014, log = TRUE) #gpsploc
  d5 = dgamma(par[5], shape = 3, rate = 3/0.1, log = TRUE) #msmsp0
  d6 = dgamma(par[6], shape = 3, rate = 3/0.1, log = TRUE) #msmsp1
  d7 = dgamma(par[7], shape = 3, rate = 3/0.1, log = TRUE) #msmsp2
  d8 = dunif(par[8], min = 1978, max = 2014, log = TRUE) #msmsploc
  d9 = dexp(par[9], rate = 30, log = TRUE) #import
  d10 = dexp(par[10], rate = 1/100, log = TRUE) #srcNe
  d11 = dbeta(par[11], shape1 = 16, shape2 = 4, log = TRUE) #pmsm2msm
  d12 = dbeta(par[12], shape1 = 16, shape2 = 4, log = TRUE) #pgpf2gpm
  d13 = dexp(par[13], rate = 1/3, log = TRUE) #initmsm
  d14 = dexp(par[14], rate = 1/3, log = TRUE) #initgp

  return(d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12 + d13 + d14)
}


# Create sampling, this is optional but recommended because the MCMCs can generate automatic starting
# conditions if this is provided
sampler <-  function(n=1){
  d1 = rgamma(n, shape = 4, rate = 4/0.6) #gpsp0
  d2 = rgamma(n, shape = 4, rate = 4/0.4) #gpsp1
  d3 = rgamma(n, shape = 4, rate = 4/0.1) #gpsp2
  d4 = runif(n, min = 1985, max = 2000) #gpsploc
  d5 = rgamma(n, shape = 4, rate = 4/0.4) #msmsp0
  d6 = rgamma(n, shape = 4, rate = 4/0.4) #msmsp1
  d7 = rgamma(n, shape = 4, rate = 4/0.2) #msmsp2
  d8 = runif(n, min = 1985, max = 2005) #msmsploc
  d9 = runif(n, 1/40, 1/5) #import
  d10 = runif(n, 5, 1000) #srcNe
  d11 = rbeta(n, shape1 = 16, shape2 = 4) #pmsm2msm
  d12 = rbeta(n, shape1 = 16, shape2 = 4) #pgpf2gpm
  d13 = runif(n, 1, 3) #initmsm
  d14 = runif(n, 1, 3) #initgp 

  return(cbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14))
}



# Create prior (necessary for the BayesianTools package)
prior <- createPrior(density = densities,
                     sampler = sampler,
                     lower = c(0.05, 0.05, 0.05, 1978, 0.05, 0.05, 0.05, 1978, 0, 1., 0, 0, 1, 1), 
                     upper = c(1, 1, 1, 2014, 1, 1, 1, 2014, 0.30, 1000, 1, 1, 5, 5))





#~ mll <- colik(tree = dated.tree,
#~                theta = THETA.new,
#~                demographic.process.model = dm,
#~                x0 = X0,
#~                t0 = 1978,
#~                res = 1e3,
#~                timeOfOriginBoundaryCondition = FALSE,
#~                forgiveAgtY = FALSE, 
#~                AgtY_penalty = 1,
#~                maxHeight = 35)



theta0 <- c(   THETA$gpsp0
  ,THETA$gpsp1
  ,THETA$gpsp2
  ,THETA$gpsploc
  ,THETA$msmsp0 
  ,THETA$msmsp1 
  ,THETA$msmsp2 
  ,THETA$msmsploc 
  ,THETA$import 
  ,THETA$srcNe 
  ,THETA$pmsm2msm 
  ,THETA$pgpf2gpm 
  ,THETA$initmsm 
  ,THETA$initgp
)

of2 <- function(x)
{
	rv <- obj_fun(x) + prior$density( x )
	print( c(rv, x ))
	rv 
}



require ( lhs )
require(parallel)
NCPU <- 40 
#~ starts <- improvedLHS( NCPU , length(theta0 ))
#~ lower = c(0.05, 0.05, 0.05, 1978, 0.05, 0.05, 0.05, 1978, 0, 0.0001, 0, 0, 1, 1)
#~ upper = c(1, 1, 1, 2014, 1, 1, 1, 2014, 0.30, 0.30, 1, 1, 300, 300)
#~ for (k in 1:ncol(starts )){
#~ 	starts[,k] <- lower[k] + (upper[k] - lower[k]) * starts[, k]
#~ }
pnames <- c( 'gpsp0', 'gpsp1', 'gpsp2', 'gpsploc', 'msmsp0', 'msmsp1', 'msmsp2', 'msmsploc', 'import', 
	'srcNe', 'pmsm2msm', 'pgpf2gpm', 'initmsm', 'initgp' )
starts <- c()
for (i in 1:NCPU){
	ofv <- -Inf 
	while ( is.infinite( ofv )){
		theta <- sampler()[1, ] 
		ofv <- of2( theta )
		print( c( ofv, theta ))
	}
	starts <- rbind( starts, theta )
}
colnames(starts) <- pnames 
parscale <- sapply( 1:ncol(starts), function(k) diff( range( starts[,k])) )

#~ mclapply( 1:NCPU, function(i ) of2( starts[i,] ) , mc.cores = NCPU )

fits <- mclapply( 1:NCPU, function(i){
	optim( starts[i,] , fn = of2, method = 'Nelder-Mead',
	 , control = list(fnscale=-1, maxit = 300, trace = 6, parscale = parscale)
	)
}, mc.cores = NCPU )

fits1 <- fits [ !(sapply(fits, function(f) class(f)=='try-error') ) ]
theta1 <- sapply( fits1, '[[', 'par' )
rownames(theta1) <- pnames
logpos1 <- sapply( fits1, '[[', 'value' )
fit.star <- fits1[[ which.max( logpos1 ) ]] 
theta.star <- theta1[ , which.max( logpos1 ) ]

theta2 <- theta1[ , logpos1 < 0]
logpos2 <- logpos1[ logpos1 < 0]
theta.star2 <- theta2[ , which.max( logpos2 ) ]

save.image('test2.rda' )

ix <- order( logpos2 )
print( rbind( theta2, logpos2 )[, ix])


# plot trajectores, paf 
if (F){
	#  
	theta.star2 <- theta2[ , ix[40]  ]
	parameters = theta.star2
  # add the values of THETA to a new variable named THETA.new
  THETA.new <- THETA

  # change the values in THETA.new to the new proposals that will be evaluated
  THETA.new$gpsp0 <- parameters[1]
  THETA.new$gpsp1 <- parameters[2]
  THETA.new$gpsp2 <- parameters[3]
  THETA.new$gpsploc <- parameters[4]
  THETA.new$msmsp0 <- parameters[5]
  THETA.new$msmsp1 <- parameters[6]
  THETA.new$msmsp2 <- parameters[7]
  THETA.new$msmsploc <- parameters[8]
  THETA.new$import <- parameters[9]
  THETA.new$srcNe <- parameters[10]
  THETA.new$pmsm2msm <- parameters[11]
  THETA.new$pgpf2gpm <- parameters[12]
  THETA.new$initmsm <- parameters[13]
  THETA.new$initgp <- parameters[14]

  # X0 is the initial conditions for the 4 demes (gpf, gpm, msm, src)
  X0 <- c(gpm = unname(THETA.new$initgp/2),
          gpf = unname(THETA.new$initgp/2),
          msm = unname(THETA.new$initmsm) ,
          src = 1e5)	
  tfgy <- dm( THETA.new, x0 = X0 , t0 = 1978, t1 = 2014, integrationMethod='lsoda' )
  y <- tfgy[[5]]
 
 print( cbind( THETA.new, THETA ))
 
 f <- tfgy[[2]][[1]]
  paf <- rowSums( f)[1:3] ; paf <- paf  /sum(paf )
  print(paf)

matplot( y[,1] , y[,2:4], type = 'l' )
 
}
