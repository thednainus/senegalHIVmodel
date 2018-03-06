#' Object function
#'
#' This object function will receive the proposals of the mcmc.
#' The reason of using an object function is to make it easier to change the
#' values of the parameters to be estimated in THETA.
#' Note that not all parameters listed in THETA will be estimated
#'
#' @param parameters is a vector or matrix containing the proposals o f the mcmc
#'
#' @return the likelihood of the model based on the new parameter values
#'    (proposals of the mcmc)
#'
#' @details This object function uses a series of global variables that are declared
#'    in \code{\link{"analysis/scripts/model.R"}} and \code{\link{"analysis/scripts/load_data.R"}}
#'    These global variables are:
#'    \itemize{
#'    \item \code{THETA} is a list with parameter values
#'    \item \code{dated.tree} which is of class \code{\link[phydynR]{DatedTree}}
#'    \item \code{dm} the demographic model constructed with \code{\link[phydynR]{build.demographic.process}}
#'     \item \code{X0} vector that contains the initial conditions for the demes in our model
#'    }
#'
#' @export
#'
#' @examples
#' #In our example we would like to estimate 12 parameters. the parameter maleX will be fixed
#' # and the value for maleX is also changed in this obj_fun
#'
#' lnl <- obj_fun(c(0.6, 0.4, 0.1, 1987, 0.4, 0.4, 0.2, 1995, 0.05, 0.1, 0.85, 0.85))
#'
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

  # maleX parameter will be fixed to 2.0
  THETA.new$maleX <- 2.0

  # After changing the parameter values to the new proposals, a likelihood is
  # calculated with the funtion colik.
  # Note that this function uses several global variables, such as, dated.tree, dm, and X0
  mll <- colik(tree = dated.tree,
               theta = THETA.new,
               demographic.process.model = dm,
               x0 = X0,
               t0 = 1978,
               res = 1e3,
               timeOfOriginBoundaryCondition = FALSE,
               AgtY_penalty = 1,
               maxHeight = 41)

  return(mll)

}
