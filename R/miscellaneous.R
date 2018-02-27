#' Setup model equations
#'
#' Function to setup the components of the mathematical model. See \code{\link[phydynR]{build.demographic.process}}
#' for more details
#'
#' @param demes a character vector naming the demes of the mathematical model.
#' @param nondemes a character vector naming the non demes of the mathematical model.
#' @param rcpp if TRUE, the expressions are interpreted as C code using the Rcpp package.
#'
#' @return this function returns a list containing the empty components (represented by zeros) to build the mathematical model.
#'  These components are the birth, death, migrations, total number of demes and non-demes of the model.
#'  \itemize{
#'      \item Birth is a matrix describing the model birth rates;
#'      \item Death is a vector describing the model death rates;
#'      \item Migration is a matrix describing the model migration rates.
#'  }
#'
#'
#' @seealso \code{\link[phydynR]{build.demographic.process}}
#'
#' @export
#'
#' @examples
#' demes <- c('gpm', 'gpf', 'msm', 'src')
#' eqns <- setup.model.equations(demes)
setup.model.equations <- function(demes, nondemes = NULL, rcpp = FALSE)
{
  m <- length(demes)
  mm <- length(nondemes)
  b <- matrix('0.', nrow = m, ncol = m)
  migs <- matrix('0.', nrow = m, ncol = m)

  rownames(b) = rownames(migs) = colnames(b) = colnames(migs) <- demes

  dths <- setNames(rep('0.', m ), demes)

  ndd <- setNames(rep('0.', mm ), nondemes)

  list(births = b, migs = migs, deaths = dths,
       nonDemeDynamics = ndd, m = m, mm = mm ,
       demes = demes, nondemes = nondemes)
}
