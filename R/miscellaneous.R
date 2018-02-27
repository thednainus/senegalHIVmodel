#' Setup Model Equations
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


#' Organize HIV metadata
#'
#' This function aims to organize the HIV metadata in a format that will be more
#' useful for the analysis of HIV transmission in Senegal using coalescent model.
#' It mainly gets information on different dataframes format and organize them so as to
#' create a matrix for the states (demes) on the phylogenetic tree
#'
#' @param metadata_CGR data.frame object that contains metadata for the CGR sequences.
#'    These sequeces will receive the state "src", a deme in our mathematical model.
#' @param metadata_SN data.frame object that contain all metadata for the Senegal (SN)
#'    sequences. Sequences will receive the states: "gpf", "gpm", or "msm".
#' @param to_remove data.frame object that contain all metadata that must be removed
#'    from analysis because there are missing information
#' @param tree phylogenetic tree in the class phylo in which the tips correspond to
#'    all sequences with metadata
#'
#' @return a data.frame object containing 2 columns. The first column reprsent the
#'    the sequences name (as in the phylogenetic tree), and second column represent
#'    the state of the sequences. In our case, state can be any of the demes (gpf, gpm,
#'    msm, and src).
#'
#' @export
#'
#' @examples
#' tree.all <- read.tree("data/bindTree_CGR_GTR+Gp12+3_droppedTip.tre")
#' all.data.cgr <- read.csv("data/HIV_subtypes_summary_CGR.csv")
#' all.data.SN <- read.csv("data/HIV_subtypes_summary_SENEGAL_noDups.csv")
#' to.remove <- read.csv("data/HIV_subtypes_SENEGAL_noDups_ToDrop_fromAnalysis.csv")
#'
#' all_data <- organize_metadata(all.data.cgr, all.data.SN, to.remove, tree.all)
organize_metadata <- function(metadata_CGR, metadata_SN, to_remove, tree){

  # Organize metadata for CGR sequences
  metadata_CGR["tip.name"] <- paste(metadata_CGR$Accession_number, "CGR", sep='_')
  metadata_CGR.2 <- metadata_CGR[c("tip.name", "Risk_group")]
  metadata_CGR.2["Sex"] <- NA
  metadata_CGR.2["States"] <- "src"
  metadata_CGR.3 <- metadata_CGR.2[c("tip.name", "States")]

  # Organize metadata for the Senegal sequences
  # It creates a dataframe that will not have metadata with missing information
  metadata_SN.2 <- metadata_SN[ ! metadata_SN$Accession_number %in% to_remove$Accession_number, ]
  metadata_SN.2["tip.name"] <- paste(metadata_SN.2$Accession_number, metadata_SN.2$Subtype, "SN", metadata_SN.2$Year, sep='.')
  metadata_SN.3 <- metadata_SN.2[c("tip.name", "Risk_group", "Sex")]
  metadata_SN.3["States"] <- tolower(paste(metadata_SN.3$Risk_group, metadata_SN.3$Sex, sep = ""))
  metadata_SN.3$States[metadata_SN.3$Risk_group == "MSM"] <- "msm"
  metadata_SN.4 <- metadata_SN.3[c("tip.name", "States")]

  # bind together both dataframes (the one containing metadata on Senegal samples,
  # and the other containing metadata on CGR sequences)
  all_data <- rbind(metadata_CGR.3, metadata_SN.4)
  # Match the information on the tip of the tree with the dataframe containing information
  # on all data
  all_data.2 <- all_data[match(tree$tip.label, all_data$tip.name),]
  all_data.2$States <- as.factor(all_data.2$States)

  return(all_data.2)

}
