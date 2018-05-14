library(ape)

# Reads the phylogenetic tree that will be used in the coalescent analysis.
# This tree had removed the tips from children and tips that risk group = NA or and sex = NA
tree.all <- read.tree(system.file("data/bindTree_CGR_GTR+Gp12+3_droppedTip.tre",
                                  package = "senegalHIVmodel"))

# Reads metadata that contain info for the CGR (close global reference) sequences
# CGRs are referred in the mathematical model as src (source) data
# These are HIV sequences that are from other countries and not from Senegal
all.data.cgr <- read.csv(system.file("data/HIV_subtypes_summary_CGR.csv",
                                     package = "senegalHIVmodel"))

# Reads all metadata for HIV sequences from Senegal
all.data.SN <- read.csv(system.file("data/HIV_subtypes_summary_SENEGAL_noDups.csv",
                                    package = "senegalHIVmodel"))

# organize metadata in 2 columns.
# the first column is the sequence names
# the second colum is the state (gpf, gpm, msm, or src) of each sequences
organize_metadata <- function(metadata_CGR, metadata_SN){

  # Organize metadata for CGR sequences
  metadata_CGR["tip.name"] <- paste(metadata_CGR$Accession_number, "CGR", sep='_')
  metadata_CGR.2 <- metadata_CGR[c("tip.name", "Risk_group")]
  metadata_CGR.2["Sex"] <- NA
  metadata_CGR.2["States"] <- "src"
  metadata_CGR.3 <- metadata_CGR.2[c("tip.name", "States")]

  # Organize metadata for the Senegal sequences
  # It creates a dataframe that will not have metadata with missing information
  metadata_SN$Risk_group <- as.character(metadata_SN$Risk_group)
  metadata_SN$Sex <- as.character(metadata_SN$Sex)


  # Read all metadata that necessary information is missing
  # These are information related to the demes of our model, for example, some sequences we don't
  # have information whether it is a male or female from the general population, or they are from Children.
  metadata_SN.2 <- subset(metadata_SN, is.na(Risk_group) == FALSE &
                            Risk_group != "Children" &
                            is.na(Sex) == FALSE)
  metadata_SN.2["tip.name"] <- paste(metadata_SN.2$Accession_number,
                                     metadata_SN.2$Subtype, "SN",
                                     metadata_SN.2$Year, sep='.')
  metadata_SN.3 <- metadata_SN.2[c("tip.name", "Risk_group", "Sex")]
  metadata_SN.3["States"] <- tolower(paste(metadata_SN.3$Risk_group,
                                           metadata_SN.3$Sex, sep = ""))
  metadata_SN.3$States[metadata_SN.3$Risk_group == "MSM"] <- "msm"
  metadata_SN.4 <- metadata_SN.3[c("tip.name", "States")]

  # bind together both dataframes (the one containing metadata on Senegal samples,
  # and the other containing metadata on CGR sequences)
  all_data <- rbind(metadata_CGR.3, metadata_SN.4)
  all_data$States <- as.factor(all_data$States)

  return(all_data)

}

all_data <- organize_metadata(all.data.cgr, all.data.SN)

#create matrix to receive the information on states for each tip of the tree
gpm <- gpf <- msm <- src <- rep(0, length(tree.all$tip.label))

gpm[all_data$States == "gpm"] <- 1
gpf[all_data$States == "gpf"] <- 1
msm[all_data$States == "msm"] <- 1
src[all_data$States == "src"] <- 1

sampleStates <- cbind(gpm, gpf, msm, src)
rownames(sampleStates) <- all_data$tip.name

#read estimated times (in calendar units) for each sequence in the phylogenetic tree
times <- readRDS(system.file("data/bindTree_CGR_GTR+Gp12+3_droppedTip_sts.RDS",
                             package = "senegalHIVmodel"))

# create an object of DatedTree [phydynR package]
# This is the tree that should be used in the calculation of the likelihood
# to estimate parameter values
dated.tree <- DatedTree(phylo = tree.all,
                        sampleTimes = times,
                        sampleStates = sampleStates,
                        minEdgeLength = 2/52,
                        tol = 0.1)
