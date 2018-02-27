# reads the phylogenetic tree that will be used in the coalescent analysis
# this tree has the tips from children or whil information of the state (demes)
# were missing
tree.all <- read.tree("data/bindTree_CGR_GTR+Gp12+3_droppedTip.tre")

# Reads metadata that contain info for the CGR (close global reference) sequences
# CGRs are referred in the mathematical model as src (source) data
# These are HIV sequences that are from other countries and not from Senegal
all.data.cgr <- read.csv("data/HIV_subtypes_summary_CGR.csv")

# Reads all metadata for HIV sequences from Senegal
all.data.SN <- read.csv("data/HIV_subtypes_summary_SENEGAL_noDups.csv")

# Read all metadata that needs to be removed because information is missing
# These are information related to the deme, for example, some sequences we don't
# have information whether it is a male or female from the general population
to.remove <- read.csv("data/HIV_subtypes_SENEGAL_noDups_ToDrop_fromAnalysis.csv")

# organize metadata in 2 columns.
# the first column is the sequences name as in the phylogenetic tree
# the second colum is the state (gpf, gpm, msm, or src) of each sequences
all_data <- organize_metadata(all.data.cgr, all.data.SN, to.remove, tree.all)

#create matrix to receive the information on states for each tip of the tree
gpm <- gpf <- msm <- src <- rep(0, length(tree.all$tip.label))

gpm[all_data$States == "gpm"] <- 1
gpf[all_data$States == "gpf"] <- 1
msm[all_data$States == "msm"] <- 1
src[all_data$States == "src"] <- 1

sampleStates <- cbind(gpm, gpf, msm, src)
rownames(sampleStates) <- all_data$tip.name

#read estimated times (in calendar units) for each sequence in the phylogenetic tree
times <- readRDS("data/bindTree_CGR_GTR+Gp12+3_droppedTip_sts.RDS")

# create an object of DatedTree [phydynR package]
# This is the tree that should be used in the calculation of the likelihood
# to estimate parameter values
dated.tree <- DatedTree(phylo = tree.all, sampleTimes = times, sampleStates = sampleStates, minEdgeLength = 2/52, tol = .1)
