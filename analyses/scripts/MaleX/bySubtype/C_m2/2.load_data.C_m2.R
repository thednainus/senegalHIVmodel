invisible('
exclude all non-dakar
')

library(ape)

# Reads the phylogenetic tree that will be used in the coalescent analysis.
# This tree had removed the tips from children and tips that risk group = NA or and sex = NA
tree.C <- readRDS(system.file("data/trees_by_subtype/dtr.C.CGR.GTR_Gp12+3_byCodon.RDS", package = "senegalHIVmodel"))

# Reads metadata that contain info for the CGR (close global reference) sequences
# CGRs are referred in the mathematical model as src (source) data
# These are HIV sequences that are from other countries and not from Senegal
all.data.cgr <- read.csv(system.file("data/HIV_subtypes_summary_CGR.csv", package = "senegalHIVmodel"))

# Reads all metadata for HIV sequences from Senegal
all.data.SN <- read.csv(system.file("data/HIV_subtypes_summary_SENEGAL_noDups.csv", package = "senegalHIVmodel"))

# Sequences that should be removed from tree because it is from Children,
# or if risk group or sex is NA.
to_remove <- subset(all.data.SN,
                (is.na(Sex) | Risk_group == "Children" |is.na(Risk_group)) &
                  Subtype == "C")
to_remove["tip"] <- paste(to_remove$Accession_number, to_remove$Subtype,
                          "SN", to_remove$Year, sep = ".")

# This is the tree to be used in the phylodynamic analysis
# Sequences from Children or Risk_group = NA or Sex = NA was removed from the tree
tree.C.tipDropped <- drop.tip(tree.C, to_remove$tip)

# Remove the dropped tip estimated dates from the all_times vector
all_times <- tree.C$sts

#tip ages that should be removed because it was removed from the tree
the_names = c(to_remove$tip)

to.remove = which(names(all_times) %in% the_names)
new.times <- all_times[-c(to.remove)]


# organize metadata in 2 columns.
# the first column is the sequence names
# the second colum is the state (gpf, gpm, msm, or src) of each sequences
# Does not include sequences from Children, or that Risk Group or Sex have not
# been reported
all_data <- organize_metadata(all.data.cgr, all.data.SN)

all.C <- all_data[match(tree.C.tipDropped$tip.label,all_data$tip.name),]


#create matrix to receive the information on states for each tip of the tree
gpm <- gpf <- msm <- src <- rep(0, length(tree.C.tipDropped$tip.label))

gpm[all.C$States == "gpm"] <- 0.5
gpf[all.C$States == "gpf"] <- 1
msm[all.C$States == "msm"] <- 1
msm[all.C$States == "gpm"] <- 0.5
src[all.C$States == "src"] <- 1

sampleStates <- cbind(gpm, gpf, msm, src)
rownames(sampleStates) <- all.C$tip.name


# create an object of DatedTree [phydynR package]
# This is the tree that should be used in the calculation of the likelihood
# to estimate parameter values
# In this example I am using all sequences from Senegal
dated.treeC <- DatedTree(phylo = tree.C.tipDropped,
                             sampleTimes = new.times,
                             sampleStates = sampleStates,
                             minEdgeLength = 2/52,
                             tol = 0.1)


