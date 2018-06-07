invisible('
exclude all non-dakar
')

library(ape)

# Reads the phylogenetic tree that will be used in the coalescent analysis.
# This tree had removed the tips from children and tips that risk group = NA or and sex = NA
tree.all <- read.tree(system.file("data/bindTree_CGR_GTR+Gp12+3_droppedTip.tre", package = "senegalHIVmodel"))

# Reads metadata that contain info for the CGR (close global reference) sequences
# CGRs are referred in the mathematical model as src (source) data
# These are HIV sequences that are from other countries and not from Senegal
all.data.cgr <- read.csv(system.file("data/HIV_subtypes_summary_CGR.csv", package = "senegalHIVmodel"))

# Reads all metadata for HIV sequences from Senegal
all.data.SN <- read.csv(system.file("data/HIV_subtypes_summary_SENEGAL_noDups.csv", package = "senegalHIVmodel"))

# organize metadata in 2 columns.
# the first column is the sequence names
# the second colum is the state (gpf, gpm, msm, or src) of each sequences
all_data <- organize_metadata(all.data.cgr, all.data.SN)

#create matrix to receive the information on states for each tip of the tree
gpm <- gpf <- msm <- src <- rep(0, length(tree.all$tip.label))

gpm[all_data$States == "gpm"] <- 0.5
gpf[all_data$States == "gpf"] <- 1
msm[all_data$States == "msm"] <- 1
msm[all_data$States == "gpm"] <- 0.5
src[all_data$States == "src"] <- 1

sampleStates <- cbind(gpm, gpf, msm, src)
rownames(sampleStates) <- all_data$tip.name

#read estimated times (in calendar units) for each sequence in the phylogenetic tree
times <- readRDS(system.file("data/bindTree_CGR_GTR+Gp12+3_droppedTip_sts.RDS", package = "senegalHIVmodel"))

# create an object of DatedTree [phydynR package]
# This is the tree that should be used in the calculation of the likelihood
# to estimate parameter values
dated.tree <- DatedTree(phylo = tree.all,
                        sampleTimes = times,
                        sampleStates = sampleStates,
                        minEdgeLength = 2/52,
                        tol = 0.1)


metadata <- read.csv(system.file("data/HIV_subtypes_summary_SENEGAL_noDups.csv",
                                 package = "senegalHIVmodel"))
tip2isdakar <- function(tip)
{
	if ( ! grepl('SN' , tip )) return(FALSE)
	ano <- strsplit( tip, '\\.') [[1]][1]
	loc  = metadata$Location[ metadata$Accession_number == ano ]
	if ( length( loc ) == 0 ) return(FALSE)
	if (tolower(loc)=='dakar') return(TRUE)
	return(FALSE)
}

tip2isSenegalNotDakar <- function(tip)
{
	if ( ! grepl('SN' , tip )) return(FALSE)
	ano <- strsplit( tip, '\\.') [[1]][1]
	loc  = metadata$Location[ metadata$Accession_number == ano ]
	if ( length( loc ) == 0 ) return(FALSE)
	if (tolower(loc)!='dakar') return(TRUE)
	return(FALSE)
}

isdakar <- sapply( dated.tree$tip, tip2isdakar )
notdakar <- sapply( dated.tree$tip, tip2isSenegalNotDakar)

tree.dakar <- drop.tip( tree.all , dated.tree$tip.label[ notdakar] )
dated.tree.dakar <- DatedTree(phylo = tree.dakar,
                        sampleTimes = times[ tree.dakar$tip.label ],
                        sampleStates = sampleStates[tree.dakar$tip.label, ],
                        minEdgeLength = 2/52,
                        tol = 0.1)


# plots
if (FALSE)
{
	ecols <- rep('black', nrow(dated.tree$edge))
	ecols <- sapply( 1:nrow(dated.tree$edge), function(iedge){
		v <- dated.tree$edge[iedge,2]
		if ( v > length( isdakar)) return('black')
		if (isdakar[v])  return('red')
		return('blue')
	})
	quartz(); plot( dated.tree, edge.color = ecols, show.tip.lab = FALSE, no.mar=TRUE )

	ismsm <- dated.tree$sampleStates[, 'msm']==1
	ecols2 <- sapply( 1:nrow(dated.tree$edge), function(iedge){
		v <- dated.tree$edge[iedge,2]
		if ( v > length( ismsm)) return('black')
		if (ismsm[v])  return('red')
		return('black')
	})
	plot( dated.tree, edge.color = ecols2, show.tip.lab = TRUE, no.mar=TRUE, cex = 0.5)
}
