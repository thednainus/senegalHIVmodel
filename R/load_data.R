# reads the phylogenetic tree that will be used in the coalescent analysis
# this tree has the tips from children or whil information of the state (demes)
# were missing
tree.all <- read.tree("data/bindTree_CGR_GTR+Gp12+3_droppedTip.tre")

all.data.cgr <- read.csv("data/HIV_subtypes_summary_CGR.csv")
all.data.cgr["tip.name"] <- paste(all.data.cgr$Accession_number, "CGR", sep='_')
all.data.cgr2 <- all.data.cgr[c("tip.name", "Risk_group")]
all.data.cgr2["Sex"] <- NA
all.data.cgr2["States"] <- "src"
all.data.cgr3 <- all.data.cgr2[c("tip.name", "States")]



all.data.SN <- read.csv("data/HIV_subtypes_summary_SENEGAL_noDups.csv")
#remove tips that are not in the tree
#to.remove is a dataframe that contains info on sequences that should be dropped from analysis
to.remove <- read.csv("data/HIV_subtypes_SENEGAL_noDups_ToDrop_fromAnalysis.csv")
all.data.SN.2 <- all.data.SN[ ! all.data.SN$Accession_number %in% to.remove$Accession_number, ]


all.data.SN.2["tip.name"] <- paste(all.data.SN.2$Accession_number, all.data.SN.2$Subtype, "SN", all.data.SN.2$Year, sep='.')
all.data.SN.3 <- all.data.SN.2[c("tip.name", "Risk_group", "Sex")]


all.data.SN.3["States"] <- tolower(paste(all.data.SN.3$Risk_group, all.data.SN.3$Sex, sep = ""))
all.data.SN.3$States[all.data.SN.3$Risk_group == "MSM"] <- "msm"
all.data.SN.4 <- all.data.SN.3[c("tip.name", "States")]


# bind together both dataframes (the one containing info on Senegal samples, and
# the other containing info on CGR sequences)
all.data <- rbind(all.data.cgr3, all.data.SN.4)
all.data.2 <- all.data[match(tree.all$tip.label, all.data$tip.name),]
#all.data.2 <- all.data[match(tr3$tip.label, all.data$tip.name),]


all.data.2$States <- as.factor(all.data.2$States)

#create matrix to receive the information on states for each tip of the tree
gpm <- gpf <- msm <- src <- rep(0, length(tree.all$tip.label))
#gpm <- gpf <- msm <- src <- rep(0, length(tr3$tip.label))

gpm[all.data.2$States == "gpm"] <- 1
gpf[all.data.2$States == "gpf"] <- 1
msm[all.data.2$States == "msm"] <- 1
src[all.data.2$States == "src"] <- 1

sampleStates <- cbind(gpm, gpf, msm, src)
rownames(sampleStates) <- all.data.2$tip.name


#DatedTree function (phylo, sampleTimes, sampleStates = NULL, sampleStatesAnnotations = NULL,
#          tol = 1e-06, minEdgeLength = 0)

times <- readRDS("data/bindTree_CGR_GTR+Gp12+3_droppedTip_sts.RDS")
#times.ordered <- times[match(tree.all$tip.label, names(times))]


dated.tree <- DatedTree(phylo = tree.all, sampleTimes = times, sampleStates = sampleStates, minEdgeLength = 2/52, tol = .1)
