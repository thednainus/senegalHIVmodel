# Function to read csv file containing metadata and format it in a way to be
# used as input data for treedater
library(treedater)
library(lubridate)
library(phytools)


# Plot lineage through time with confidence interval
# (below is the function plot.parboot.ltt) using parametric bootstrap
parboot.ltt <- function(pbtd)
{
  t0 <-  NA
  res <- 100
  t1 <- max(pbtd$td$sts, na.rm = T)
  if (is.na(t0))
    t0 <- min(sapply(pbtd$trees, function(tr) tr$timeOf))
  times <- seq(t0, t1, l = res)
  ltt <- cbind(times = times, t(sapply(times, function(t) {
    c(pml = sum(pbtd$td$sts > t) - sum(pbtd$td$Ti > t),
      setNames(quantile(sapply(pbtd$trees,
                               function(tre) sum(tre$sts > t) - sum(tre$Ti > t)),
                        probs = c(0.025, 0.5, 0.975)), c("lb", "median", "ub")))
  })))
  pl.df <- as.data.frame(ltt)
  return(pl.df)

}


#### non parametric bootstrap LTT (lineages through time) ###########
boot.ltt <- function (pbtd)
{
  t0 <-  NA
  res <- 100
  t1 <- max(pbtd$td$sts, na.rm = T)
  if (is.na(t0))
    t0 <- min(sapply(pbtd$trees, function(tr) tr$timeOf))
  times <- seq(t0, t1, l = res)
  ltt <- cbind(times = times, t(sapply(times, function(t) {
    c(pml = sum(pbtd$td$sts > t) - sum(pbtd$td$Ti > t),
      setNames(quantile(sapply(pbtd$trees,
                               function(tre) sum(tre$sts > t) - sum(tre$Ti > t)),
                        probs = c(0.025, 0.5, 0.975)), c("lb", "median", "ub")))
  })))
  pl.df <- as.data.frame(ltt)
  return(pl.df)
}

# Function to re-root bootstrap trees and drop outgroup tips
# (after re-rooting trees)
root_and_drop_tips <- function(tree, outgroups){
  node <- getMRCA(tree, outgroups)
  res <- try(reroot(tree, node.number=node, edgelabel = TRUE), silent = TRUE)

  if(class(res) == "try-error"){
    tree <- midpoint.root(tree)
    node <- getMRCA(tree, outgroups)
    tree.r <- reroot(tree, node.number=node, edgelabel = TRUE)
    tree.r.new <- drop.tip(tree.r, tip=outgroups)
  }else{
    tree.r <- reroot(tree, node.number=node, edgelabel = TRUE)
    tree.r.new <- drop.tip(tree.r, tip=outgroups)
  }
}


################################################################################

setwd("~/Box Sync/Senegal/HIV_myDesktop/sn-data/")

# Metadata for Senegal only and close global reference (CGR)
SN.data <- read.csv("HIV_subtypes_summary_SENEGAL_noDups.csv")
CGR.data <-  read.csv("HIV_subtypes_summary_CGR.csv")

# Metadata for Senegal sequences
B.SN <- subset(SN.data, Subtype == "B")
C.SN <- subset(SN.data, Subtype == "C")
AG.SN <- subset(SN.data, Subtype == "02_AG")

# Metadata for CGR sequences
B.CGR <- subset(CGR.data, Subtype == "B")
C.CGR <- subset(CGR.data, Subtype == "C")
AG.CGR <- subset(CGR.data, Subtype == "02_AG")


################################################################################
## Subtype B

B.SN.data <- data_format(B.SN, "SN")
B.SN.df <- B.SN.data[[1]]
B.missing_sample <- B.SN.data[[2]]

B.CGR.data <- data_format(B.CGR, "CGR")
B.CGR.df <- B.CGR.data[[1]]
B.CGR.missing_sample <- B.CGR.data[[2]]


#Dataframe to be used in treedater for the missing sample date
B.missing_sample.df <- rbind(B.missing_sample[c("tip","lower","upper")],
                             B.CGR.missing_sample[c("tip","lower","upper")])
B.missing_sample.df.2 <- B.missing_sample.df[(c("lower","upper"))]
row.names(B.missing_sample.df.2) <- B.missing_sample.df[,1]


# chech which sequence is not present in the phylogeny but present in the
# database
B.df <- rbind(B.SN.df[c("Accession_number","tip","decimal")],
              B.CGR.df[c("Accession_number","tip","decimal")])
#B.df[!B.df$tip %in% tB.CGR.GTR_G$tip.label,]
#B.df2 <- subset(B.df, Accession_number != "AJ583740" &
#                  Accession_number != "FN599677" &
#                  Accession_number != "LN680713")

B.df$decimal <- as.character(B.df$decimal)
B.df$decimal <- as.numeric(B.df$decimal)

##### READING TREES for SUBTYPE B##########
# DNA substitution model: GTR+G
(tB.CGR.GTR_G <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/B/additional_data/raxml-ng/no_duplicated/CGR/by_codon/B_CGR_GTR_G/ML_0_GTR+G.raxml.support.tre"))
quartz()
plot(tB.CGR.GTR_G)
nodelabels()
(tB.CGR.GTR_G.r <- reroot(tB.CGR.GTR_G, node.number=84, edgelabel = TRUE))
plot(tB.CGR.GTR_G.r)
(tB.CGR.GTR_G.r.new <- drop.tip(tB.CGR.GTR_G.r,
                                tip=c("Ref.D.CM.01.01CM_4412HAL.AY371157",
                                      "Ref.D.CD.83.ELI.K03454",
                                      "Ref.D.TZ.01.A280.AY253311")))
plot(ladderize(tB.CGR.GTR_G.r.new, right = FALSE), show.node.label = TRUE)

# DNA substitution model: GTR+G partion by codon position 12+3
(tB.CGR.GTR_Gp <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/B/additional_data/raxml-ng/no_duplicated/CGR/by_codon/B_CGR_GTR_Gp/ML_0_GTR+Gp.raxml.support.tre"))
quartz()
plot(tB.CGR.GTR_Gp)
nodelabels()
(tB.CGR.GTR_Gp.r <- reroot(tB.CGR.GTR_Gp, node.number=95, edgelabel = TRUE))
plot(tB.CGR.GTR_Gp.r)
(tB.CGR.GTR_Gp.r.new <- drop.tip(tB.CGR.GTR_Gp.r,
                                 tip=c("Ref.D.CM.01.01CM_4412HAL.AY371157",
                                       "Ref.D.CD.83.ELI.K03454",
                                       "Ref.D.TZ.01.A280.AY253311")))
plot(ladderize(tB.CGR.GTR_Gp.r.new, right = FALSE), show.node.label = TRUE)

# DNA substitution model: GTR+R3
(tB.CGR.GTR_R3 <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/B/additional_data/raxml-ng/no_duplicated/CGR/by_codon/B_CGR_GTR_R/ML_0_GTR+R3.raxml.support.tre"))
quartz()
plot(tB.CGR.GTR_R3)
nodelabels()
(tB.CGR.GTR_R3.r <- reroot(tB.CGR.GTR_R3, node.number=90, edgelabel = TRUE))
plot(tB.CGR.GTR_R3.r)
(tB.CGR.GTR_R3.r.new <- drop.tip(tB.CGR.GTR_R3.r, tip=c("Ref.D.CM.01.01CM_4412HAL.AY371157", "Ref.D.CD.83.ELI.K03454", "Ref.D.TZ.01.A280.AY253311")))
plot(ladderize(tB.CGR.GTR_R3.r.new, right = FALSE))


# DNA substitution model: GTR+I+G
(tB.CGR.GTR_I_G <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/B/additional_data/raxml-ng/no_duplicated/CGR/by_codon/B_CGR_GTR_I_G/ML_0_CGR_GTR+I+G.raxml.support.tre"))
quartz()
plot(tB.CGR.GTR_I_G)
nodelabels()
(tB.CGR.GTR_I_G.r <- reroot(tB.CGR.GTR_I_G, node.number=87, edgelabel = TRUE))
plot(tB.CGR.GTR_I_G.r)
(tB.CGR.GTR_I_G.r.new <- drop.tip(tB.CGR.GTR_I_G.r, tip=c("Ref.D.CM.01.01CM_4412HAL.AY371157", "Ref.D.CD.83.ELI.K03454", "Ref.D.TZ.01.A280.AY253311")))
plot(ladderize(tB.CGR.GTR_I_G.r.new, right = FALSE), show.node.label = TRUE)


################################################################################
## TREEDATER ANALYSES


## SUBTYPE B

B.seqlen <- 1302 # the length of the HIV sequences

B.sts <- B.df$decimal
names(B.sts) <- B.df$tip

(dtr.B.GTR_G <- dater(tB.CGR.GTR_G.r.new, B.sts, B.seqlen,
                      estimateSampleTimes = B.missing_sample.df.2))
(dtr.B.GTR_Gp <- dater(tB.CGR.GTR_Gp.r.new, B.sts, B.seqlen,
                       estimateSampleTimes = B.missing_sample.df.2))
(dtr.B.GTR_I_G <- dater(tB.CGR.GTR_I_G.r.new, B.sts, B.seqlen,
                        estimateSampleTimes = B.missing_sample.df.2))
(dtr.B.GTR_R3 <- dater(tB.CGR.GTR_R3.r.new, B.sts, B.seqlen,
                       estimateSampleTimes = B.missing_sample.df.2))


# saving as RDS file
#path_dir="~/Box Sync/treedater_R_files/"
#saveRDS(dtr.B.GTR_G, paste(path_dir, "dtr.B.CGR.GTR_G_byCodon.RDS", sep=""))
#saveRDS(dtr.B.GTR_Gp, paste(path_dir, "dtr.B.CGR.GTR_Gp12+3_byCodon.RDS", sep=""))
#saveRDS(dtr.B.GTR_I_G, paste(path_dir, "dtr.B.CGR.GTR_I_G_byCodon.RDS", sep=""))
#saveRDS(dtr.B.GTR_R3, paste(path_dir, "dtr.B.CGR.GTR_R3_byCodon.RDS", sep=""))


######################################################
#postorder trees to plot times vs bootstrap support ##
######################################################

po_B_CGR.GTR_G <- reorder(tB.CGR.GTR_G.r.new, order = "postorder")
po_B_CGR.GTR_R3 <- reorder(tB.CGR.GTR_R3.r.new, order = "postorder")

po_dtr.B.CGR.GTR_G <- reorder(dtr.B.GTR_G, order = "postorder")
po_dtr.B.CGR.GTR_R3 <- reorder(dtr.B.GTR_R3, order = "postorder")

# GTR+G
B.GTR.G <- data.frame(times=po_dtr.B.CGR.GTR_G$Ti,
                      Bootstrap = po_B_CGR.GTR_G$node.label)
B.GTR.G$Bootstrap <- as.character(B.GTR.G$Bootstrap)
B.GTR.G$Bootstrap <- as.numeric(B.GTR.G$Bootstrap)

# GTR+R3
B.GTR.R3 <- data.frame(times=po_dtr.B.CGR.GTR_R3$Ti,
                       Bootstrap = po_B_CGR.GTR_R3$node.label)
B.GTR.R3$Bootstrap <- as.character(B.GTR.R3$Bootstrap)
B.GTR.R3$Bootstrap <- as.numeric(B.GTR.R3$Bootstrap)

quartz()
ggplot(B.GTR.G, aes(x=times, y=Bootstrap)) +
  geom_point(shape=19) + theme_bw() + ggtitle("B.CGR.GTR+G") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

ggplot(B.GTR.R3, aes(x=times, y=Bootstrap)) +
  geom_point(shape=19) + theme_bw() + ggtitle("B.CGR.GTR+R3") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))


#setwd("~/Box Sync/HIV_myDesktop/data/B/additional_data/treedater/no_duplicated/CGR/")
#write.tree(dtr.B.GTR_G, file = "dated_B_CGR_GTR_G_byCodon.tre")
#write.tree(dtr.B.GTR_Gp, file = "dated_B_CGR_GTR_Gp12+3_byCodon.tre")
#write.tree(dtr.B.GTR_I_G, file = "dated_B_CGR_GTR_I_G_byCodon.tre")
#write.tree(dtr.B.GTR_R3, file = "dated_B_CGR_GTR_R3_byCodon.tre")

#### NON-PARAMETRIC bootstrap subtype B

# Rooting bootstrap trees using the outgroups and removing outgroups from
# phylogenetic trees
B.CGR.GTR_G_100boot <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/B/additional_data/raxml-ng/no_duplicated/CGR/by_codon/B_CGR_GTR_G/ML_0_GTR+G.raxml.bootstraps.tre")[1:100]
B.CGR.GTR_G_100boot.r.new <- lapply(unclass(B.CGR.GTR_G_100boot),
                                    root_and_drop_tips,
                                    outgroups=c("Ref.D.CM.01.01CM_4412HAL.AY371157",
                                                "Ref.D.CD.83.ELI.K03454",
                                                "Ref.D.TZ.01.A280.AY253311"))
class(B.CGR.GTR_G_100boot.r.new)<-"multiPhylo"

B.CGR.GTR_R3_100boot <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/B/additional_data/raxml-ng/no_duplicated/CGR/by_codon/B_CGR_GTR_R/ML_0_GTR+R3.raxml.bootstraps.tre")[1:100]
B.CGR.GTR_R3_100boot.r.new <- lapply(unclass(B.CGR.GTR_R3_100boot),
                                     root_and_drop_tips,
                                     outgroups=c("Ref.D.CM.01.01CM_4412HAL.AY371157",
                                                 "Ref.D.CD.83.ELI.K03454",
                                                 "Ref.D.TZ.01.A280.AY253311"))
class(B.CGR.GTR_R3_100boot.r.new)<-"multiPhylo"

B.CGR.GTR_Gp_100boot <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/B/additional_data/raxml-ng/no_duplicated/CGR/by_codon/B_CGR_GTR_Gp/ML_0_GTR+Gp.raxml.bootstraps.tre")[1:100]
B.CGR.GTR_Gp_100boot.r.new <- lapply(unclass(B.CGR.GTR_Gp_100boot),
                                     root_and_drop_tips,
                                     outgroups=c("Ref.D.CM.01.01CM_4412HAL.AY371157",
                                                 "Ref.D.CD.83.ELI.K03454",
                                                 "Ref.D.TZ.01.A280.AY253311"))
class(B.CGR.GTR_Gp_100boot.r.new)<-"multiPhylo"



### treedater on bootstrap trees ####
np.boot.B.GTR_G <- boot.treedater(dtr.B.GTR_G, B.CGR.GTR_G_100boot.r.new)
np.boot.B.GTR_Gp <- boot.treedater(dtr.B.GTR_Gp, B.CGR.GTR_Gp_100boot.r.new)
np.boot.B.GTR_R3 <- boot.treedater(dtr.B.GTR_R3, B.CGR.GTR_R3_100boot.r.new)


boot.B.GTR_G.ltt <- boot.ltt(np.boot.B.GTR_G)
boot.B.GTR_G.ltt$group = "B.CGR.GTR+G"
boot.B.GTR_R3.ltt <- boot.ltt(np.boot.B.GTR_R3)
boot.B.GTR_R3.ltt$group = "B.CGR.GTR+R3"

boot.B.comb.ltt <- rbind(boot.B.GTR_G.ltt, boot.B.GTR_R3.ltt)
boot.B.comb.ltt$group <- as.factor(boot.B.comb.ltt$group)

quartz()
p <- ggplot(boot.B.comb.ltt, aes(color = group, group = group)) +
  geom_ribbon(aes(x = times, ymin = lb, ymax = ub, fill = group, col = group),
              alpha = 0.1)
p <- p + geom_path(aes(x = times, y = pml)) +
  scale_fill_manual(values=c("#0072B2", "#D55E00")) +
  scale_colour_manual(values=c("#0072B2", "#D55E00")) +
  theme_bw()
(p <- p + ylab("Lineages through time") + xlab("Time"))


################################################################################

## SUBTYPE C
C.SN.data <- data_format(C.SN, "SN")
C.SN.df <- C.SN.data[[1]]
C.missing_sample <- C.SN.data[[2]]

C.CGR.data <- data_format(C.CGR, "CGR")
C.CGR.df <- C.CGR.data[[1]]
C.CGR.missing_sample <- C.CGR.data[[2]]


#Dataframe to be used in treedater for the missing sample date
C.missing_sample.df <- rbind(C.missing_sample[c("tip", "lower", "upper")],
                             C.CGR.missing_sample[c("tip", "lower", "upper")])
C.missing_sample.df.2 <- C.missing_sample.df[(c("lower", "upper"))]
row.names(C.missing_sample.df.2) <- C.missing_sample.df[,1]

# chech which sequences are present in phylogeny but not in dataframe
C.df <- rbind(C.SN.df[c("Accession_number","tip","decimal")],
              C.CGR.df[c("Accession_number","tip","decimal")])
#(tC.CGR.GTR_G <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/C/additional_data/raxml-ng/no_duplicated/CGR/by_codon/C_CGR_GTR+G/ML_0_GTR+G.raxml.bestTree.raxml.support.tre"))
#C <- C.df[!C.df$tip %in% tC.CGR.GTR_G$tip.label,]

C.df$decimal <- as.character(C.df$decimal)
C.df$decimal <- as.numeric(C.df$decimal)


##### READING TREES for SUBTYPE C ##########
#DNA substitution model: GTR+G
(tC.CGR.GTR_G <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/C/additional_data/raxml-ng/no_duplicated/CGR/by_codon/C_CGR_GTR+G/ML_0_GTR+G.raxml.bestTree.raxml.support.tre"))
quartz()
plot(tC.CGR.GTR_G)
nodelabels()
(tC.CGR.GTR_G.r <- reroot(tC.CGR.GTR_G, node.number=188, edgelabel = TRUE))
plot(tC.CGR.GTR_G.r)
tC.CGR.GTR_G.r.new <- drop.tip(tC.CGR.GTR_G.r,
                               tip=c("Ref.D.CM.01.01CM_4412HAL.AY371157",
                                     "Ref.D.CD.83.ELI.K03454",
                                     "Ref.D.TZ.01.A280.AY253311"))
plot(ladderize(tC.CGR.GTR_G.r.new, right = FALSE))


#DNA substitution model: GTR+G partition by codon position 12+3
(tC.CGR.GTR_Gp <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/C/additional_data/raxml-ng/no_duplicated/CGR/by_codon/C_CGR_GTR+Gp/ML_0_GTR+Gp.raxml.bestTree.raxml.support.tre"))
quartz()
plot(tC.CGR.GTR_Gp)
nodelabels()
(tC.CGR.GTR_Gp.r <- reroot(tC.CGR.GTR_Gp, node.number=225, edgelabel = TRUE))
plot(tC.CGR.GTR_Gp.r)
tC.CGR.GTR_Gp.r.new <- drop.tip(tC.CGR.GTR_Gp.r,
                                tip=c("Ref.D.CM.01.01CM_4412HAL.AY371157",
                                      "Ref.D.CD.83.ELI.K03454",
                                      "Ref.D.TZ.01.A280.AY253311"))
plot(ladderize(tC.CGR.GTR_Gp.r.new, right = FALSE))


#DNA substitution model: GTR+I+G
(tC.CGR.GTR_I_G <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/C/additional_data/raxml-ng/no_duplicated/CGR/by_codon/C_CGR_GTR_I_G/ML_0_GTR+I+G.raxml.bestTree.raxml.support.tre"))
quartz()
plot(tC.CGR.GTR_I_G)
nodelabels()
#tC.CGR.GTR_I_G <- midpoint.root(tC.CGR.GTR_I_G) # I used midpoint root here because outgroup sequences did not group all together
# after using midpoint root the 3 outgroup sequences are together and it's now easy to get the node number representing the common ancestor of the outgroup sequences
# plot(tC.CGR.GTR_I_G)
# nodelabels()
(tC.CGR.GTR_I_G.r <- reroot(tC.CGR.GTR_I_G, node.number=190, edgelabel = TRUE))
plot(tC.CGR.GTR_I_G.r, no.mar=T)
(tC.CGR.GTR_I_G.r.new <- drop.tip(tC.CGR.GTR_I_G.r,
                                  tip=c("Ref.D.CM.01.01CM_4412HAL.AY371157",
                                        "Ref.D.CD.83.ELI.K03454",
                                        "Ref.D.TZ.01.A280.AY253311")))
plot(ladderize(tC.CGR.GTR_I_G.r.new, right = FALSE))


#DNA substitution model: GTR+R
(tC.CGR.GTR_R <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/C/additional_data/raxml-ng/no_duplicated/CGR/by_codon/C_CGR_GTR+R/ML_0_GTR+R.raxml.bestTree.raxml.support.tre"))
quartz()
plot(tC.CGR.GTR_R)
nodelabels()
(tC.CGR.GTR_R.r <- reroot(tC.CGR.GTR_R, node.number=226, edgelabel = TRUE))
plot(tC.CGR.GTR_R.r)
tC.CGR.GTR_R.r.new <- drop.tip(tC.CGR.GTR_R.r,
                               tip=c("Ref.D.CM.01.01CM_4412HAL.AY371157",
                                     "Ref.D.CD.83.ELI.K03454",
                                     "Ref.D.TZ.01.A280.AY253311"))
plot(ladderize(tC.CGR.GTR_R.r.new, right = FALSE))


######################################
# Tredater Senegal only Subtype C ####

C.seqlen <- 1302 # the length of the HIV sequences

C.sts <- C.df$decimal
names(C.sts) <- C.df$tip

(dtr.C.GTR_G <- dater(tC.CGR.GTR_G.r.new, C.sts, C.seqlen,
                      estimateSampleTimes = C.missing_sample.df.2))
(dtr.C.GTR_Gp <- dater(tC.CGR.GTR_Gp.r.new, C.sts, C.seqlen,
                       estimateSampleTimes = C.missing_sample.df.2))
(dtr.C.GTR_I_G <- dater(tC.CGR.GTR_I_G.r.new, C.sts, C.seqlen,
                        estimateSampleTimes = C.missing_sample.df.2))
(dtr.C.GTR_R <- dater(tC.CGR.GTR_R.r.new, C.sts, C.seqlen,
                      estimateSampleTimes = C.missing_sample.df.2))


# saving as RDS file
#path_dir="~/Box Sync/Senegal/treedater_R_files/CGR/byCodon/"
#saveRDS(dtr.C.GTR_G, paste(path_dir, "dtr.C.GTR_G_byCodon.RDS", sep=""))
#saveRDS(dtr.C.GTR_Gp, paste(path_dir, "dtr.C.GTR_Gp12+3_byCodon.RDS", sep=""))
#saveRDS(dtr.C.GTR_I_G, paste(path_dir, "dtr.C.GTR_I_G_byCodon.RDS", sep=""))
#saveRDS(dtr.C.GTR_R, paste(path_dir, "dtr.C.GTR_R_bycodon.RDS", sep=""))

#postorder trees
po_C_CGR.GTR_G <- reorder(tC.CGR.GTR_G.r.new, order = "postorder")
po_C_CGR.GTR_R <- reorder(tC.CGR.GTR_R.r.new, order = "postorder")

po_dtr.C.CGR.GTR_G <- reorder(dtr.C.GTR_G, order = "postorder")
po_dtr.C.CGR.GTR_R <- reorder(dtr.C.GTR_R, order = "postorder")

# GTR+G
C.GTR.G <- data.frame(times=po_dtr.C.CGR.GTR_G$Ti,
                      Bootstrap = po_C_CGR.GTR_G$node.label)
C.GTR.G$Bootstrap <- as.character(C.GTR.G$Bootstrap)
C.GTR.G$Bootstrap <- as.numeric(C.GTR.G$Bootstrap)

# GTR+R
C.GTR.R <- data.frame(times=po_dtr.C.CGR.GTR_R$Ti,
                      Bootstrap = po_C_CGR.GTR_R$node.label)
C.GTR.R$Bootstrap <- as.character(C.GTR.R$Bootstrap)
C.GTR.R$Bootstrap <- as.numeric(C.GTR.R$Bootstrap)

quartz()
ggplot(C.GTR.G, aes(x=times, y=Bootstrap)) +
  geom_point(shape=19) + theme_bw() + ggtitle("C.CGR.GTR+G") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

ggplot(C.GTR.R, aes(x=times, y=Bootstrap)) +
  geom_point(shape=19) + theme_bw() + ggtitle("C.CGR.GTR+R") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

#setwd("~/Box Sync/HIV_myDesktop/data/C/additional_data/treedater/no_duplicated/CGR/")
#write.tree(dtr.C.GTR_G, file = "dated_C_CGR_GTR_G.tre")
#write.tree(dtr.C.GTR_I_G, file = "dated_C_CGR_GTR_I_G.tre")
#write.tree(dtr.C.GTR_R, file = "dated_C_CGR_GTR_R.tre")

#quartz()
#plot(dtr.C.GTR_G)
#quartz()
#plot(dtr.C.GTR_R)

#### parametric bootstrap for treedater trees #######
boot.C.GTR_G <- parboot.treedater(dtr.C.GTR_G)
boot.C.GTR_R <- parboot.treedater(dtr.C.GTR_R)


boot.C.GTR_G.ltt <- parboot.ltt(boot.C.GTR_G)
boot.C.GTR_G.ltt$group = "C.CGR.GTR+G"
boot.C.GTR_R.ltt <- parboot.ltt(boot.C.GTR_R)
boot.C.GTR_R.ltt$group = "C.CGR.GTR+R"

boot.C.comb.ltt <- rbind(boot.C.GTR_G.ltt, boot.C.GTR_R.ltt)
boot.C.comb.ltt$group <- as.factor(boot.C.comb.ltt$group)


p <- ggplot(boot.C.comb.ltt, aes(color = group, group = group)) +
  geom_ribbon(aes(x = times, ymin = lb, ymax = ub, fill = group, col = group),
              alpha = 0.1)
p <- p + geom_path(aes(x = times, y = pml)) +
  scale_fill_manual(values=c("#0072B2", "#D55E00")) +
  scale_colour_manual(values=c("#0072B2", "#D55E00")) +
  theme_bw()
(p <- p + ylab("Lineages through time") + xlab("Time"))


#### non-parametric bootstrap for treedater trees #######
# Rooting bootstrap trees using the outgroups and removing
# outgroups from phylogenetic trees
C.CGR.GTR_G_100boot <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/C/additional_data/raxml-ng/no_duplicated/CGR/by_codon/C_CGR_GTR+G/final.BOOT.C.CGR.GTR+G.raxml.bootstraps")[1:100]
C.CGR.GTR_G_100boot.r.new <- lapply(unclass(C.CGR.GTR_G_100boot),
                                    root_and_drop_tips,
                                    outgroups=c("Ref.D.CM.01.01CM_4412HAL.AY371157",
                                                "Ref.D.CD.83.ELI.K03454",
                                                "Ref.D.TZ.01.A280.AY253311"))
class(C.CGR.GTR_G_100boot.r.new)<-"multiPhylo"

C.CGR.GTR_R_100boot <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/C/additional_data/raxml-ng/no_duplicated/CGR/by_codon/C_CGR_GTR+R/final.BOOT.C.CGR.GTR+R.raxml.bootstraps")[1:100]
C.CGR.GTR_R_100boot.r.new <- lapply(unclass(C.CGR.GTR_R_100boot),
                                    root_and_drop_tips,
                                    outgroups=c("Ref.D.CM.01.01CM_4412HAL.AY371157",
                                                "Ref.D.CD.83.ELI.K03454",
                                                "Ref.D.TZ.01.A280.AY253311"))
class(C.CGR.GTR_R_100boot.r.new)<-"multiPhylo"



boot.C.GTR_G <- boot.treedater(dtr.C.GTR_G, C.CGR.GTR_G_100boot.r.new)
boot.C.GTR_R <- boot.treedater(dtr.C.GTR_R, C.CGR.GTR_R_100boot.r.new)


boot.C.GTR_G.ltt <- boot.ltt(boot.C.GTR_G)
boot.C.GTR_G.ltt$group = "C.CGR.GTR+G"
boot.C.GTR_R.ltt <- boot.ltt(boot.C.GTR_R)
boot.C.GTR_R.ltt$group = "C.CGR.GTR+R"

boot.C.comb.ltt <- rbind(boot.C.GTR_G.ltt, boot.C.GTR_R.ltt)
boot.C.comb.ltt$group <- as.factor(boot.C.comb.ltt$group)


p <- ggplot(boot.C.comb.ltt, aes(color = group, group = group)) +
  geom_ribbon(aes(x = times, ymin = lb, ymax = ub, fill = group, col = group),
              alpha = 0.1)
p <- p + geom_path(aes(x = times, y = pml)) +
  scale_fill_manual(values=c("#0072B2", "#D55E00")) +
  scale_colour_manual(values=c("#0072B2", "#D55E00")) +
  theme_bw()
(p <- p + ylab("Lineages through time") + xlab("Time"))



###############################################################################

## SUBTYPE 02_AG
AG.SN.data <- data_format(AG.SN, "SN")
AG.SN.df <- AG.SN.data[[1]]
AG.missing_sample <- AG.SN.data[[2]]

AG.CGR.data <- data_format(AG.CGR, "CGR")
AG.CGR.df <- AG.CGR.data[[1]]
AG.CGR.missing_sample <- AG.CGR.data[[2]]


#Dataframe to be used in treedater for the missing sample date
AG.missing_sample.df <- rbind(AG.missing_sample[c("tip","lower","upper")],
                              AG.CGR.missing_sample[c("tip","lower","upper")])
AG.missing_sample.df.2 <- AG.missing_sample.df[(c("lower","upper"))]
row.names(AG.missing_sample.df.2) <- AG.missing_sample.df[,1]

AG.df <- rbind(AG.SN.df[c("Accession_number","tip","decimal")],
               AG.CGR.df[c("Accession_number","tip","decimal")])
#(tAG.CGR.GTR_G <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/02_AG/additional_data/raxml-ng/no_duplicated/CGR/02_AG_CGR_GTR+G/final.ML_13_GTR+G.raxml.bestTree.raxml.support.tre"))
#AG <- AG.df[!AG.df$tip %in% tAG.CGR.GTR_G$tip.label,]



AG.df$decimal <- as.character(AG.df$decimal)
AG.df$decimal <- as.numeric(AG.df$decimal)

##### READING TREES for SUBTYPE 02_AG ##########
#DNA substitution model: GTR+G
(tAG.CGR.GTR_G <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/02_AG/additional_data/raxml-ng/no_duplicated/CGR/by_codon/02_AG_CGR_GTR+G/final.ML_14_GTR+G.raxml.bestTree.raxml.support.tre"))
quartz()
plot(tAG.CGR.GTR_G, no.mar=T, cex = .5)
nodelabels()
(tAG.CGR.GTR_G.r <- reroot(tAG.CGR.GTR_G, node.number=484, edgelabel = TRUE))
plot(tAG.CGR.GTR_G.r)
(tAG.CGR.GTR_G.r.new <- drop.tip(tAG.CGR.GTR_G.r,
                                 tip=c("Ref.A1.UG.92.92UG037.AB253429",
                                       "Ref.A1.AU.03.PS1044_Day0.DQ676872",
                                       "Ref.A1.RW.92.92RW008.AB253421")))
plot(ladderize(tAG.CGR.GTR_G.r.new, right = FALSE))

#DNA substitution model: GTR+G partitioned by codon (12+3)
(tAG.CGR.GTR_Gp <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/02_AG/additional_data/raxml-ng/no_duplicated/CGR/by_codon/02_AG_CGR_GTR+Gp12+3/final.ML_9_GTR+Gp12+3.raxml.bestTree.raxml.support.tre"))
quartz()
plot(tAG.CGR.GTR_Gp, no.mar=T, cex = .7)
nodelabels()
(tAG.CGR.GTR_Gp.r <- reroot(tAG.CGR.GTR_Gp, node.number=689, edgelabel = TRUE))
plot(tAG.CGR.GTR_Gp.r)
(tAG.CGR.GTR_Gp.r.new <- drop.tip(tAG.CGR.GTR_Gp.r,
                                  tip=c("Ref.A1.UG.92.92UG037.AB253429",
                                        "Ref.A1.AU.03.PS1044_Day0.DQ676872",
                                        "Ref.A1.RW.92.92RW008.AB253421")))
plot(ladderize(tAG.CGR.GTR_Gp.r.new, right = FALSE))

#DNA substitution model: GTR+I+G
(tAG.CGR.GTR_I_G <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/02_AG/additional_data/raxml-ng/no_duplicated/CGR/by_codon/02_AG_CGR_GTR+I+G/final.ML_11_GTR+I+G.raxml.bestTree.raxml.support.tre"))
quartz()
plot(tAG.CGR.GTR_I_G, no.mar=T, cex = .5)
nodelabels()
(tAG.CGR.GTR_I_G.r <- reroot(tAG.CGR.GTR_I_G, node.number=822, edgelabel = TRUE))
plot(tAG.CGR.GTR_I_G.r)
(tAG.CGR.GTR_I_G.r.new <- drop.tip(tAG.CGR.GTR_I_G.r,
                                   tip=c("Ref.A1.UG.92.92UG037.AB253429",
                                         "Ref.A1.AU.03.PS1044_Day0.DQ676872",
                                         "Ref.A1.RW.92.92RW008.AB253421")))
plot(ladderize(tAG.CGR.GTR_I_G.r.new, right = FALSE))

#DNA substitution model: GTR+R
(tAG.CGR.GTR_R <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/02_AG/additional_data/raxml-ng/no_duplicated/CGR/by_codon/02_AG_CGR_GTR+R/final.ML_15_GTR+R.raxml.bestTree.raxml.support.tre"))
quartz()
plot(tAG.CGR.GTR_R, no.mar=T, cex = .5)
nodelabels()
(tAG.CGR.GTR_R.r <- reroot(tAG.CGR.GTR_R, node.number=615, edgelabel = TRUE))
plot(tAG.CGR.GTR_R.r)
(tAG.CGR.GTR_R.r.new <- drop.tip(tAG.CGR.GTR_R.r,
                                 tip=c("Ref.A1.UG.92.92UG037.AB253429",
                                       "Ref.A1.AU.03.PS1044_Day0.DQ676872",
                                       "Ref.A1.RW.92.92RW008.AB253421")))
plot(ladderize(tAG.CGR.GTR_R.r.new, right = FALSE))






####### TREEDATER ANALYSIS ##########
AG.seqlen <- 1296 # the length of the HIV sequences

AG.sts <- AG.df$decimal
names(AG.sts) <- AG.df$tip

(dtr.AG.CGR.GTR_G <- dater(tAG.CGR.GTR_G.r.new, AG.sts, AG.seqlen,
                           estimateSampleTimes = AG.missing_sample.df.2) )
(dtr.AG.CGR.GTR_Gp <- dater(tAG.CGR.GTR_Gp.r.new, AG.sts, AG.seqlen,
                            estimateSampleTimes = AG.missing_sample.df.2) )
(dtr.AG.CGR.GTR_I_G <- dater(tAG.CGR.GTR_I_G.r.new, AG.sts, AG.seqlen,
                             estimateSampleTimes = AG.missing_sample.df.2) )
(dtr.AG.CGR.GTR_R <- dater(tAG.CGR.GTR_R.r.new, AG.sts, AG.seqlen,
                           estimateSampleTimes = AG.missing_sample.df.2) )

# saving as RDS file
#path_dir="~/Box Sync/Senegal/treedater_R_files/"
#saveRDS(dtr.AG.CGR.GTR_G, paste(path_dir, "dtr.AG.CGR.GTR_G_byCodonCORRECT.RDS", sep=""))
#saveRDS(dtr.AG.CGR.GTR_Gp, paste(path_dir, "dtr.AG.CGR.GTR_Gp12+3_byCodonCORRECT.RDS", sep=""))
#saveRDS(dtr.AG.CGR.GTR_I_G, paste(path_dir, "dtr.AG.CGR.GTR_I_G_byCodonCORRECT.RDS", sep=""))
#saveRDS(dtr.AG.CGR.GTR_R, paste(path_dir, "dtr.AG.CGR.GTR_R_byCodonCORRECT.RDS", sep=""))


#postorder trees to plot times vs bootstrap
po_AG_CGR.GTR_G <- reorder(tAG.CGR.GTR_G.r.new, order = "postorder")
po_AG_CGR.GTR_R <- reorder(tAG.CGR.GTR_R.r.new, order = "postorder")

po_dtr.AG.CGR.GTR_G <- reorder(dtr.AG.CGR.GTR_G, order = "postorder")
po_dtr.AG.CGR.GTR_R <- reorder(dtr.AG.CGR.GTR_R, order = "postorder")

# GTR+G
AG.GTR.G <- data.frame(times=po_dtr.AG.CGR.GTR_G$Ti, Bootstrap = po_AG_CGR.GTR_G$node.label)
AG.GTR.G$Bootstrap <- as.character(AG.GTR.G$Bootstrap)
AG.GTR.G$Bootstrap <- as.numeric(AG.GTR.G$Bootstrap)

# GTR+R
AG.GTR.R <- data.frame(times=po_dtr.AG.CGR.GTR_R$Ti, Bootstrap = po_AG_CGR.GTR_R$node.label)
AG.GTR.R$Bootstrap <- as.character(AG.GTR.R$Bootstrap)
AG.GTR.R$Bootstrap <- as.numeric(AG.GTR.R$Bootstrap)

quartz()
ggplot(AG.GTR.G, aes(x=times, y=Bootstrap)) +
  geom_point(shape=19) + theme_bw() + ggtitle("AG.CGR.GTR+G") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

ggplot(AG.GTR.R, aes(x=times, y=Bootstrap)) +
  geom_point(shape=19) + theme_bw() + ggtitle("AG.CGR.GTR+R") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))



#setwd("~/Box Sync/HIV_myDesktop/data/02_AG/additional_data/treedater/no_duplicated/CGR/")
#write.tree(dtr.AG.CGR.GTR_G, file = "dated_AG_CGR_GTR_G.tre")
#write.tree(dtr.AG.CGR.GTR_R, file = "dated_AG_CGR_GTR_R.tre")



#### parametric bootstrap for treedater trees #######
boot.AG.GTR_G <- parboot.treedater(dtr.AG.CGR.GTR_G)
boot.AG.GTR_R <- parboot.treedater(dtr.AG.CGR.GTR_R)


boot.AG.GTR_G.ltt <- parboot.ltt(boot.AG.GTR_G)
boot.AG.GTR_G.ltt$group = "02_AG.CGR.GTR+G"
boot.AG.GTR_R.ltt <- parboot.ltt(boot.AG.GTR_R)
boot.AG.GTR_R.ltt$group = "02_AG.CGR.GTR+R"

boot.AG.comb.ltt <- rbind(boot.AG.GTR_G.ltt, boot.AG.GTR_R.ltt)
boot.AG.comb.ltt$group <- as.factor(boot.AG.comb.ltt$group)


p <- ggplot(boot.AG.comb.ltt, aes(color = group, group = group)) +
  geom_ribbon(aes(x = times, ymin = lb, ymax = ub, fill = group, col = group),
              alpha = 0.1)
p <- p + geom_path(aes(x = times, y = pml)) +
  scale_fill_manual(values=c("#0072B2", "#D55E00")) +
  scale_colour_manual(values=c("#0072B2", "#D55E00")) +
  theme_bw()
(p <- p + ylab("Lineages through time") + xlab("Time"))


#### non-parametric bootstrap for treedater trees #######
# Rooting bootstrap trees using the outgroups and removing outgroups from phylogenetic trees
AG.CGR.GTR_G_100boot <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/02_AG/additional_data/raxml-ng/no_duplicated/CGR/by_codon/02_AG_CGR_GTR+G/final.BOOT.02_AG.CGR.GTR+G.raxml.bootstraps.tre")[1:2]
AG.CGR.GTR_G_100boot.r.new <- lapply(unclass(AG.CGR.GTR_G_100boot),
                                     root_and_drop_tips,
                                     outgroups=c("Ref.A1.UG.92.92UG037.AB253429",
                                                 "Ref.A1.AU.03.PS1044_Day0.DQ676872",
                                                 "Ref.A1.RW.92.92RW008.AB253421"))
class(AG.CGR.GTR_G_100boot.r.new)<-"multiPhylo"


AG.CGR.GTR_R_100boot <- read.tree("~/Box Sync/Senegal/HIV_myDesktop/data/02_AG/additional_data/raxml-ng/no_duplicated/CGR/02_AG_CGR_GTR+R/final.BOOT.02_AG.CGR.GTR+R.raxml.bootstraps.tre")[1:100]
AG.CGR.GTR_R_100boot.r.new <- lapply(unclass(AG.CGR.GTR_R_100boot),
                                     root_and_drop_tips,
                                     outgroups=c("Ref.A1.UG.92.92UG037.AB253429",
                                                 "Ref.A1.AU.03.PS1044_Day0.DQ676872",
                                                 "Ref.A1.RW.92.92RW008.AB253421"))
class(AG.CGR.GTR_R_100boot.r.new)<-"multiPhylo"



boot.AG.GTR_G <- boot.treedater(dtr.AG.CGR.GTR_G, AG.CGR.GTR_G_100boot.r.new)
boot.AG.GTR_G.2 <- boot.treedater(dtr.AG.CGR.GTR_G, AG.CGR.GTR_G_100boot.r.new)
boot.AG.GTR_G.3 <- boot.treedater(dtr.AG.CGR.GTR_G, AG.CGR.GTR_G_100boot.r.new)
boot.AG.GTR_G.4 <- boot.treedater(dtr.AG.CGR.GTR_G, AG.CGR.GTR_G_100boot.r.new)
boot.AG.GTR_G.5 <- boot.treedater(dtr.AG.CGR.GTR_G, AG.CGR.GTR_G_100boot.r.new)


boot.AG.GTR_R <- boot.treedater(dtr.AG.CGR.GTR_R, AG.CGR.GTR_R_100boot.r.new)


boot.AG.GTR_G.ltt <- boot.ltt(boot.AG.GTR_G)
boot.AG.GTR_G.2.ltt <- boot.ltt(boot.AG.GTR_G.2)

boot.AG.GTR_G.ltt$group = "AG.CGR.GTR+G"
boot.AG.GTR_R.ltt <- boot.ltt(boot.AG.GTR_R)
boot.AG.GTR_R.ltt$group = "AG.CGR.GTR+R"

boot.AG.comb.ltt <- rbind(boot.AG.GTR_G.ltt, boot.AG.GTR_R.ltt)
boot.AG.comb.ltt$group <- as.factor(boot.AG.comb.ltt$group)


p <- ggplot(boot.AG.comb.ltt, aes(color = group, group = group)) +
  geom_ribbon(aes(x = times, ymin = lb, ymax = ub, fill = group, col = group),
              alpha = 0.1)
p <- p + geom_path(aes(x = times, y = pml)) +
  scale_fill_manual(values=c("#0072B2", "#D55E00")) +
  scale_colour_manual(values=c("#0072B2", "#D55E00")) + theme_bw()
(p <- p + ylab("Lineages through time") + xlab("Time"))


