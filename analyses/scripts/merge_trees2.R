# Script used to load the dated tree for each HIV subtype and merge this tree in a polytomy

library(ape)
library(treedater)

# Reads the dated tree for subtype B, C and 02_AG
tB.CGR.Gp <- readRDS(system.file("data/trees_by_subtype/dtr.B.CGR.GTR_Gp12+3_byCodon.RDS", package = "senegalHIVmodel"))
tC.CGR.Gp <- readRDS(system.file("data/trees_by_subtype/dtr.C.CGR.GTR_Gp12+3_byCodon.RDS", package = "senegalHIVmodel"))
tAG.CGR.Gp <- readRDS(system.file("data/trees_by_subtype/dtr.AG.CGR.GTR_Gp12+3_byCodon.RDS", package = "senegalHIVmodel"))


tB.CGR.Gp$timeOfMRCA #1974.837 youngest
tC.CGR.Gp$timeOfMRCA #1960.757 middle age
tAG.CGR.Gp$timeOfMRCA #1950.909 oldest

# Create a root edge to add to the youngest MRCA (in this example it is the Subtype B tree)
B.edge <- tB.CGR.Gp$timeOfMRCA - tC.CGR.Gp$timeOfMRCA
tB.CGR.Gp$root.edge <- B.edge

# Bind the 2 youngest trees together
tr <- bind.tree(tC.CGR.Gp, tB.CGR.Gp)

# Create a root edge to add to the oldest MRCA
tr.edge <- tC.CGR.Gp$timeOfMRCA - tAG.CGR.Gp$timeOfMRCA
tr$root.edge <- tr.edge

# Bind the 3rd tree to the prevoius binded tree "tr"
tr2 <- bind.tree(tAG.CGR.Gp, tr)

# add estimated dates to a variable
time.B <- tB.CGR.Gp$sts
time.C <- tC.CGR.Gp$sts
time.AG <- tAG.CGR.Gp$sts


all.times <- c(time.AG, time.C, time.B)


# Create plot "distance tip to root" vs "estimated dates"
# You should expect to see a 1 to 1 relationship if everythig was merged correctly
plot(dist.nodes(tr2)[1:617,618], all.times)

plot(dist.nodes(tr)[1:169,170], c(time.C, time.B))

# Drop tips that are from Children, risk group = NA, and sex = NA
# That is because the mathematical model assumes we know this information for each tip
metadata <- read.csv(system.file("data/HIV_subtypes_summary_SENEGAL_noDups.csv",
                                 package = "senegalHIVmodel"))

metadata2 <- subset(metadata,
                    is.na(Risk_group) == TRUE | Risk_group == "Children" | is.na(Sex) == TRUE)
metadata2$tip <- paste(metadata2$Accession_number, metadata2$Subtype, "SN", metadata2$Year, sep='.')


# New tree without tips from Children, risk group = NA, and sex = NA
tr3 <- drop.tip(tr2, tip = metadata2$tip)


# Remove the dropped tip estimated dates from the all.times vector
new.times <- all.times

the_names = c(metadata2$tip)

to.remove = which(names(new.times) %in% the_names)
new.times2 <- new.times[-c(to.remove)]

#Testing whether the plot distance tip to root vs estimated dates are correct after removing some estimate dates
plot(dist.nodes(tr3)[1:512,513], new.times2)

#write.tree(tr3, "~/Box Sync/my_R_packages/senegalHIVmodel/inst/data/bindTree_CGR_GTR+Gp12+3_droppedTip.tre")
#saveRDS(new.times2, "~/Box Sync/my_R_packages/senegalHIVmodel/inst/data/bindTree_CGR_GTR+Gp12+3_droppedTip_sts.RDS")
