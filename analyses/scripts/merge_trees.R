# Script used to load the dated tree for each HIV subtype and merge this tree in a polytomy

library(ape)
library(treedater)

# Reads the dated tree for subtype B, C and 02_AG
tB.CGR.Gp <- readRDS(system.file("data/trees_by_subtype/dtr.B.CGR.GTR_Gp12+3_byCodon.RDS", package = "senegalHIVmodel"))
tC.CGR.Gp <- readRDS(system.file("data/trees_by_subtype/dtr.C.CGR.GTR_Gp12+3_byCodon.RDS", package = "senegalHIVmodel"))
tAG.CGR.Gp <- readRDS(system.file("data/trees_by_subtype/dtr.AG.CGR.GTR_Gp12+3_byCodon.RDS", package = "senegalHIVmodel"))


tB.CGR.Gp$timeOfMRCA #1974.837 youngest
tC.CGR.Gp$timeOfMRCA #1939.386 oldest
tAG.CGR.Gp$timeOfMRCA #1950.909 middle age

# Create a root edge to add to the youngest MRCA (in this example it is the Subtype B tree)
B.edge <- tB.CGR.Gp$timeOfMRCA - tAG.CGR.Gp$timeOfMRCA
tB.CGR.Gp$root.edge <- B.edge

# Bind the 2 youngest trees together
tr <- bind.tree(tAG.CGR.Gp, tB.CGR.Gp)

# Create a root edge to add to the youngest MRCA (now it is the merged 02_AG and B subtypes)
tr.edge <- tAG.CGR.Gp$timeOfMRCA - tC.CGR.Gp$timeOfMRCA
tr$root.edge <- tr.edge

# Bind the 3rd tree to the prevoius binded tree "tr"
tr2 <- bind.tree(tC.CGR.Gp, tr)

# add estimated dates to a variable
time.B <- tB.CGR.Gp$sts
time.C <- tC.CGR.Gp$sts
time.AG <- tAG.CGR.Gp$sts


all.times <- c(time.C, time.AG, time.B)


# Create plot "distance tip to root" vs "estimated dates"
# You should expect to see a 1 to 1 relationship if everythig was merged correctly
plot(dist.nodes(tr2)[1:621,622], all.times)


# Drop tips that are from Children, risk group = NA, and sex = NA
# That is because the mathematical model assumes we know this information for each tip
summary.data <- read.csv(system.file("data/HIV_subtypes_SENEGAL_noDups_ToDrop_fromAnalysis.csv", package = "senegalHIVmodel"))
summary.data$tip <- paste(summary.data$Accession_number, summary.data$Subtype, "SN", summary.data$Year, sep='.')

# New tree without tips from Children, risk group = NA, and sex = NA
tr3 <- drop.tip(tr2, tip = summary.data$tip)


# Remove the dropped tip estimated dates from the all.times vector
new.times <- all.times

the_names = c(summary.data$tip)

to.remove = which(names(new.times) %in% the_names)
new.times2 <- new.times[-c(to.remove)]

#Testing whether the plot distance tip to root vs estimated dates are correct after removing some estimate dates
plot(dist.nodes(tr3)[1:516,517], new.times2)

