library(treedater)
library(skygrowth)
library(ape)
library(ggplot2)
library(phylodyn)

load(system.file('NY_flu.rda', package='skygrowth'))
(tr <- NY_flu) # NOTE branch lengths in weeks  / 13 years in all


plot(tr)
fit <- skygrowth.map(tr
                     ,res = 24*13  # Ne changes every 2 weeks
                     ,tau0 = .1    # Smoothing parameter. If prior is not specified, this will also set the scale of the prior
                     )
plot(fit)


#phylodyn
b0 <- BNPR(tr)
plot_BNPR(b0)

mcmcfit <- skygrowth.mcmc(tr, res = 24*13, tau0=.1 )
quartz()
plot( mcmcfit )  + scale_y_log10(limits=c(.01, 1e5))

growth.plot( mcmcfit )


# Senegal data
# Reading metadata for Senegal only samples and close global reference (CGR) samples
SN.data <- read.csv(system.file("data/HIV_subtypes_summary_SENEGAL_noDups.csv", package = "senegalHIVmodel"))
CGR.data <-  read.csv(system.file("data/HIV_subtypes_summary_CGR.csv", package = "senegalHIVmodel"))

# organize metadata in 2 columns.
# the first column is the sequence names
# the second colum is the state (gpf, gpm, msm, or src) of each sequences
all_data <- organize_metadata(CGR.data, SN.data)

# Senegal tree
tree.all <- read.tree(system.file("data/bindTree_CGR_GTR+Gp12+3_droppedTip.tre", package = "senegalHIVmodel"))

only_msm <- subset(all_data, States == "msm")
only_msm$States <- as.character(only_msm$States)
only_msm$States <- as.factor(only_msm$States)

only_src <- subset(all_data, States == "src")
only_src$States <- as.character(only_src$States)
only_src$States <- as.factor(only_src$States)

tree_gp_src <- drop.tip(tree.all, tip = only_msm$tip.name)

tree_gp <- drop.tip(tree_gp_src, tip = only_src$tip.name)

fit1 <- skygrowth.map(tree_gp_src, tau0 = 0.1)
fit2 <- skygrowth.map(tree_gp, tau0 = 0.1)
plot(fit1)  + ggtitle("gp and src")
plot(fit2)  + ggtitle("gp")

fit3 <- skygrowth.mcmc(tree_gp_src, tau0 = 0.1)
plot(fit3)  + ggtitle("mcmc gp and src")

# double check this
plot(fit3) + xlim(-40,0) + scale_y_log10(limits=c(1, 1e6))

fit4 <- skygrowth.mcmc(tree_gp, tau0 = 0.1)
plot(fit4)  + ggtitle("mcmc gp")

#phylodyn
b0 <- BNPR(tree_gp_src)
plot_BNPR(b0)

