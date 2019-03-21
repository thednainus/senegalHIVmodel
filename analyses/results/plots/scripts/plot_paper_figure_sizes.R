####### create plots for paper
library(ggplot2)
library(phydynR)
library(reshape2)
library(senegalHIVmodel)

#### Sizes ####

# SUBTYPE C: Sizes
# Load solved objects
load("analyses/results/plots/solved_objects/dmC_m3.rda")

# MODEL 3
Cm3_sizes <- df_sizes_prop(sizes.p = dmC_m3.1$run[4,],
                           sizes.map = dmC_m3.1$MAP[4],
                           times = dmC_m3.1$run[[1]], Nrep = 1000, Ntime = 1000)
Cm3_sizes["Model"] <- "Subtype C"

Cm3_sizes.l <- melt(Cm3_sizes, id.vars = c("times", "lower", "upper",
                                           "group", "group2", "Model"))
colnames(Cm3_sizes.l)[4] <- "Deme"
colnames(Cm3_sizes.l)[7] <- "Linetype"
Cm3_sizes.l["values2"] <- "Proportion"

# absolute numbers
aCm3_sizes <- df_sizes(sizes.p = dmC_m3.1$run[4,],
                       sizes.map = dmC_m3.1$MAP[4],
                       times = dmC_m3.1$run[[1]], Nrep = 1000, Ntime = 1000)
aCm3_sizes["Model"] <- "Subtype C"

aCm3_sizes.l <- melt(aCm3_sizes, id.vars = c("times", "lower", "upper",
                                             "group", "group2", "Model"))
colnames(aCm3_sizes.l)[4] <- "Deme"
colnames(aCm3_sizes.l)[7] <- "Linetype"
aCm3_sizes.l["values2"] <- "Absolute number"

# merge proportions and absolute number dataframes
C_merged <- rbind(Cm3_sizes.l, aCm3_sizes.l)
C_merged["Model2"] <- paste(C_merged$Model, C_merged$values2, sep = " - ")



# SUBTYPE 02AG: sizes
# Load solved objects
load("analyses/results/plots/solved_objects/dmAG_m3.2.rda")

# MODEL 3
# proportions
AGm3_sizes <- df_sizes_prop(sizes.p = dmAG_m3.2$run[4,],
                            sizes.map = dmAG_m3.2$MAP[4],
                            times = dmAG_m3.2$run[[1]], Nrep = 1000, Ntime = 1000)
AGm3_sizes["Model"] <- "Subtype 02_AG"

AGm3_sizes.l <- melt(AGm3_sizes, id.vars = c("times", "lower", "upper",
                                             "group", "group2", "Model"))
colnames(AGm3_sizes.l)[4] <- "Deme"
colnames(AGm3_sizes.l)[7] <- "Linetype"
AGm3_sizes.l["values2"] <- "Proportion"

# absolute numbers
aAGm3_sizes <- df_sizes(sizes.p = dmAG_m3.2$run[4,],
                        sizes.map = dmAG_m3.2$MAP[4],
                        times = dmAG_m3.2$run[[1]], Nrep = 1000, Ntime = 1000)
aAGm3_sizes["Model"] <- "Subtype 02_AG"

aAGm3_sizes.l <- melt(aAGm3_sizes, id.vars = c("times", "lower", "upper",
                                               "group", "group2", "Model"))
colnames(aAGm3_sizes.l)[4] <- "Deme"
colnames(aAGm3_sizes.l)[7] <- "Linetype"
aAGm3_sizes.l["values2"] <- "Absolute number"

# merge proportions and absolute number dataframes
AG_merged <- rbind(AGm3_sizes.l, aAGm3_sizes.l)
AG_merged["Model2"] <- paste(AG_merged$Model, AG_merged$values2, sep = " - ")

# SUBTYPES COMBINED: Sizes

#PREVALENCE
load("analyses/results/plots/solved_objects/dm_m5.rda")

# Model 5 (prevalence)
m5_sizes <- df_sizes_prop(sizes.p = dm_m5.1$run[4,],
                          sizes.map = dm_m5.1$MAP[4],
                          times = dm_m5.1$run[[1]], Nrep = 1000, Ntime = 1000)
m5_sizes["Model"] <- "Subtypes combined"


m5_sizes.l <- melt(m5_sizes, id.vars = c("times", "lower", "upper",
                                         "group", "group2", "Model"))
colnames(m5_sizes.l)[4] <- "Deme"
colnames(m5_sizes.l)[7] <- "Linetype"
m5_sizes.l["values2"] <- "Proportion"

# absolute numbers
am5_sizes <- df_sizes(sizes.p = dm_m5.1$run[4,],
                      sizes.map = dm_m5.1$MAP[4],
                      times = dm_m5.1$run[[1]], Nrep = 1000, Ntime = 1000)
am5_sizes["Model"] <- "Subtypes combined"

am5_sizes.l <- melt(am5_sizes, id.vars = c("times", "lower", "upper",
                                           "group", "group2", "Model"))
colnames(am5_sizes.l)[4] <- "Deme"
colnames(am5_sizes.l)[7] <- "Linetype"
am5_sizes.l["values2"] <- "Absolute number"

# merge proportions and absolute number dataframes
m5_merged <- rbind(m5_sizes.l, am5_sizes.l)
m5_merged["Model2"] <- paste(m5_merged$Model, m5_merged$values2, sep = " - ")


#PLOT
# Plot for subtype C (for proportions and absolute numbers)
C_sizes <- ggplot(C_merged, aes(x = times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.70) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(values2 ~ group2, scales = "free", ncol = 2) +
  ylab("Effective number of infections") +
  xlab("Time (years)") +
  scale_fill_brewer() + scale_colour_brewer() + theme_bw() +
  theme(legend.position="bottom", text = element_text(size = 20))

# Plot for subtype 02_AG (for proportions and absolute numbers)
AG_sizes <- ggplot(AG_merged, aes(x = times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.70) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(values2 ~ group2, scales = "free", ncol = 2) +
  ylab("Effective number of infections") +
  xlab("Time (years)") +
  scale_fill_brewer() + scale_colour_brewer() + theme_bw() +
  theme(legend.position="bottom", text = element_text(size = 20))

# Plot for combined analyses (for proportions and absolute numbers)
m5_sizes <- ggplot(m5_merged, aes(x = times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.70) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(values2 ~ group2, scales = "free", ncol = 2) +
  ylab("Effective number of infections") +
  xlab("Time (years)") +
  scale_fill_brewer() + scale_colour_brewer() + theme_bw() +
  theme(legend.position="bottom", text = element_text(size = 20))









# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
