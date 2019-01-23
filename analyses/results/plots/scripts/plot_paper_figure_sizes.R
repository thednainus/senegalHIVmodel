####### create plots for paper
library(ggplot2)
library(phydynR)
library(reshape2)
library(senegalHIVmodel)

#### Sizes ####

# SUBTYPE C: Sizes
# Load solved objects
load("analyses/plots/solved_objects/dmC_m4.2.rda")

# MODEL 4
Cm4_sizes <- df_sizes_prop(sizes.p = dmC_m4.2$run[4,],
                           sizes.map = dmC_m4.2$MAP[4],
                           times = dmC_m4.2$run[[1]], Nrep = 1000, Ntime = 1000)
Cm4_sizes["Model"] <- "Subtype C"

Cm4_sizes.l <- melt(Cm4_sizes, id.vars = c("times", "lower", "upper",
                                           "group", "group2", "Model"))
colnames(Cm4_sizes.l)[4] <- "Deme"
colnames(Cm4_sizes.l)[7] <- "Linetype"


# SUBTYPE 02AG: sizes
# Load solved objects
load("analyses/plots/solved_objects/dmAG_m4.2.rda")

# MODEL 4
# proportions
AGm4_sizes <- df_sizes_prop(sizes.p = dmAG_m4.2$run[4,],
                            sizes.map = dmAG_m4.2$MAP[4],
                            times = dmAG_m4.2$run[[1]], Nrep = 1000, Ntime = 1000)
AGm4_sizes["Model"] <- "Subtype 02_AG"

AGm4_sizes.l <- melt(AGm4_sizes, id.vars = c("times", "lower", "upper",
                                             "group", "group2", "Model"))
colnames(AGm4_sizes.l)[4] <- "Deme"
colnames(AGm4_sizes.l)[7] <- "Linetype"
AGm4_sizes.l["values2"] <- "Proportion"
# absolute numbers
aAGm4_sizes <- df_sizes(sizes.p = dmAG_m4.2$run[4,],
                        sizes.map = dmAG_m4.2$MAP[4],
                        times = dmAG_m4.2$run[[1]], Nrep = 1000, Ntime = 1000)
aAGm4_sizes["Model"] <- "Subtype 02_AG"

aAGm4_sizes.l <- melt(aAGm4_sizes, id.vars = c("times", "lower", "upper",
                                               "group", "group2", "Model"))
colnames(aAGm4_sizes.l)[4] <- "Deme"
colnames(aAGm4_sizes.l)[7] <- "Linetype"
aAGm4_sizes.l["values2"] <- "Absolute number"

# merge proportions and absolute number dataframes
AG_merged <- rbind(AGm4_sizes.l, aAGm4_sizes.l)

# SUBTYPES COMBINED: Sizes

#PREVALENCE
load("analyses/plots/solved_objects/dm_m6.rda")

# Model 6 (prevalence)
m6_sizes <- df_sizes_prop(sizes.p = dm_m6.1$run[4,],
                          sizes.map = dm_m6.1$MAP[4],
                          times = dm_m6.1$run[[1]], Nrep = 1000, Ntime = 1000)
m6_sizes["Model"] <- "Subtypes combined"

m6_sizes.l <- melt(m6_sizes, id.vars = c("times", "lower", "upper",
                                         "group", "group2", "Model"))
colnames(m6_sizes.l)[4] <- "Deme"
colnames(m6_sizes.l)[7] <- "Linetype"


# Combine dataframes
all_data <- rbind(Cm4_sizes.l, AGm4_sizes.l, m6_sizes.l)

#PLOT
p1_sizes <- ggplot(all_data, aes(x = times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.60) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap(Model ~ Deme, scales = "free") +
  ylab("Effective number of infections (proportion)") +
  xlab("Time (years)") +
  scale_fill_grey()+ scale_colour_grey() + theme_bw() +
  theme(legend.position="bottom")

# Plot for subtype CRF 02_AG
AG_sizes <- ggplot(AGm4_sizes.l, aes(x = times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.45) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  ylab("Effective number of infections (proportion)") +
  xlab("Time (years)") +
  theme_bw() +
  theme(legend.position="bottom")

AG <- ggplot(aAGm4_sizes.l, aes(x = times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Deme), alpha=0.45) +
  geom_line(aes(y = value, colour = Deme, linetype = Linetype)) +
  facet_wrap( ~ group2, scales = "free", ncol = 1) +
  ylab("Effective number of infections (absolute number)") +
  xlab("Time (years)") +
  theme_bw() +
  theme(legend.position="none")

multiplot(AG, AG_sizes)
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
