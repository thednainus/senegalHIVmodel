####### create plots
library(ggplot2)
library(phydynR)
library(reshape2)


# Load solved objects
load("analyses/plots/solved_objects/solved_objects_prevalence_1000reps.rda")
load("analyses/plots/solved_objects/solved_objects_prevalence_maps.rda")


### Model 5 ######
# gets the element births. Births are the number of new HIV cases
bm5_1000 <- m5_o.1000[2,]
times_m5 <- m5_o.1000[[1]]

# calculate the median and quantiles for pafs
all_m5 <- births_pafs(bm5_1000, times_m5)
# calculate pafs for MAP
m5_pafs_map <- t(sapply(m5_map_o[[2]], calculate_pafs))

#convert it to dataframe and transform to long format
m5.map.df <- as.data.frame(m5_pafs_map)
m5.map.df.m <- melt(m5.map.df)

#add map to dataframe
all_m5["MAP"] <- m5.map.df.m$value

### Model 7 ######
# gets the element births. Births are the number of new HIV cases
bm7_1000 <- m7_o.1000[2,]
times_m7 <- m7_o.1000[[1]]

# calculate the median and quantiles for pafs
all_m7 <- births_pafs(bm7_1000, times_m7)
# calculate pafs for MAP
m7_pafs_map <- t(sapply(m7_map_o[[2]], calculate_pafs))

#convert it to dataframe and transform to long format
m7.map.df <- as.data.frame(m7_pafs_map)
m7.map.df.m <- melt(m7.map.df)

#add map to dataframe
all_m7["MAP"] <- m7.map.df.m$value


### Subtype 02_AG: model 3 ######
# gets the element births. Births are the number of new HIV cases
b02_AGm3_1000 <- m02_AG_m3_o.1000[2,]
times_02_AGm3 <- m02_AG_m3_o.1000[[1]]

# calculate the median and quantiles for pafs
all_02_AGm3 <- births_pafs(b02_AGm3_1000, times_02_AGm3)
# calculate pafs for MAP
m02_AGm3_pafs_map <- t(sapply(m02_AG_m3_map_o[[2]], calculate_pafs))

#convert it to dataframe and transform to long format
m02_AGm3.map.df <- as.data.frame(m02_AGm3_pafs_map)
m02_AGm3.map.df.m <- melt(m02_AGm3.map.df)

#add map to dataframe
all_02_AGm3["MAP"] <- m02_AGm3.map.df.m$value


### Subtype 02_AG: model 4 ######
# gets the element births. Births are the number of new HIV cases
b02_AGm4_1000 <- m02_AG_m4_o.1000[2,]
times_02_AGm4 <- m02_AG_m4_o.1000[[1]]

# calculate the median and quantiles for pafs
all_02_AGm4 <- births_pafs(b02_AGm4_1000, times_02_AGm4)
# calculate pafs for MAP
m02_AGm4_pafs_map <- t(sapply(m02_AG_m4_map_o[[2]], calculate_pafs))

#convert it to dataframe and transform to long format
m02_AGm4.map.df <- as.data.frame(m02_AGm4_pafs_map)
m02_AGm4.map.df.m <- melt(m02_AGm4.map.df)

#add map to dataframe
all_02_AGm4["MAP"] <- m02_AGm4.map.df.m$value


### Subtype C: model 3 ######
# gets the element births. Births are the number of new HIV cases
bCm3_1000 <- C_m3_o.1000[2,]
times_Cm3 <- C_m3_o.1000[[1]]

# calculate the median and quantiles for pafs
all_Cm3 <- births_pafs(bCm3_1000, times_Cm3)
# calculate pafs for MAP
mCm3_pafs_map <- t(sapply(mC_m3_map_o[[2]], calculate_pafs))

#convert it to dataframe and transform to long format
mCm3.map.df <- as.data.frame(mCm3_pafs_map)
mCm3.map.df.m <- melt(mCm3.map.df)

#add map to dataframe
all_Cm3["MAP"] <- mCm3.map.df.m$value


### Subtype C: model 4 ######
# gets the element births. Births are the number of new HIV cases
bCm4_1000 <- C_m4_o.1000[2,]
times_Cm4 <- C_m4_o.1000[[1]]

# calculate the median and quantiles for pafs
all_Cm4 <- births_pafs(bCm4_1000, times_Cm4)
# calculate pafs for MAP
mCm4_pafs_map <- t(sapply(mC_m4_map_o[[2]], calculate_pafs))

#convert it to dataframe and transform to long format
mCm4.map.df <- as.data.frame(mCm4_pafs_map)
mCm4.map.df.m <- melt(mCm4.map.df)

#add map to dataframe
all_Cm4["MAP"] <- mCm4.map.df.m$value




# PLOTTING
p1 <- ggplot(all_m5, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("PAF - All subtypes: Model 5") + ylab("PAF") + theme_bw()

p2 <- ggplot(all_m7, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("PAF - All subtypes: Model 7") + ylab("PAF") + theme_bw()

p3 <- ggplot(all_02_AGm3, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("PAF - 02_AG: Model 3") + ylab("PAF") + theme_bw()

p4 <- ggplot(all_02_AGm4, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("PAF - 02_AG: Model 4") + ylab("PAF") + theme_bw()

p5 <- ggplot(all_Cm3, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("PAF - C: Model 3") + ylab("PAF") + theme_bw()

p6 <- ggplot(all_Cm4, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("PAF - C: Model 4") + ylab("PAF") + theme_bw()


quartz()
multiplot(p1, p2, cols=1)
multiplot(p3, p4, cols=1)
multiplot(p5, p6, cols=1)

# from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
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

