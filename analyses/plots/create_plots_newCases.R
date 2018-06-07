####### create plots
library(ggplot2)
library(phydynR)
library(reshape2)


# Load solved objects
#load("analyses/plots/solved_objects_noMalex.rda")
load("analyses/plots/solved_objects_noMalex_1000reps.rda")
load("analyses/plots/solved_objects_noMalex_maps.rda")




### Model 2 ######
# gets the element births. Births are the number of new HIV cases
bm2_1000 <- m2_o.1000[2,]
times_m2 <- m2_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m2 <- births_newCases(bm2_1000, times_m2)
# calculate new cases for MAP
m2_nc_map <- t(sapply(m2_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m2.map.df <- as.data.frame(m2_nc_map)
m2.map.df.m <- melt(m2.map.df)

#add map to dataframe
all_m2["MAP"] <- m2.map.df.m$value

### Model 3 ######
# gets the element births. Births are the number of new HIV cases
bm3_1000 <- m3_o.1000[2,]
times_m3 <- m3_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m3 <- births_newCases(bm3_1000, times_m3)
# calculate new cases for MAP
m3_nc_map <- t(sapply(m3_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m3.map.df <- as.data.frame(m3_nc_map)
m3.map.df.m <- melt(m3.map.df)

#add map to dataframe
all_m3["MAP"] <- m3.map.df.m$value


### Model 4 ######
# gets the element births. Births are the number of new HIV cases
bm4_1000 <- m4_o.1000[2,]
times_m4 <- m4_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m4 <- births_newCases(bm4_1000, times_m4)
# calculate new cases for MAP
m4_nc_map <- t(sapply(m4_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m4.map.df <- as.data.frame(m4_nc_map)
m4.map.df.m <- melt(m4.map.df)

#add map to dataframe
all_m4["MAP"] <- m4.map.df.m$value


### by subtype 02_AG: model 1 ######
# gets the element births. Births are the number of new HIV cases
bm1_02AG_1000 <- m02_AG_m1_o.1000[2,]
times_m1_02AG <- m02_AG_m1_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m1_02AG <- births_newCases(bm1_02AG_1000, times_m1_02AG)
# calculate new cases for MAP
m1_02AG_nc_map <- t(sapply(m02_AG_m1_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m1_02AG.map.df <- as.data.frame(m1_02AG_nc_map)
m1_02AG.map.df.m <- melt(m1_02AG.map.df)

#add map to dataframe
all_m1_02AG["MAP"] <- m1_02AG.map.df.m$value




### by subtype 02_AG: model 2 ######
# gets the element births. Births are the number of new HIV cases
bm2_02AG_1000 <- m02_AG_m2_o.1000[2,]
times_m2_02AG <- m02_AG_m2_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m2_02AG <- births_newCases(bm2_02AG_1000, times_m2_02AG)
# calculate new cases for MAP
m2_02AG_nc_map <- t(sapply(m02_AG_m2_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m2_02AG.map.df <- as.data.frame(m2_02AG_nc_map)
m2_02AG.map.df.m <- melt(m2_02AG.map.df)

#add map to dataframe
all_m2_02AG["MAP"] <- m2_02AG.map.df.m$value




### by subtype C: model 1 ######
# gets the element births. Births are the number of new HIV cases
bm1_C_1000 <- C_m1_o.1000[2,]
times_m1_C <- C_m1_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m1_C <- births_newCases(bm1_C_1000, times_m1_C)
# calculate new cases for MAP
m1_C_nc_map <- t(sapply(mC_m1_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m1_C.map.df <- as.data.frame(m1_C_nc_map)
m1_C.map.df.m <- melt(m1_C.map.df)

#add map to dataframe
all_m1_C["MAP"] <- m1_C.map.df.m$value


### by subtype C: model 2 ######
# gets the element births. Births are the number of new HIV cases
bm2_C_1000 <- C_m2_o.1000[2,]
times_m2_C <- C_m2_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m2_C <- births_newCases(bm2_C_1000, times_m2_C)
# calculate new cases for MAP
m2_C_nc_map <- t(sapply(mC_m2_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m2_C.map.df <- as.data.frame(m2_C_nc_map)
m2_C.map.df.m <- melt(m2_C.map.df)

#add map to dataframe
all_m2_C["MAP"] <- m2_C.map.df.m$value

head(all_m2)

# PLOTTING
p1 <- ggplot(all_m2, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - All subtypes: Model 2") + ylab("New Cases") + theme_bw()


p2 <- ggplot(all_m3, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - All subtypes: Model 3") + ylab("New Cases") + theme_bw()

p3 <- ggplot(all_m4, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - All subtypes: Model 4") + ylab("New Cases") + theme_bw()

p4 <- ggplot(all_m1_02AG, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - 02_AG: Model 1") + ylab("New Cases") + theme_bw()

p5 <- ggplot(all_m2_02AG, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - 02_AG: Model 2") + ylab("New Cases") + theme_bw()

p6 <- ggplot(all_m1_C, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - C: Model 1") + ylab("New Cases") + theme_bw()

p7 <- ggplot(all_m2_C, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - C: Model 2") + ylab("New Cases") + theme_bw()

quartz()
multiplot(p1, p2, p3, cols=1)
multiplot(p4, p5, cols=1)
multiplot(p6, p7, cols=1)

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

