####### create plots
library(ggplot2)
library(phydynR)
library(reshape2)
library(senegalHIVmodel)

# Load solved objects
load("analyses/plots/solved_objects/solved_objects_prevalence_1000reps_new.rda")
load("analyses/plots/solved_objects/solved_objects_prevalence_maps_new.rda")




### Model 5 ######
# gets the element births. Births are the number of new HIV cases
bm5_1000 <- m5_o.1000[2,]
times_m5 <- m5_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m5 <- births_newCases(bm5_1000, times_m5)
# calculate new cases for MAP
m5_nc_map <- t(sapply(m5_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m5.map.df <- as.data.frame(m5_nc_map)
m5.map.df.m <- melt(m5.map.df)

#add map to dataframe
all_m5["MAP"] <- m5.map.df.m$value


### Model 6 ######
# gets the element births. Births are the number of new HIV cases
bm6_1000 <- m6_o.1000[2,]
times_m6 <- m6_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m6 <- births_newCases(bm6_1000, times_m6)
# calculate new cases for MAP
m6_nc_map <- t(sapply(m6_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m6.map.df <- as.data.frame(m6_nc_map)
m6.map.df.m <- melt(m6.map.df)

#add map to dataframe
all_m6["MAP"] <- m6.map.df.m$value


### Model 7 ######
# gets the element births. Births are the number of new HIV cases
bm7_1000 <- m7_o.1000[2,]
times_m7 <- m7_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m7 <- births_newCases(bm7_1000, times_m7)
# calculate new cases for MAP
m7_nc_map <- t(sapply(m7_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m7.map.df <- as.data.frame(m7_nc_map)
m7.map.df.m <- melt(m7.map.df)

#add map to dataframe
all_m7["MAP"] <- m7.map.df.m$value




### by subtype 02_AG: model 3 ######
# gets the element births. Births are the number of new HIV cases
bm3_02AG_1000 <- m02_AG_m3_o.1000[2,]
times_m3_02AG <- m02_AG_m3_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m3_02AG <- births_newCases(bm3_02AG_1000, times_m3_02AG)
# calculate new cases for MAP
m3_02AG_nc_map <- t(sapply(m02_AG_m3_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m3_02AG.map.df <- as.data.frame(m3_02AG_nc_map)
m3_02AG.map.df.m <- melt(m3_02AG.map.df)

#add map to dataframe
all_m3_02AG["MAP"] <- m3_02AG.map.df.m$value




### by subtype 02_AG: model 4 ######
# gets the element births. Births are the number of new HIV cases
bm4_02AG_1000 <- m02_AG_m4_o.1000[2,]
times_m4_02AG <- m02_AG_m4_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m4_02AG <- births_newCases(bm4_02AG_1000, times_m4_02AG)
# calculate new cases for MAP
m4_02AG_nc_map <- t(sapply(m02_AG_m4_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m4_02AG.map.df <- as.data.frame(m4_02AG_nc_map)
m4_02AG.map.df.m <- melt(m4_02AG.map.df)

#add map to dataframe
all_m4_02AG["MAP"] <- m4_02AG.map.df.m$value




### by subtype C: model 3 ######
# gets the element births. Births are the number of new HIV cases
bm3_C_1000 <- C_m3_o.1000[2,]
times_m3_C <- C_m3_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m3_C <- births_newCases(bm3_C_1000, times_m3_C)
# calculate new cases for MAP
m3_C_nc_map <- t(sapply(mC_m3_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m3_C.map.df <- as.data.frame(m3_C_nc_map)
m3_C.map.df.m <- melt(m3_C.map.df)

#add map to dataframe
all_m3_C["MAP"] <- m3_C.map.df.m$value


### by subtype C: model 4 ######
# gets the element births. Births are the number of new HIV cases
bm4_C_1000 <- C_m4_o.1000[2,]
times_m4_C <- C_m4_o.1000[[1]]

# calculate the median and quantiles for new cases
all_m4_C <- births_newCases(bm4_C_1000, times_m4_C)
# calculate new cases for MAP
m4_C_nc_map <- t(sapply(mC_m4_map_o[[2]], calculate_newCases))

#convert it to dataframe and transform to long format
m4_C.map.df <- as.data.frame(m4_C_nc_map)
m4_C.map.df.m <- melt(m4_C.map.df)

#add map to dataframe
all_m4_C["MAP"] <- m4_C.map.df.m$value

head(all_m4_C)

# PLOTTING
p1 <- ggplot(all_m5, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - All subtypes: Model 5") + ylab("New Cases") + theme_bw()

p2 <- ggplot(all_m6, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - All subtypes: Model 6") + ylab("New Cases") + theme_bw()

p3 <- ggplot(all_m7, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - All subtypes: Model 7") + ylab("New Cases") + theme_bw()

p4 <- ggplot(all_m3_02AG, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - 02_AG: Model 3") + ylab("New Cases") + theme_bw()

p5 <- ggplot(all_m4_02AG, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - 02_AG: Model 4") + ylab("New Cases") + theme_bw()

p6 <- ggplot(all_m3_C, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - C: Model 3") + ylab("New Cases") + theme_bw()

p7 <- ggplot(all_m4_C, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("New Cases - C: Model 4") + ylab("New Cases") + theme_bw()

quartz()
multiplot(p1, p2, cols=1)
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

