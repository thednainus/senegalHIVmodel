####### create plots
library(ggplot2)
library(phydynR)
library(reshape2)


# Load solved objects
load("analyses/plots/solved_objects/solved_objects_prevalence_1000reps.rda")
load("analyses/plots/solved_objects/solved_objects_prevalence_maps.rda")


# gets the element sizes. Sizes are the number of HIV cases
sizes_m5_1000 <- m5_o.1000[4,]

#re-organize demes by sizes element
o.sizes_m5_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                         sizes = sizes_m5_1000)

times.m5 <- m5_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m5_1000.mq <- median_and_quantiles(o.sizes_m5_1000, times.m5)
m5_1000.mq["group2"] <- ifelse(m5_1000.mq$group == "msm", "msm", "gp")


# calculate sizes for MAP
m5_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                      m5_map_o[4])

#convert it to dataframe and transform to long format
m5.map.df <- data.frame(gpm=m5_sizes_map[[1]][1:1000],
                        gpf=m5_sizes_map[[2]][1:1000],
                        msm=m5_sizes_map[[3]][1:1000])
m5.map.df.m <- melt(m5.map.df)

#add map to dataframe
m5_1000.mq["MAP"] <- m5.map.df.m$value


### Model 3 ########
# gets the element sizes
sizes_m7_1000 <- m7_o.1000[4,]

#re-organize demes by sizes element
o.sizes_m7_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                         sizes = sizes_m7_1000)

times.m7 <- m7_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m7_1000.mq <- median_and_quantiles(o.sizes_m7_1000, times.m7)
m7_1000.mq["group2"] <- ifelse(m7_1000.mq$group == "msm", "msm", "gp")

# calculate sizes for MAP
m7_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                      m7_map_o[4])

#convert it to dataframe and transform to long format
m7.map.df <- data.frame(gpm=m7_sizes_map[[1]][1:1000],
                        gpf=m7_sizes_map[[2]][1:1000],
                        msm=m7_sizes_map[[3]][1:1000])
m7.map.df.m <- melt(m7.map.df)

#add map to dataframe
m7_1000.mq["MAP"] <- m7.map.df.m$value




###### by subtype 02_AG: Model 3 ############

# gets the element sizes
sizes_m02_AGm3_1000 <- m02_AG_m3_o.1000[4,]

#re-organize demes by sizes element
o.sizes_m02_AGm3_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                              sizes = sizes_m02_AGm3_1000)
times.m02_AGm3 <- m02_AG_m3_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m02_AGm3_1000.mq <- median_and_quantiles(o.sizes_m02_AGm3_1000, times.m02_AGm3)
m02_AGm3_1000.mq["group2"] <- ifelse(m02_AGm3_1000.mq$group == "msm", "msm", "gp")


# calculate sizes for MAP
m02_AGm3_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                      m02_AG_m3_map_o[4])

#convert it to dataframe and transform to long format
m02_AGm3.map.df <- data.frame(gpm=m02_AGm3_sizes_map[[1]][1:1000],
                              gpf=m02_AGm3_sizes_map[[2]][1:1000],
                              msm=m02_AGm3_sizes_map[[3]][1:1000])
m02_AGm3.map.df.m <- melt(m02_AGm3.map.df)

#add map to dataframe
m02_AGm3_1000.mq["MAP"] <- m02_AGm3.map.df.m$value




###### by subtype 02_AG: Model 4 ############
# gets the element sizes
sizes_m02_AGm4_1000 <- m02_AG_m4_o.1000[4,]

#re-organize demes by sizes element
o.sizes_m02_AGm4_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                               sizes = sizes_m02_AGm4_1000)

times.m02_AGm4 <- m02_AG_m4_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m02_AGm4_1000.mq <- median_and_quantiles(o.sizes_m02_AGm4_1000, times.m02_AGm4)
m02_AGm4_1000.mq["group2"] <- ifelse(m02_AGm4_1000.mq$group == "msm", "msm", "gp")


# calculate sizes for MAP
m02_AGm4_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                            m02_AG_m4_map_o[4])

#convert it to dataframe and transform to long format
m02_AGm4.map.df <- data.frame(gpm=m02_AGm4_sizes_map[[1]][1:1000],
                              gpf=m02_AGm4_sizes_map[[2]][1:1000],
                              msm=m02_AGm4_sizes_map[[3]][1:1000])
m02_AGm4.map.df.m <- melt(m02_AGm4.map.df)

#add map to dataframe
m02_AGm4_1000.mq["MAP"] <- m02_AGm4.map.df.m$value



##### by subtype C: model 3 ##########
# gets the element sizes
sizes_Cm3_1000 <- C_m3_o.1000[4,]

#re-organize demes by sizes element
o.sizes_Cm3_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                        sizes = sizes_Cm3_1000)

times_Cm3 <- C_m3_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m3_C_1000.mq <- median_and_quantiles(o.sizes_Cm3_1000, times_Cm3)
m3_C_1000.mq["group2"] <- ifelse(m3_C_1000.mq$group == "msm", "msm", "gp")


# calculate sizes for MAP
mCm3_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                            mC_m3_map_o[4])

#convert it to dataframe and transform to long format
mCm3.map.df <- data.frame(gpm=mCm3_sizes_map[[1]][1:1000],
                          gpf=mCm3_sizes_map[[2]][1:1000],
                          msm=mCm3_sizes_map[[3]][1:1000])
mCm3.map.df.m <- melt(mCm3.map.df)

#add map to dataframe
m3_C_1000.mq["MAP"] <- mCm3.map.df.m$value


##### by subtype C: model 4 ######
# gets the element sizes
sizes_Cm4_1000 <- C_m4_o.1000[4,]

#re-organize demes by sizes element
o.sizes_Cm4_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                          sizes = sizes_Cm4_1000)

times_Cm4 <- C_m4_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m4_C_1000.mq <- median_and_quantiles(o.sizes_Cm4_1000, times_Cm4)
m4_C_1000.mq["group2"] <- ifelse(m4_C_1000.mq$group == "msm", "msm", "gp")

# calculate sizes for MAP
mCm4_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                        mC_m4_map_o[4])

#convert it to dataframe and transform to long format
mCm4.map.df <- data.frame(gpm=mCm4_sizes_map[[1]][1:1000],
                          gpf=mCm4_sizes_map[[2]][1:1000],
                          msm=mCm4_sizes_map[[3]][1:1000])
mCm4.map.df.m <- melt(mCm4.map.df)

#add map to dataframe
m4_C_1000.mq["MAP"] <- mCm4.map.df.m$value




p1 <- ggplot(m5_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("All subtypes: Model 5") + ylab("sizes") + theme_bw()

p2 <- ggplot(m7_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("All subtypes: Model 7") + ylab("sizes") + theme_bw()

p3 <- ggplot(m02_AGm3_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("Subtype 02_AG: Model 3") + ylab("sizes") + theme_bw()

p4 <- ggplot(m02_AGm4_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("Subtype 02_AG: Model 4") + ylab("sizes") + theme_bw()

p5 <- ggplot(m3_C_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("Subtype C: Model 3") + ylab("sizes") + theme_bw()

p6 <- ggplot(m4_C_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("Subtype C: Model 4") + ylab("sizes") + theme_bw()

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

