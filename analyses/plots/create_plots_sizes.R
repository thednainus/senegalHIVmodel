####### create plots
library(ggplot2)
library(phydynR)
library(reshape2)


# Load solved objects
load("analyses/plots/solved_objects_noMalex_1000reps.rda")
load("analyses/plots/solved_objects_noMalex_maps.rda")


# gets the element sizes. Sizes are the number of HIV cases
sizes_m2_1000 <- m2_o.1000[4,]

#re-organize demes by sizes element
o.sizes_m2_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                         sizes = sizes_m2_1000)

times.m2 <- m2_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m2_1000.mq <- median_and_quantiles(o.sizes_m2_1000, times.m2)
m2_1000.mq["group2"] <- ifelse(m2_1000.mq$group == "msm", "msm", "gp")


# calculate sizes for MAP
m2_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                      m2_map_o[4])

#convert it to dataframe and transform to long format
m2.map.df <- data.frame(gpm=m2_sizes_map[[1]][1:1000],
                        gpf=m2_sizes_map[[2]][1:1000],
                        msm=m2_sizes_map[[3]][1:1000])
m2.map.df.m <- melt(m2.map.df)

#add map to dataframe
m2_1000.mq["MAP"] <- m2.map.df.m$value


### Model 3 ########
# gets the element sizes
sizes_m3_1000 <- m3_o.1000[4,]

#re-organize demes by sizes element
o.sizes_m3_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                         sizes = sizes_m3_1000)

times.m3 <- m3_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m3_1000.mq <- median_and_quantiles(o.sizes_m3_1000, times.m3)
m3_1000.mq["group2"] <- ifelse(m3_1000.mq$group == "msm", "msm", "gp")

# calculate sizes for MAP
m3_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                      m3_map_o[4])

#convert it to dataframe and transform to long format
m3.map.df <- data.frame(gpm=m3_sizes_map[[1]][1:1000],
                        gpf=m3_sizes_map[[2]][1:1000],
                        msm=m3_sizes_map[[3]][1:1000])
m3.map.df.m <- melt(m3.map.df)

#add map to dataframe
m3_1000.mq["MAP"] <- m3.map.df.m$value

#### Model 4 #####
# gets the element sizes
sizes_m4_1000 <- m4_o.1000[4,]

#re-organize demes by sizes element
o.sizes_m4_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                         sizes = sizes_m4_1000)

times.m4 <- m4_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m4_1000.mq <- median_and_quantiles(o.sizes_m4_1000, times.m4)
m4_1000.mq["group2"] <- ifelse(m4_1000.mq$group == "msm", "msm", "gp")


# calculate sizes for MAP
m4_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                      m4_map_o[4])

#convert it to dataframe and transform to long format
m4.map.df <- data.frame(gpm=m4_sizes_map[[1]][1:1000],
                        gpf=m4_sizes_map[[2]][1:1000],
                        msm=m4_sizes_map[[3]][1:1000])
m4.map.df.m <- melt(m4.map.df)

#add map to dataframe
m4_1000.mq["MAP"] <- m4.map.df.m$value



###### by subtype 02_AG: Model 1 ############

# gets the element sizes
sizes_m02_AGm1_1000 <- m02_AG_m1_o.1000[4,]

#re-organize demes by sizes element
o.sizes_m02_AGm1_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                              sizes = sizes_m02_AGm1_1000)
times.m02_AGm1 <- m02_AG_m1_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m02_AGm1_1000.mq <- median_and_quantiles(o.sizes_m02_AGm1_1000, times.m02_AGm1)
m02_AGm1_1000.mq["group2"] <- ifelse(m02_AGm1_1000.mq$group == "msm", "msm", "gp")


# calculate sizes for MAP
m02_AGm1_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                      m02_AG_m1_map_o[4])

#convert it to dataframe and transform to long format
m02_AGm1.map.df <- data.frame(gpm=m02_AGm1_sizes_map[[1]][1:1000],
                              gpf=m02_AGm1_sizes_map[[2]][1:1000],
                              msm=m02_AGm1_sizes_map[[3]][1:1000])
m02_AGm1.map.df.m <- melt(m02_AGm1.map.df)

#add map to dataframe
m02_AGm1_1000.mq["MAP"] <- m02_AGm1.map.df.m$value




###### by subtype 02_AG: Model 2 ############
# gets the element sizes
sizes_m02_AGm2_1000 <- m02_AG_m2_o.1000[4,]

#re-organize demes by sizes element
o.sizes_m02_AGm2_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                               sizes = sizes_m02_AGm2_1000)

times.m02_AGm2 <- m02_AG_m2_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m02_AGm2_1000.mq <- median_and_quantiles(o.sizes_m02_AGm2_1000, times.m02_AGm2)
m02_AGm2_1000.mq["group2"] <- ifelse(m02_AGm2_1000.mq$group == "msm", "msm", "gp")


# calculate sizes for MAP
m02_AGm2_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                            m02_AG_m2_map_o[4])

#convert it to dataframe and transform to long format
m02_AGm2.map.df <- data.frame(gpm=m02_AGm2_sizes_map[[1]][1:1000],
                              gpf=m02_AGm2_sizes_map[[2]][1:1000],
                              msm=m02_AGm2_sizes_map[[3]][1:1000])
m02_AGm2.map.df.m <- melt(m02_AGm2.map.df)

#add map to dataframe
m02_AGm2_1000.mq["MAP"] <- m02_AGm2.map.df.m$value



##### by subtype C: model 1 ##########
# gets the element sizes
sizes_C_1000 <- C_m1_o.1000[4,]

#re-organize demes by sizes element
o.sizes_C_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                        sizes = sizes_C_1000)

times_Cm1 <- C_m1_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m1_C_1000.mq <- median_and_quantiles(o.sizes_C_1000, times_Cm1)
m1_C_1000.mq["group2"] <- ifelse(m1_C_1000.mq$group == "msm", "msm", "gp")


# calculate sizes for MAP
mCm1_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                            mC_m1_map_o[4])

#convert it to dataframe and transform to long format
mCm1.map.df <- data.frame(gpm=mCm1_sizes_map[[1]][1:1000],
                          gpf=mCm1_sizes_map[[2]][1:1000],
                          msm=mCm1_sizes_map[[3]][1:1000])
mCm1.map.df.m <- melt(mCm1.map.df)

#add map to dataframe
m1_C_1000.mq["MAP"] <- mCm1.map.df.m$value


##### by subtype C: model 2 ######
# gets the element sizes
sizes_Cm2_1000 <- C_m2_o.1000[4,]

#re-organize demes by sizes element
o.sizes_Cm2_1000 <- reorganize_deme_sizes(Nrep = 1000, Ntime = 1000,
                                          sizes = sizes_Cm2_1000)

times_Cm2 <- C_m2_o.1000[[1]]

# get the dataframe to plot tajectories for sizes (median and quantiles)
m2_C_1000.mq <- median_and_quantiles(o.sizes_Cm2_1000, times_Cm2)
m2_C_1000.mq["group2"] <- ifelse(m2_C_1000.mq$group == "msm", "msm", "gp")

# calculate sizes for MAP
mCm2_sizes_map <- reorganize_deme_sizes(Nrep = 1, Ntime = 1000,
                                        mC_m2_map_o[4])

#convert it to dataframe and transform to long format
mCm2.map.df <- data.frame(gpm=mCm2_sizes_map[[1]][1:1000],
                          gpf=mCm2_sizes_map[[2]][1:1000],
                          msm=mCm2_sizes_map[[3]][1:1000])
mCm2.map.df.m <- melt(mCm2.map.df)

#add map to dataframe
m2_C_1000.mq["MAP"] <- mCm2.map.df.m$value


quartz()


p1 <- ggplot(m2_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("All subtypes: Model 2") + ylab("sizes") + theme_bw()

p2 <- ggplot(m3_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("All subtypes: Model 3") + ylab("sizes") + theme_bw()

p3 <- ggplot(m4_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("All subtypes: Model 4") + ylab("sizes") + theme_bw()

p4 <- ggplot(m02_AGm1_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("Subtype 02_AG: Model 1") + ylab("sizes") + theme_bw()

p5 <- ggplot(m02_AGm2_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("Subtype 02_AG: Model 2") + ylab("sizes") + theme_bw()

p6 <- ggplot(m1_C_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("Subtype C: Model 1") + ylab("sizes") + theme_bw()

p7 <- ggplot(m2_C_1000.mq, aes(x=times)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha=0.3) +
  geom_line(aes(y = median, colour=group), linetype="solid") +
  geom_line(aes(y = MAP, colour=group), linetype="longdash") +
  facet_wrap(~ group2, scales = "free") +
  ggtitle("Subtype C: Model 2") + ylab("sizes") + theme_bw()

quartz()
multiplot(p1, p2, p3, cols=1)
quartz()
multiplot(p4, p5, cols=1)
quartz()
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

