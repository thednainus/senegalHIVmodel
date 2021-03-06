# Senegal data
# Reading metadata for Senegal only samples and close global reference (CGR)
# samples
SN.data <- read.csv(system.file("data/HIV_subtypes_summary_SENEGAL_noDups.csv", package = "senegalHIVmodel"))

SN.data.2 <- subset(SN.data,
                    is.na(Risk_group) == FALSE & Risk_group != "Children" & is.na(Sex) == FALSE)

SN.data.2$Location <- as.character(SN.data.2$Location)
SN.data.2$Location <- as.factor(SN.data.2$Location)

SN.data.2$Risk_group <- as.character(SN.data.2$Risk_group)
SN.data.2$Risk_group <- as.factor(SN.data.2$Risk_group)

SN.data.2$Subtype <- as.character(SN.data.2$Subtype)
SN.data.2$Subtype <- as.factor(SN.data.2$Subtype)


# Bar graph of counts
SN_plot <- ggplot(data=SN.data.2, aes(x=tolower(Location))) +
  geom_bar(stat="count")

SN_plot + facet_grid(Risk_group ~ ., scales="free")

SN_plot2 <- ggplot(data=SN.data.2, aes(x=Risk_group)) +
  geom_bar(stat="count")
SN_plot2 + facet_grid(. ~ Subtype)


SN_plot <- ggplot(data=SN.data.2, aes(x=Location)) +
  geom_bar(stat="count")

SN_plot + facet_grid(Risk_group ~ .)

# getting quantiles for the whole Senegal data, removing NA
SN.data.2$Age <- as.character(SN.data.2$Age)
SN.data.2$Age <- as.numeric(SN.data.2$Age)
quantile(SN.data.2$Age, na.rm = TRUE)

quantile(SN.data.2$Age, na.rm = TRUE, probs=c(0.025, 0.975))
median(SN.data.2$Age, na.rm = TRUE)

# get number of males and females
table(SN.data.2$Sex)

#get number of individuals by Location
table(SN.data.2$Location)
