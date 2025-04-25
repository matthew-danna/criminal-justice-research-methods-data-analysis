### LIBRARIES
install.packages('tidyverse')
install.packages('cluster')
install.packages('ggpubr')
install.packages('ggrepel')
library(tidyverse)
library(cluster)
library(ggpubr)
library(ggrepel)

### NFLIS
# read in 2022 NFLIS data
nflis.2022 <- read.csv("C:/Users/mdanna2/Downloads/2022NFLIS.csv",
                       stringsAsFactors = FALSE)

# flip the rows and columns
nflis.names <- nflis.2022$Drug
nflis <- as.data.frame(t(nflis.2022[,-1]))
colnames(nflis) <- nflis.names

nflis.full <- nflis
nflis.full$State <- row.names(nflis)

####
#### MDS for a subset of drugs (NFLIS only)
####
############## ADJUST THESE TO MAKE THEM YOUR OWN!
drugs.subset <- subset(nflis.2022, nflis.2022$Drug == 'Cocaine' |
                         nflis.2022$Drug == 'Methamphetamine' |
                         nflis.2022$Drug == 'Heroin')
subset.names <- drugs.subset$Drug
subset <- as.data.frame(t(drugs.subset[,-1]))
colnames(subset) <- subset.names

# calculate MDS
mds.nflis <- cmdscale(dist(subset))

# empty plot
plot(mds.nflis[, 1], mds.nflis[, 2], 
     type = "n", xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# add points
points(mds.nflis[, 1], mds.nflis[, 2], 
       pch = 21, bg = "lightblue")
text(mds.nflis[, 1], mds.nflis[, 2], 
     labels = row.names(nflis), 
     pos = 3, cex = 0.8)

# calculate k means
clusters <- kmeans(mds.nflis, centers = 5)$cluster

# add to plot
points(mds.nflis[, 1], mds.nflis[, 2], 
       pch = 21, bg = clusters, cex = 1.2)

# update MDS table with clusters
mds.df <- as.data.frame(mds.nflis)
mds.df$groups <- as.factor(clusters)
mds.df$state <- row.names(nflis)

# updated cluster plot
ggscatter(mds.df, x = "V1", y = "V2",
          color = "groups",
          palette = "jco",
          size = 2.5,
          ellipse = TRUE,
          ellipse.type = "convex",
          title = "K-means Clustering of MDS State NFLIS Data",
          xlab = "MDS Dimension 1",
          ylab = "MDS Dimension 2") +
  geom_text_repel(aes(label = state), box.padding = 0.25)

####
#### MDS on drugs with states as the dimensions
####

states.nflis <- nflis.2022
row.names(states.nflis) <- states.nflis$Drug
states.nflis <- states.nflis[,2:52]

# calculate MDS
mds.nflis <- cmdscale(dist(states.nflis))

# empty plot
plot(mds.nflis[, 1], mds.nflis[, 2], 
     type = "n", xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# add points
points(mds.nflis[, 1], mds.nflis[, 2], 
       pch = 21, bg = "lightblue")
text(mds.nflis[, 1], mds.nflis[, 2], 
     labels = row.names(states.nflis), 
     pos = 3, cex = 0.8)

# calculate k means
clusters <- kmeans(mds.nflis, centers = 3)$cluster

# add to plot
points(mds.nflis[, 1], mds.nflis[, 2], 
       pch = 21, bg = clusters, cex = 1.2)

# update MDS table with clusters
mds.df <- as.data.frame(mds.nflis)
mds.df$groups <- as.factor(clusters)
mds.df$state <- row.names(states.nflis)

# updated cluster plot
ggscatter(mds.df, x = "V1", y = "V2",
          color = "groups",
          palette = "jco",
          size = 2.5,
          ellipse = TRUE,
          ellipse.type = "convex",
          title = "K-means Clustering of MDS State NFLIS Data",
          xlab = "MDS Dimension 1",
          ylab = "MDS Dimension 2") +
  geom_text_repel(aes(label = state), box.padding = 0.25)

###
###
### CDC
###
###
###

# read in the CDC provisional drug overdose data while you can
cdc.ods <- read.csv("https://data.cdc.gov/api/views/xkb8-kh2a/rows.csv?accessType=DOWNLOAD&bom=true&format=true",
                    stringsAsFactors = FALSE)
# remove a useless value
cdc.ods <- subset(cdc.ods, 
                  cdc.ods$Indicator != 'Percent with drugs specified')

# formats data.value as a number
cdc.ods$count <- gsub(",", "", cdc.ods$Data.Value)
cdc.ods$count <- as.numeric(cdc.ods$count)
cdc.ods$count[is.na(cdc.ods$count)] <- 0

# summarize by state, year, and indicator
cdc.state <- cdc.ods %>%
  group_by(State.Name, Year, Indicator) %>%
  summarise(count.total = sum(count))

# filter to 2022 AND only state AND a select set of drugs
cdc.state.2022 <- subset(cdc.state, cdc.state$Year == '2022' &
                           cdc.state$State.Name != 'United States' &
######## ADJUST THIS TO MAKE IT YOUR OWN
                            (cdc.state$Indicator == 'Cocaine (T40.5)' |
                              cdc.state$Indicator == 'Heroin (T40.1)' |
                              cdc.state$Indicator == 'Opioids (T40.0-T40.4,T40.6)'))

# subset by each indicator
cdc1 <- subset(cdc.state.2022, cdc.state.2022$Indicator == 'Cocaine (T40.5)')
cdc2 <- subset(cdc.state.2022, cdc.state.2022$Indicator == 'Heroin (T40.1)')
cdc3 <- subset(cdc.state.2022, cdc.state.2022$Indicator == 'Opioids (T40.0-T40.4,T40.6)')

colnames(cdc1) <- c("State", "Year", "Indicator", "Cocaine")
colnames(cdc2) <- c("State", "Year", "Indicator", "Heroin")
colnames(cdc3) <- c("State", "Year", "Indicator", "Opioids")

cdc <- cdc1 %>%
  left_join(cdc2, by = 'State')
cdc <- cdc %>%
  left_join(cdc3, by = 'State')
cdc <- cdc[c(1,4,7,10)]

####
#### MDS on overdoses with drugs as the dimensions
####

cdc.temp <- cdc
cdc <- cdc[c(2:4)]
row.names(cdc) <- cdc.temp$State

# calculate MDS
mds.cdc <- cmdscale(dist(cdc))

# empty plot
plot(mds.cdc[, 1], mds.cdc[, 2], 
     type = "n", xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# add points
points(mds.cdc[, 1], mds.cdc[, 2], 
       pch = 21, bg = "lightblue")
text(mds.cdc[, 1], mds.cdc[, 2], 
     labels = row.names(cdc), 
     pos = 3, cex = 0.8)

# calculate k means
clusters <- kmeans(mds.cdc, centers = 6)$cluster

# add to plot
points(mds.cdc[, 1], mds.cdc[, 2], 
       pch = 21, bg = clusters, cex = 1.2)

# update MDS table with clusters
mds.df <- as.data.frame(mds.cdc)
mds.df$groups <- as.factor(clusters)
mds.df$state <- row.names(cdc)

# updated cluster plot
ggscatter(mds.df, x = "V1", y = "V2",
          color = "groups",
          palette = "jco",
          size = 2.5,
          ellipse = TRUE,
          ellipse.type = "convex",
          title = "K-means Clustering of MDS State CDC Data",
          xlab = "MDS Dimension 1",
          ylab = "MDS Dimension 2") +
  geom_text_repel(aes(label = state), box.padding = 0.25)

#####
##### COMBINE NFLIS AND CDC
#####

colnames(cdc.temp) <- c("State", "OD.Cocaine", "OD.Heroin", 'OD.Opioids')
cdc.temp$State <- gsub(" ", ".", cdc.temp$State)
nflis.full$State <- gsub("District.of..Columbia", "District.of.Columbia",
                         nflis.full$State)

combo <- nflis.full %>%
  left_join(cdc.temp, by = 'State')

combo2 <- combo[c(62:64,32,16,26)]
row.names(combo2) <- combo$State

# calculate MDS
mds.combo <- cmdscale(dist(combo2))

# empty plot
plot(mds.combo[, 1], mds.combo[, 2], 
     type = "n", xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# add points
points(mds.combo[, 1], mds.combo[, 2], 
       pch = 21, bg = "lightblue")
text(mds.combo[, 1], mds.combo[, 2], 
     labels = row.names(combo2), 
     pos = 3, cex = 0.8)

# calculate k means
clusters <- kmeans(mds.combo, centers = 6)$cluster

# add to plot
points(mds.combo[, 1], mds.combo[, 2], 
       pch = 21, bg = clusters, cex = 1.2)

# update MDS table with clusters
mds.df <- as.data.frame(mds.combo)
mds.df$groups <- as.factor(clusters)
mds.df$state <- row.names(combo2)

# updated cluster plot
ggscatter(mds.df, x = "V1", y = "V2",
          color = "groups",
          palette = "jco",
          size = 2.5,
          ellipse = TRUE,
          ellipse.type = "convex",
          title = "K-means Clustering of MDS State CDC + NFLIS Data",
          xlab = "MDS Dimension 1",
          ylab = "MDS Dimension 2") +
  geom_text_repel(aes(label = state), box.padding = 0.25)
