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

nflis.2022 <- subset(nflis.2022, !is.na(nflis.2022$Drug))

# flip the rows and columns
nflis.names <- nflis.2022$Drug
nflis <- as.data.frame(t(nflis.2022[,-1]))
colnames(nflis) <- nflis.names

### CDC
# read in the CDC provisional drug overdose data while you can
cdc.ods <- read.csv("https://data.cdc.gov/api/views/xkb8-kh2a/rows.csv?accessType=DOWNLOAD&bom=true&format=true",
                    stringsAsFactors = FALSE)
# remove a useless value
cdc.ods <- subset(cdc.ods, cdc.ods$Indicator != 'Percent with drugs specified')

# formats data.value as a number
cdc.ods$count <- gsub(",", "", cdc.ods$Data.Value)
cdc.ods$count <- as.numeric(cdc.ods$count)
cdc.ods$count[is.na(cdc.ods$count)] <- 0

# summarize by state, year, and indicator
cdc.state <- cdc.ods %>%
  group_by(State.Name, Year, Indicator) %>%
  summarise(count.total = sum(count))

### MDS!
# calculate MDS
mds.nflis <- cmdscale(dist(nflis))

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
clusters <- kmeans(mds.nflis, centers = 6)$cluster

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
          size = 3,
          ellipse = TRUE,
          ellipse.type = "convex",
          title = "K-means Clustering of MDS State NFLIS Data",
          xlab = "MDS Dimension 1",
          ylab = "MDS Dimension 2") +
  geom_text_repel(aes(label = state), box.padding = 0.25)
