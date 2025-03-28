# packages
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(corrr)
library(corrplot)

# Create a function for correlations
correlation.function <- function(data,
                                 method = "pearson",
                                 sig.level = 0.05,
                                 order = "original",
                                 diag = FALSE,
                                 type = "upper",
                                 tl.srt = 90,
                                 number.font = 1,
                                 number.cex = 1,
                                 mar = c(0, 0, 0, 0)) {
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficients on the diagonal
           diag = diag
  )
} 

# data
crime <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                          "1U_52BvjANpybxD7YkUNd-4Vlj-l5o5hu"))

# filter for IPV crimes
ipv <- subset(crime, crime$incident.type == 'DOMESTIC')

# filter for IPV crimes with location data
ipv.map <- subset(ipv, !is.na(ipv$lat))

# get geographic data
va.roads <- roads("VA", "Fairfax city")
va.outline <- county_subdivisions("VA", "Fairfax city")
va.placenames <- landmarks("VA")
va.water <- area_water("VA", "Fairfax city")

# get population counts per Census Block Group
fairfax.population <- get_acs(
  state = "VA",
  county = "Fairfax city",
  geography = "cbg",
  variables = "B01003_001",
  geometry = TRUE,
  year = 2023
)

# get Median Household Income per Census Block Group
fairfax.income <- get_acs(
  state = "VA",
  county = "Fairfax city",
  geography = "cbg",
  variables = "B19013_001",
  geometry = TRUE,
  year = 2023
)

# Make IPV crimes a mappable file
ipv.sf <- st_as_sf(x = ipv.map, coords = c("lon", "lat"), 
                   crs = st_crs(fairfax.population))

# join the IPV crimes to the population areas
crime.population <- st_join(fairfax.population, ipv.sf,
                             join = st_intersects)

# areas by crime count
ipv.sum <- crime.population %>%
  group_by(GEOID) %>%
  summarise(COUNT = n())

# calculate block area
ipv.sum$AREA <- st_area(ipv.sum)
ipv.sum$AREA <- as.numeric(ipv.sum$AREA)

# join population to crime counts
fairfax.pop <- st_drop_geometry(fairfax.population)
fairfax.pop <- fairfax.pop[c(1,4)]
ipv.sum <- ipv.sum %>%
  left_join(fairfax.pop, by = 'GEOID')

# calculate crimes per 1,000 people 
ipv.sum$ipv.rate <- ipv.sum$COUNT/ipv.sum$estimate * 1000


# join income to the master file
fairfax.inc <- st_drop_geometry(fairfax.income)
fairfax.inc <- fairfax.inc[c(1,4)]
ipv.sum <- ipv.sum %>%
  left_join(fairfax.inc, by = 'GEOID')

# change table names
names(ipv.sum) <- c("GEOID", "IPV.COUNT", "geometry", "AREA", 
                        "POPULATION", "IPV.RATE", "INCOME")

# join all crime counts to master file
crime.map <- subset(crime, !is.na(crime$lat))

crime.sf <- st_as_sf(x = crime.map, coords = c("lon", "lat"),
                          crs = st_crs(fairfax.population))

fairfax.all <- st_join(ipv.sum, crime.sf,
                       join = st_intersects)

fairfax2 <- fairfax.all %>%
  group_by(GEOID) %>%
  summarise(ALL.CRIME = n())
fairfax2 <- st_drop_geometry(fairfax2)

fairfax.sum <- ipv.sum %>%
  left_join(fairfax2, by = 'GEOID')

# calculate IPV crimes per 100 total crimes
fairfax.sum$CRIME.RATE <- fairfax.sum$IPV.COUNT/fairfax.sum$ALL.CRIME * 100

# calculate IPVs per area
fairfax.sum$IPV.AREA <- fairfax.sum$IPV.COUNT/fairfax.sum$AREA * 100000

# reorder the columns
fairfax.sum <- fairfax.sum[c(1,2,6,9,10,7,8,5,4,3)]

# calculate correlations
fairfax.corr <- st_drop_geometry(fairfax.sum)
fairfax.corr <- fairfax.corr[c(2:9)]
correlate.counts <- correlate(fairfax.corr)

correlation.function(
    data = fairfax.corr,
    method = "pearson",
    sig.level = 0.05, # adjust accordingly, 0.05 is the social science standard
    order = "original",
    diag = FALSE,
    type = "upper",
    tl.srt = 75
)

correlate.counts %>% network_plot(min_cor = .01, colors = c("red","blue"))

##### VISUALS
# map population
fairfax.population %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma")

# map income
fairfax.income %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 

# pick a color
# https://ggplot2.tidyverse.org/reference/scale_viridis.html

# maps
fairfax.sum %>%
  ggplot(aes(fill = IPV.RATE)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "H") +
  labs(
    title = "Stuff",
    subtitle = "Something something something",
    fill = "The data I used"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

