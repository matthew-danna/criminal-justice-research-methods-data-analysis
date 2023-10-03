# Step 0
install.packages('tidyverse')
install.packages('leaflet')
install.packages('sf')
install.packages('tigris')
library(tidyverse)
library(leaflet)
library(sf)
library(tigris)

# Step 1
dc.data <- read.csv("https://opendata.arcgis.com/datasets/89561a4f02ba46cca3c42333425d1b87_5.csv", 
                    stringsAsFactors = FALSE) 

# Step 2
### Geographic reference data
dc.roads <- roads("DC", "District of Columbia")
dc.roads.major <- dc.roads %>%
  filter(RTTYP %in% c("I","S","U"))
dc.outline <- county_subdivisions("DC", "District of Columbia")

# empty map of roads
ggplot(dc.roads) + 
  geom_sf()

# points, one color
ggplot(dc.data, aes(x=LONGITUDE, y=LATITUDE, color = "red")) +
  geom_point()

# points with a city outline and clean background
ggplot() +
  geom_sf(data = dc.outline) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color="red"), data = dc.data) +
  theme_void()

# points with city outline, roads, a clean background, and titles
ggplot() +
  geom_sf(data= dc.outline) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color = "red"), data = dc.data) + 
  geom_sf(data = dc.roads.major) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, 
                                                                                     margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text")

# color-coded points
ggplot(dc.data, aes(x=LONGITUDE, y=LATITUDE, color = SHIFT)) + 
  geom_point()

# color-coded points for a specific offense
ggplot(subset(dc.data, dc.data$OFFENSE == 'HOMICIDE'), aes(x=LONGITUDE, y=LATITUDE, color = SHIFT)) + 
  geom_point()

# transparent points
ggplot() + 
  geom_point(aes(x=LONGITUDE, y=LATITUDE), data= dc.data, alpha = 0.05, size = 1.0) +
  theme(legend.position = "bottom")

### an example
### the xxx, yyy, and zzz are need to be manually changed
ggplot() +
  geom_sf(data = dc.outline) +
  geom_sf(data = dc.roads.major, color = "grey") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), data = subset(dc.data, dc.data$OFFENSE == 'xxx'), 
             alpha = yyy, size = zzz) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, 
                                                                                     margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text")

# density
ggplot() + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = 0.01),  
                 size = 0.01, bins = 50, data = dc.data, geom = "polygon")

# contour
ggplot() + 
  geom_sf(data = dc.outline) +
  geom_density2d(data = dc.data, aes(x = LONGITUDE, y = LATITUDE), size = 0.15)

# combination
ggplot() + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = 0.01), 
                 size = 0.001, bins = 10, data = dc.data, geom = "polygon") + 
  geom_density2d(data = dc.data, aes(x = LONGITUDE, y = LATITUDE), size = 0.15)

### an example
### the xxx and yyy need to be updated
ggplot() + 
  geom_sf(data = dc.outline) +
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = 0.01), 
                 size = 0.01, bins = xxx, data = subset(dc.data, dc.data$OFFENSE == 'yyy'), geom = "polygon") +
  geom_sf(data = dc.roads.major) +
  theme_void() +
  ggtitle("Update my title please") +
  theme(legend.position = "none")



