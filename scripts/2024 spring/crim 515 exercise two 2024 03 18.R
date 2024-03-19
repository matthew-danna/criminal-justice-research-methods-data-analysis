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
temp.calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                          "12PArV_t_kvonL098R6zSQt6PSTmP1GMt"))
calls <- subset(temp.calls, !is.na(temp.calls$lat))

# Step 2
va.roads <- roads("VA", "Fairfax city")
va.outline <- county_subdivisions("VA", "Fairfax city")

# road map
ggplot(va.roads) +
  geom_sf()

# pin map
ggplot(calls, aes(x=lon, y=lat, color = "red")) + 
  geom_point()

# pin map with a city outline
ggplot() +
  geom_sf(data = va.outline) +
  geom_point(aes(x=lon, y=lat, color = "red"), data = calls) + 
  theme_void()

# pin map with roads and titles
ggplot() +
  geom_point(aes(x=lon, y=lat, color = "red"), data = calls) + 
  geom_sf(data = va.roads) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text")

# pin map color coded by hour of day
ggplot(calls, aes(x=lon, y=lat, color = hour)) + 
  geom_point()

# pin map subset by a call type, color coded by hour
ggplot(subset(calls, calls$type == 'MISC'), 
       aes(x=lon, y=lat, color = hour)) + 
  geom_point()

# transparent pin map
ggplot() + 
  geom_point(aes(x = lon, y = lat), data = calls, alpha = 0.05, 
             size = 0.5) + 
  theme(legend.position="bottom")

# transparent pin map example
calls.map1 <- subset(calls,calls$type == 'NOISE COMPLAINT')
ggplot() + 
  geom_sf(data = va.outline) +
  geom_sf(data = va.roads, color = 'grey', size = 0.25) +
  geom_point(aes(x = lon, y = lat), data = calls.map1, alpha = 0.25, 
             size = 0.5) + 
  theme_void()

# density plot
ggplot() + 
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., 
                     alpha = 0.01),  
                 size = 0.01, bins = 5, data = calls, 
                 geom = "polygon")

# density plot example
calls.map2 <- subset(calls, calls$year > 2021)
ggplot() + 
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., 
                     alpha = 0.01),  
                 size = 0.01, bins = 5, data = calls.map2, 
                 geom = "polygon")

# contour map
ggplot() + 
  geom_sf(data = va.outline) +
  geom_density2d(data = calls.map2, aes(x = lon, y = lat), size = 0.15) +
  ggtitle("Hotspots, xxxx-2023")

# contour + density map
ggplot() + 
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., 
                     alpha = 0.01), 
                 size = 0.001, bins = 5, 
                 data = calls.map2, geom = "polygon") + 
  geom_density2d(data = calls.map2, aes(x = lon, y = lat), 
                 size = 0.15)

# interactive cluster map
leaflet(calls) %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = calls$type, 
             clusterOptions = markerClusterOptions())

leaflet(calls) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = calls$type, 
             clusterOptions = markerClusterOptions())

leaflet(calls) %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, 
             popup = paste(
               "Call Type: ", calls$type, "<br>",
               "Date:", calls$date), 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = va.outline)

# facet map?
ggplot() + 
  geom_sf(data = va.outline) +
  geom_sf(data = va.roads, color = 'grey', size = 0.25) +
  geom_point(aes(x = lon, y = lat), data = calls.map1, alpha = 0.25, 
             size = 0.5) + 
  theme_void() +
  facet_wrap(~year)






