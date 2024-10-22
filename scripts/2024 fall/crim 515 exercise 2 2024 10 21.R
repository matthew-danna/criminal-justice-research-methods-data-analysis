# Step 0: Prep
install.packages('tidyverse')
install.packages('leaflet')
install.packages('sf')
install.packages('tigris')
library(tidyverse)
library(leaflet)
library(sf)
library(tigris)

# Step 1: Get Data
temp.calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                          "1gniFP35GEfxycgpKGHAzkgSYmjTteTKm"))
calls <- subset(temp.calls, !is.na(temp.calls$lat))

# Step 2: Map
va.roads <- roads("VA", "Fairfax city")
va.outline <- county_subdivisions("VA", "Fairfax city")

# empty map, just roads
ggplot(va.roads) + geom_sf()
# empty map, just outline
ggplot(va.outline) + geom_sf()
# empty map, roads and outline
ggplot() +
  geom_sf(data = va.outline) +
  geom_sf(data = va.roads)

# point map, one color
ggplot(calls, aes(x = lon, y = lat), color = "red") + 
  geom_point()

# points, roads, outline, and a title
ggplot() +
  geom_sf(data = va.outline) +
  geom_point(data = calls, aes(x = lon, y = lat), color = "blue") +
  geom_sf(data = va.roads) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text")

# Map 1: transparent points
map1 <- subset(calls, calls$type == 'NOISE COMPLAINT')

ggplot() +
  geom_point(data = map1, aes(x = lon, y = lat), color = 'blue',
             alpha = 0.1, size = 0.75)

ggplot() +
  geom_sf(data = va.outline) +
  geom_sf(data = va.roads) +
  geom_point(data = map1, aes(x = lon, y = lat), color = 'red',
             alpha = 0.1, size = 0.75) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Call Type?")

# Map 2: density
map2 <- subset(calls, calls$type == '911 HANG-UP')

## density
ggplot() + 
  stat_density2d(data = map2, aes(x = lon, y = lat, 
                                  fill = ..level.., alpha = 0.01),  
                 size = 0.01, bins = 7, geom = "polygon")

## contour
ggplot() +
  geom_density2d(data = map2, aes(x = lon, y = lat), size = 0.15)

## hexes
ggplot() +
  geom_hex(data = map2, aes(x = lon, y = lat), bins = 20) +
  scale_fill_continuous(type = "viridis")

ggplot() +
  geom_sf(data = va.outline) +
  geom_hex(data = map2, aes(x = lon, y = lat), bins = 20) +
  scale_fill_continuous(type = "viridis") + 
  geom_sf(data = va.roads, color = 'white', alpha = 0.35) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text")

# Map 3: facet
map3 <- subset(calls, calls$type == 'FAMILY FIGHT' |
                 calls$type == 'DOMESTIC')

ggplot() +
  geom_sf(data = va.outline) +
  geom_hex(data = map3, aes(x = lon, y = lat), bins = 10) +
  scale_fill_continuous(type = "viridis") + 
  geom_sf(data = va.roads, color = 'white', alpha = 0.25) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text") +
  facet_wrap(~ year)

# Map 4: interactive clusters
map4 <- subset(calls, calls$type == 'ASSAULT')

leaflet(map4) %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = map4$date,
             clusterOptions = markerClusterOptions())

leaflet(map4) %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = 
               paste(
                 "Call Type: ",map4$type,"<br>",
                 "Date: ", map4$date,"<br>",
                 "Report: ", map4$number),
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = va.outline)
