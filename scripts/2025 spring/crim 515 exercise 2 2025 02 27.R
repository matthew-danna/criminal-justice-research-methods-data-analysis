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
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                          "1wKXnsCgcEuipeU9Xv3xBUBAA-B2wprtq"))
calls$date <- as.Date(calls$date)
calls$month <- month(calls$date)
calls$months <- months(calls$date)

calls.map <- subset(calls, !is.na(calls$lat))

# Step 2
va.roads <- roads("VA", "Fairfax city")
va.outline <- county_subdivisions("VA", "Fairfax city")
va.water <- area_water("VA", "Fairfax city")

### sample maps
ggplot(va.roads) +
  geom_sf()

ggplot(calls.map, aes(x=lon,y=lat)) +
  geom_point()

ggplot() +
  geom_sf(data = va.outline) +
  geom_point(aes(x=lon, y=lat), color = "lightblue", 
             data = calls.map) + 
  theme_void()

ggplot() +
  geom_point(aes(x=lon, y=lat), color = "blue", data = calls.map) + 
  geom_sf(data = va.roads) +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust=.5), plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) + 
  labs(title = "A Title", subtitle = "Some other text")

### data subsets
data1 <- subset(calls.map, calls.map$year == '2024')
data2 <- subset(calls.map, calls.map$type == 'TRESPASSING')
data3 <- subset(calls.map, calls.map$type == 'SUSPICIOUS')
data4 <- subset(calls.map, calls.map$months == 'March')

ggplot(data1, aes(x=lon, y=lat, color = month)) + 
  geom_point()

ggplot() + 
  geom_point(aes(x = lon, y = lat), 
             data = data1, alpha = 0.15, size = 1.5) + 
  theme(legend.position="bottom")

# MAP 1 EXAMPLE
ggplot() +
  geom_sf(data = va.outline$geometry, fill = "transparent", 
          linewidth = 1, color = "black") +
  geom_sf(data = va.roads$geometry, alpha = 0.15) +
  geom_sf(data = va.water$geometry, fill = "lightblue") +
  geom_point(data = data1, 
             aes(x = lon, y = lat), size = 1.5, alpha = 0.05, 
             color = "orange") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

ggplot() + 
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., 
                     alpha = 0.01),  
                 size = 0.01, bins = 4, data = data1, 
                 geom = "polygon")

ggplot() + 
  geom_sf(data = va.outline) +
  geom_density2d(data = data1, aes(x = lon, y = lat), 
                 size = 0.15) +
  ggtitle("Hotspots, xxxx-2024")

ggplot() +
  geom_hex(aes(x = lon, y = lat), data = data1, bins = 30) +
  scale_fill_continuous(type = "viridis")

# MAP 2 EXAMPLE
ggplot() +
  geom_sf(data = va.outline$geometry, fill = "transparent", 
          linewidth = 1, color = "darkblue") +
  geom_sf(data = va.roads$geometry, alpha = 0.5) +
  geom_sf(data = va.water$geometry, fill = "lightblue") +
  geom_hex(aes(x = lon, y = lat), data = data1, bins = 30,
           alpha = 0.75) +
  scale_fill_continuous(type = "viridis") +
  theme_void()

# MAP 2 EXAMPLE
ggplot() +
  geom_sf(data = va.outline$geometry, fill = "transparent", linewidth = 1, color = "black") +
  geom_sf(data = va.roads$geometry, alpha = 0.4) +
  geom_sf(data = va.water$geometry, fill = "lightblue") +
  stat_density2d(data = data1, aes(x = lon, y = lat, 
                                   fill = ..level..), 
                 bins = 4, h = 0.01, geom = "polygon", 
                 alpha = 0.75) +
  scale_fill_viridis_c(option = "plasma") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

# MAP 3 EXAMPLE
ggplot() +
  geom_sf(data = va.outline$geometry, fill = "transparent", 
          linewidth = 1.0, color = "grey") +
  geom_sf(data = va.roads$geometry, alpha = 0.15) +
  geom_sf(data = va.water$geometry, fill = "lightblue") +
  geom_hex(aes(x = lon, y = lat), data = data3, bins = 15,
           alpha = 0.75) +
  scale_fill_continuous(type = "viridis") +
  theme_void() +
  facet_wrap(~year)

# MAP 4 EXAMPLES
leaflet(data1) %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = data1$type, 
             clusterOptions = markerClusterOptions())

leaflet(data1) %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, 
             popup = paste(
               "Call Type: ", data1$type, "<br>",
               "Date:", data1$date), 
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = va.outline)



