library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)

crime <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                          "1e2q4yerhe4Md0kBfHE8pNVOVCAPaRkgx"))

# filter by crime types

crime.map <- subset(crime, !is.na(crime$lat))

va.roads <- roads("VA", "Fairfax city")
va.outline <- county_subdivisions("VA", "Fairfax city")
va.placenames <- landmarks("VA")
va.water <- area_water("VA", "Fairfax city")

fairfax.population <- get_acs(
  state = "VA",
  county = "Fairfax city",
  geography = "cbg",
  variables = "B01003_001",
  geometry = TRUE,
  year = 2023
)

fairfax.income <- get_acs(
  state = "VA",
  county = "Fairfax city",
  geography = "cbg",
  variables = "B19013_001",
  geometry = TRUE,
  year = 2023
)

fairfax.population %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 


# joins
crime.sf <- st_as_sf(x = crime.map, coords = c("lon", "lat"), crs = st_crs(fairfax.population))

fairfax1 <- st_join(fairfax.population, crime.sf,
                             join = st_intersects)

# sum by crime count
fairfax.sum <- fairfax1 %>%
  group_by(GEOID) %>%
  summarise(COUNT = n())

# calculate block area
fairfax.sum$area <- st_area(fairfax.sum)

# join population
fairfax.pop <- st_drop_geometry(fairfax.population)
fairfax.pop <- fairfax.pop[c(1,4)]
fairfax.sum <- fairfax.sum %>%
  left_join(fairfax.pop, by = 'GEOID')

# calculate rate
fairfax.sum$crime.rate <- fairfax.sum$COUNT/fairfax.sum$estimate * 1000

# pick a color
# https://ggplot2.tidyverse.org/reference/scale_viridis.html
fairfax.sum %>%
  ggplot(aes(fill = crime.rate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "H") +
  labs(
    title = "Crime Rate",
    subtitle = "All Crime, 2007-2024: Per 1,000 people",
    fill = "Crime Rate"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

  
