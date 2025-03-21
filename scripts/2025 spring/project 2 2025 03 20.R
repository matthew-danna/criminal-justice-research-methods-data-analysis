# packages
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)

# data
crime <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                          "1e2q4yerhe4Md0kBfHE8pNVOVCAPaRkgx"))

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

# Make IPV crimes a mappable file
ipv.sf <- st_as_sf(x = ipv.map, coords = c("lon", "lat"), 
                   crs = st_crs(fairfax.population))

# join the IPV crimes to the population areas
fairfax1 <- st_join(fairfax.population, ipv.sf,
                             join = st_intersects)

# areas by crime count
fairfax.sum <- fairfax1 %>%
  group_by(GEOID) %>%
  summarise(COUNT = n())

# calculate block area
fairfax.sum$area <- st_area(fairfax.sum)

# join population to crime counts
fairfax.pop <- st_drop_geometry(fairfax.population)
fairfax.pop <- fairfax.pop[c(1,4)]
fairfax.sum <- fairfax.sum %>%
  left_join(fairfax.pop, by = 'GEOID')

# calculate crimes per 1,000 people 
fairfax.sum$crime.rate <- fairfax.sum$COUNT/fairfax.sum$estimate * 1000

# pick a color
# https://ggplot2.tidyverse.org/reference/scale_viridis.html

# map crimes by count
fairfax.sum %>%
  ggplot(aes(fill = COUNT)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "H") +
  labs(
    title = "Crime Count",
    subtitle = "IPV events, xxxx-2024: Per 1,000 people",
    fill = "Crime Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

# map crimes per 1,000 people
fairfax.sum %>%
  ggplot(aes(fill = crime.rate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "H") +
  labs(
    title = "Crime Rate",
    subtitle = "IPV events, xxxx-2024: Per 1,000 people",
    fill = "Crime Rate"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())

# join income to the master file
fairfax.inc <- st_drop_geometry(fairfax.income)
fairfax.inc <- fairfax.inc[c(1,4)]
fairfax.sum <- fairfax.sum %>%
  left_join(fairfax.inc, by = 'GEOID')

# change table names
names(fairfax.sum) <- c("GEOID", "CRIME.COUNT", "geometry", "area", 
                        "POPULATION", "CRIME.RATE", "INCOME")

# join all crime counts to master file
crime.2024 <- subset(crime, crime$year == '2024' &
                       !is.na(crime$lat))
crime.2024.sf <- st_as_sf(x = crime.2024, coords = c("lon", "lat"),
                          crs = st_crs(fairfax.population))

fairfax.all <- st_join(fairfax.sum, crime.2024.sf,
                       join = st_intersects)

fairfax2 <- fairfax.all %>%
  group_by(GEOID) %>%
  summarise(COUNT = n())
fairfax2 <- st_drop_geometry(fairfax2)
fairfax2 <- fairfax2[c(1,2)]
fairfax.sum <- fairfax.sum %>%
  left_join(fairfax2, by = 'GEOID')

# change table names
names(fairfax.sum) <- c("GEOID", "CRIME.COUNT", "geometry", "area", 
                        "POPULATION", "CRIME.POP", "INCOME", "ALL.CRIME")

# calculate IPV crimes per 100 total crimes
fairfax.sum$CRIME.RATE <- fairfax.sum$CRIME.COUNT/fairfax.sum$ALL.CRIME * 100

# join more features/rates to fairfax.sum
# calculate correlations