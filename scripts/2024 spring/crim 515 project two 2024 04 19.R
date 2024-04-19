# Step 0: libraries
install.packages(c("cowplot", "ggrepel", "rgeos", "sf", "maps", "usmap", "ggspatial", "libwgeom", "rnaturalearth", "rnaturalearthdata"))
install.packages('leaflet')
install.packages('tidycensus')
library(tidycensus)
library(leaflet)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(lubridate)
library(usmap)
library(sf)
theme_set(theme_bw())

# Step 1: get data
wapo.data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv", stringsAsFactors = FALSE)

# Step 2: clean data
wapo.data$date <- as.Date(wapo.data$date)
wapo.data$month <- substr(wapo.data$date, 6, 7)
wapo.data$year <- substr(wapo.data$date,0,4)
wapo.data$yearmonth <- paste(wapo.data$year, wapo.data$month, sep = "-")

unique(wapo.data$race)
wapo.data$race <- gsub("W;B;N", "O", wapo.data$race)
wapo.data$race <- gsub("N;H", "O", wapo.data$race)
wapo.data$race <- gsub("W;H", "O", wapo.data$race)
wapo.data$race <- gsub("B;H", "O", wapo.data$race)
wapo.data$race <- gsub("W;B", "O", wapo.data$race)
unique(wapo.data$race)

# Step 3: calculate data
### single variable summaries, with stats!
wapo.race <- wapo.data %>% 
  group_by(race) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))

wapo.race$mean <- mean(wapo.race$count)
wapo.race$sd <- sd(wapo.race$count)
wapo.race$median <- median(wapo.race$count)

wapo.state <- wapo.data %>% 
  group_by(state) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))
wapo.state$mean <- mean(wapo.state$count)
wapo.state$sd <- sd(wapo.state$count)
wapo.state$median <- median(wapo.state$count)

wapo.year <- wapo.data %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))

wapo.full.year <- subset(wapo.year, wapo.year$year < '2024')

wapo.year$mean <- mean(wapo.full.year$count)
wapo.year$sd <- sd(wapo.full.year$count)
wapo.year$median <- median(wapo.full.year$count)

# multi-variable summaries
wapo.race.mental <- wapo.data %>% 
  group_by(race, was_mental_illness_related) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)*100)

wapo.mental.race <- wapo.data %>% 
  group_by(was_mental_illness_related, race) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)*100)

wapo.race.sex <- wapo.data %>% 
  group_by(race, gender) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)*100)

# Step 4: visualize
### bar graphs
ggplot(wapo.data) + 
  geom_bar(aes(x = race), stat = "count", fill = "grey")
ggplot(wapo.data) + 
  geom_bar(aes(x = state), stat = "count", fill = "grey")

ggplot(wapo.race.mental, aes(x = factor(race), y = pct, 
                             fill = factor(was_mental_illness_related))) + 
  geom_bar(stat="identity", width = 0.7) + 
  labs(x = "Race", y = "Percent", fill = "Mental Illness") + 
  theme_minimal(base_size = 14)
ggplot(wapo.race.sex, aes(x = factor(race), y = pct, 
                             fill = factor(gender))) + 
  geom_bar(stat="identity", width = 0.7) + 
  labs(x = "Race", y = "Percent", fill = "Sex") + 
  theme_minimal(base_size = 14)

ggplot(wapo.data) + 
  geom_bar(aes(x = was_mental_illness_related), stat = "count", fill = "grey") + 
  facet_wrap(~ race, nrow = 3)

### maps
# clean
wapo.data.clean <- subset(wapo.data, !is.na(wapo.data$latitude))
wapo.data.clean$latitude <- round(as.numeric(wapo.data.clean$latitude),4)
wapo.data.clean$longitude <- round(as.numeric(wapo.data.clean$longitude),4)
wapo.data.clean <- subset(wapo.data.clean, wapo.data.clean$longitude > -160)

# background layers
world <- ne_countries(scale = "medium", returnclass = "sf") # this builds a list of countries
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # this cleans up the US states

# pin map
ggplot(data = world) + 
  geom_sf() + 
  geom_point(data = wapo.data.clean, aes(x = longitude, y = latitude), size = 2, shape = 23, 
             fill = "black") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data.clean, aes(x = longitude, y = latitude), 
             size = 2, shape = 1) + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data.clean, aes(x = longitude, y = latitude), 
             size = 2, alpha = 0.15) + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

# density
ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  stat_density_2d(data = wapo.data.clean, 
                  aes(x = longitude, y = latitude, fill = stat(level)), bins = 5, 
                  geom = "polygon") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_density_2d(data = wapo.data.clean, aes(x = longitude, y = latitude), bins = 5) + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data.clean, aes(x = longitude, y = latitude), size = 1, alpha = 0.20, shape = 1) + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE) +
  facet_wrap(~ year, nrow = 4)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_density_2d(data = wapo.data.clean, 
                  aes(x = longitude, y = latitude), bins = 5) + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE) + 
  facet_wrap(~ race, nrow = 3)

leaflet(wapo.data.clean) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions())

wapo.victim.black <- subset(wapo.data.clean, wapo.data.clean$race == 'B')
leaflet(wapo.victim.black) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions())

# Step 5: census data!
census_api_key("YOUR API KEY HERE", install = TRUE, overwrite = TRUE) # run once!
readRenviron("~/.Renviron")

census.variables.2022 <- load_variables(2022, "acs5", cache = TRUE)

race.2022 <- get_acs(geography = "state", 
                     variables = c("B02008_001", "B02009_001", "B02010_001", "B02011_001", 
                                   "B03001_003"), 
                     year = 2022)
race.2022$variable <- gsub("B02008_001", "White", race.2022$variable)
race.2022$variable <- gsub("B02009_001", "Black", race.2022$variable)
race.2022$variable <- gsub("B02010_001", "Native American", race.2022$variable)
race.2022$variable <- gsub("B02011_001", "Asian", race.2022$variable)
race.2022$variable <- gsub("B03001_003", "Hispanic", race.2022$variable)

race.total <- get_acs(geography = "us", 
                      variables = c("B02008_001", "B02009_001", "B02010_001", "B02011_001", 
                                    "B03001_003"), year = 2022)
race.total$variable <- gsub("B02008_001", "White", race.total$variable)
race.total$variable <- gsub("B02009_001", "Black", race.total$variable)
race.total$variable <- gsub("B02010_001", "Native American", race.total$variable)
race.total$variable <- gsub("B02011_001", "Asian", race.total$variable)
race.total$variable <- gsub("B03001_003", "Hispanic", race.total$variable)

totalpop.2022 <- get_acs(geography = "state", variables = "B01003_001", year = 2022)
names(totalpop.2022) <- c("GEOID", "State", "Variable", "Population", "junk")
totalpop.2022$State.PCT <- round(totalpop.2022$Population/sum(totalpop.2022$Population)*100, 2)

temp.race.population <- race.2022 %>% left_join(totalpop.2022, by = "GEOID")
race.population <- temp.race.population[c(1:4,8,10)]
names(race.population) <- c("GEOID", "State", "Race", "Population.Race", 
                            "Population.State", "State.PCT")
race.population$Race.PCT <- round(race.population$Population.Race/race.population$Population.State*100, 2)

race.total$PCT <- round(race.total$estimate/sum(totalpop.2022$Population)*100, 2)
names(race.total) <- c("GEOID", "Area", "Race", "Count", "moe", "Race.PCT")

wapo.race$race <- gsub("A", "Asian", wapo.race$race)
wapo.race$race <- gsub("B", "Black", wapo.race$race)
wapo.race$race <- gsub("H", "Hispanic", wapo.race$race)
wapo.race$race <- gsub("N", "Native American", wapo.race$race)
wapo.race$race <- gsub("O", "Other", wapo.race$race)
wapo.race$race <- gsub("W", "White", wapo.race$race)
names(wapo.race) <- c("Race", "Count", "Shooting.PCT", "Shooting.Mean", "Shooting.SD", 
                      "Shooting.Median")

wapo.state$State.Name <- state.name[match(wapo.state$state,state.abb)]
wapo.state$State.Name <- replace_na(wapo.state$State.Name, "District of Columbia")
names(wapo.state) <- c("oldstate", "Count", "Shooting.PCT", "Shooting.Mean", "Shooting.SD",
                       "Shooting.Median", "State")

wapo.census.race <- wapo.race %>% left_join(race.total, by = "Race")
wapo.census.race <- wapo.census.race[c(1:6,11)]
names(wapo.census.race) <- c("Race", "Shootings", "Shooting.PCT", "Shooting.Mean", 
                             "Shooting.SD", "Shooting.Median", "Race.PCT")

wapo.census.state <- wapo.state %>% left_join(totalpop.2022, by = "State")
wapo.census.state <- wapo.census.state[c(7,2:6,12)]



