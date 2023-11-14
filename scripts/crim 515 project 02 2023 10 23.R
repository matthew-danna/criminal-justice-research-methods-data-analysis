# Libraries
install.packages(c("cowplot", "ggrepel", "rgeos", "sf", "maps", "usmap", "ggspatial", "libwgeom", 
                   "rnaturalearth", "rnaturalearthdata"))
library(gtrendsR)
library(tidyverse)
library(tidycensus)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(lubridate)
library(usmap)
library(sf)
library(leaflet)
library(plotly)
theme_set(theme_bw())


# Step 1: Data
## Police Shootings
wapo.data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv", 
                      stringsAsFactors = FALSE)

## Google Trends
today <- as.character(Sys.Date())
time.range <- paste("2015-01-01", today, sep = " ")
#trends1 <- gtrends("police shooting", geo = "US", time = time.range)
#plot(trends1)
#p <- plot(trends1)
#ggplotly(p)

# Step 2
wapo.data$date <- as.Date(wapo.data$date)
wapo.data$month <- substr(wapo.data$date, 6, 7)
wapo.data$year <- substr(wapo.data$date,0,4)
wapo.data$yearmonth <- paste(wapo.data$year, wapo.data$month, sep = "-")
wapo.data$race <- gsub("B;H", "O", wapo.data$race)

##### to find all the gun-related "armed_with" events
wapo.data$armed <- ""
wapo.data.gun <- subset(wapo.data, wapo.data$armed_with == 'gun' |
                          wapo.data$armed_with == 'other;gun' | 
                          wapo.data$armed_with == 'gun;knife' |
                          wapo.data$armed_with == 'vehicle;gun' |
                          wapo.data$armed_with == 'gun;vehicle')
wapo.data.gun$armed <- "GUN"
wapo.data.blunt <- subset(wapo.data, wapo.data$armed_with == 'blunt_object' |
                            wapo.data$armed_with == 'blunt_object;blunt_object' |
                            wapo.data$armed_with == 'knife;blunt_object' |
                            wapo.data$armed_with == 'blunt_object;knife' |
                            wapo.data$armed_with == 'other;blunt_object;knife')
wapo.data.blunt$armed <- "BLUNT OBJECT"
wapo.data.replica <- subset(wapo.data, wapo.data$armed_with == 'replica' |
                              wapo.data$armed_with == 'replica;vehicle' |
                              wapo.data$armed_with == 'replica;knife')
wapo.data.replica$armed <- "REPLICA"
wapo.data.other <- subset(wapo.data, wapo.data$armed_with == 'unarmed' |
                            wapo.data$armed_with == 'other' |
                            wapo.data$armed_with == '' |
                            wapo.data$armed_with == 'vehicle' |
                            wapo.data$armed_with == 'undetermined' |
                            wapo.data$armed_with == 'unknown')
wapo.data.other$armed <- "OTHER"
wapo.data.knife <- subset(wapo.data, wapo.data$armed_with == 'knife' |
                            wapo.data$armed_with == 'knife;vehicle' |
                            wapo.data$armed_with == 'vehicle;knife;other')
wapo.data.knife$armed <- "KNIFE"

wapo.data.full <- rbind(wapo.data.gun, wapo.data.blunt, wapo.data.replica, wapo.data.other, wapo.data.knife)

wapo.armed <- wapo.data.full %>%
  group_by(armed) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

# Step 3
# single variable summary tables
wapo.race <- wapo.data %>%
  group_by(race) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))
  
wapo.year <- wapo.data %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

wapo.state <- wapo.data %>%
  group_by(state) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

wapo.armed <- wapo.data %>%
  group_by(armed_with) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

wapo.mental <- wapo.data %>%
  group_by(was_mental_illness_related) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

# multiple variable summary tables
wapo.race.armed <- wapo.data %>%
  group_by(race, armed_with) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

wapo.race.mental <- wapo.data %>%
  group_by(race, was_mental_illness_related) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

wapo.race.state <- wapo.data %>%
  group_by(race, state) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

# Step 5: Census Data
census_api_key("YOU API KEY HERE", install = TRUE) # run once!

### table 1: race by state
race.2021 <- get_acs(geography = "state", 
                     variables = c("B02008_001", "B02009_001", "B02010_001", "B02011_001", "B03001_003"), 
                     year = 2021)

race.2021$variable <- gsub("B02008_001", "White", race.2021$variable)
race.2021$variable <- gsub("B02009_001", "Black", race.2021$variable)
race.2021$variable <- gsub("B02010_001", "Native American", race.2021$variable)
race.2021$variable <- gsub("B02011_001", "Asian", race.2021$variable)
race.2021$variable <- gsub("B03001_003", "Hispanic", race.2021$variable)

### table 2: race for all US
race.total <- get_acs(geography = "us", 
                      variables = c("B02008_001", "B02009_001", "B02010_001", "B02011_001", "B03001_003"), 
                      year = 2021)

race.total$variable <- gsub("B02008_001", "White", race.total$variable)
race.total$variable <- gsub("B02009_001", "Black", race.total$variable)
race.total$variable <- gsub("B02010_001", "Native American", race.total$variable)
race.total$variable <- gsub("B02011_001", "Asian", race.total$variable)
race.total$variable <- gsub("B03001_003", "Hispanic", race.total$variable)

### table 3: population by state
totalpop.2021 <- get_acs(geography = "state", variables = "B01003_001", year = 2021)
names(totalpop.2021) <- c("GEOID", "State", "Variable", "Population", "junk")
totalpop.2021$State.PCT <- round(totalpop.2021$Population/sum(totalpop.2021$Population)*100,2)

temp.race.population <- race.2021 %>% left_join(totalpop.2021, by = "GEOID")
race.population <- temp.race.population[c(1:4,8,10)]
names(race.population) <- c("GEOID", "State", "Race", "Population.Race", "Population.State", "State.PCT")
race.population$Race.PCT <- round(race.population$Population.Race/race.population$Population.State*100, 2)
race.total$PCT <- race.total$estimate/sum(totalpop.2021$Population)*100
names(race.total) <- c("GEOID", "Area", "Race", "Count", "moe", "Race.PCT")

# clean the WAPO data to match the race names
wapo.race$race <- gsub("W", "White", wapo.race$race)
wapo.race$race <- gsub("B", "Black", wapo.race$race)
wapo.race$race <- gsub("H", "Hispanic", wapo.race$race)
wapo.race$race <- gsub("A", "Asian", wapo.race$race)
wapo.race$race <- gsub("N", "Native American", wapo.race$race)
wapo.race$race <- gsub("O", "Other", wapo.race$race)
names(wapo.race) <- c("Race", "Count", "Shooting.PCT")

# clean the WAPO data to match the state names
wapo.state$State.Name <- state.name[match(wapo.state$state,state.abb)]
wapo.state$State.Name <- replace_na(wapo.state$State.Name, "District of Columbia")
names(wapo.state) <- c("oldstate", "Count", "Shooting.PCT", "State")

# join the census and WAPO datas together
wapo.census.race <- wapo.race %>% left_join(race.total, by = "Race")
wapo.census.race <- wapo.census.race[c(1,3,8)]
wapo.census.state <- wapo.state %>% left_join(totalpop.2021, by = "State")
wapo.census.state <- wapo.census.state[c(4,3,9)]

##### Graphs
ggplot(wapo.data) + geom_bar(aes(x = race), stat = "count", fill = "grey")

ggplot(wapo.race.mental, aes(x = factor(race), y = pct, fill = factor(was_mental_illness_related))) + 
  geom_bar(stat="identity", width = 0.7) + 
  labs(x = "Race", y = "pct", fill = "Mental Illness") + 
  theme_minimal(base_size = 14)

ggplot(wapo.data) + 
  geom_bar(aes(x = was_mental_illness_related), stat = "count", fill = "grey") + 
  facet_wrap(~ race, nrow = 3)

##### Maps
world <- ne_countries(scale = "medium", returnclass = "sf") # this builds a list of countries
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # this cleans up the US states
names(wapo.state) <- c("state", "Count", "Shooting.PCT", "State.Full")
plot_usmap(data = wapo.state, values = "Count", color = "red") + 
  scale_fill_continuous(name = "Events per state", label = scales::comma) +   
  theme(legend.position = "right") # by counts

ggplot(data = world) + 
  geom_sf() + 
  geom_point(data = wapo.data, aes(x = longitude, y = latitude), size = 2, shape = 23, fill = "black") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data, aes(x = longitude, y = latitude), size = 2, shape = 1, fill = "darkred") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data, aes(x = longitude, y = latitude), size = 2, alpha = 0.15, shape = 1, fill = "darkred") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data, aes(x = longitude, y = latitude), size = 2, alpha = 0.20, shape = 1) + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE) +
  facet_wrap(~ year, nrow = 3)

wapo.data.clean <- subset(wapo.data, !is.na(wapo.data$latitude))
wapo.data.clean$latitude <- round(as.numeric(wapo.data.clean$latitude),4)
wapo.data.clean$longitude <- round(as.numeric(wapo.data.clean$longitude),4)
wapo.data.clean <- subset(wapo.data.clean, wapo.data.clean$longitude > -125)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  stat_density_2d(data = wapo.data.clean, aes(x = longitude, y = latitude, fill = stat(level)), bins = 10, geom = "polygon") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

