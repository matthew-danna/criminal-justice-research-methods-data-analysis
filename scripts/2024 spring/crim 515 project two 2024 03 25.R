# Step 0: Libraries
install.packages(c("cowplot", "ggrepel", "rgeos", "sf", "maps", "usmap", "ggspatial", 
                   "libwgeom", "rnaturalearth", "rnaturalearthdata"))
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(lubridate)
library(usmap)
library(sf)
theme_set(theme_bw())

# Step 1: get the Washington Post event data
wapo.data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv", 
                      stringsAsFactors = FALSE)

# Step 2: clean up the data
# format the date
wapo.data$date <- as.Date(wapo.data$date)
# calculate the month
wapo.data$month <- substr(wapo.data$date, 6, 7)
# calculate the year
wapo.data$year <- substr(wapo.data$date,0,4)
# calculate the year-month
wapo.data$yearmonth <- paste(wapo.data$year, wapo.data$month, sep = "-")
# calculate the multi-race victims as 'other'
unique(wapo.data$race)
wapo.data$race <- gsub("W;B;N", "O", wapo.data$race)
wapo.data$race <- gsub("N;H", "O", wapo.data$race)
wapo.data$race <- gsub("W;H", "O", wapo.data$race)
wapo.data$race <- gsub("B;H", "O", wapo.data$race)
wapo.data$race <- gsub("W;B", "O", wapo.data$race)
unique(wapo.data$race)

# Step 3: Summary tables
# by race
wapo.race <- wapo.data %>% 
  group_by(race) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))

# calculate the average and standard deviation for
# number of victims per race
mean(wapo.race$count)
sd(wapo.race$count)

# by year
wapo.year <- wapo.data %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))
wapo.year.no2024 <- subset(wapo.year, wapo.year$year < 2024)
mean(wapo.year.no2024$count)

# by state
wapo.state <- wapo.data %>% 
  group_by(state) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))
mean(wapo.state$count)

# race and mental illness
wapo.race.mental <- wapo.data %>% 
  group_by(race, was_mental_illness_related) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)*100)

# Step 4: Bar graphs
ggplot(wapo.data) + 
  geom_bar(aes(x = race), stat = "count", fill = "grey")

ggplot(wapo.race.mental, aes(x = factor(race), 
                             y = pct, 
                             fill = factor(was_mental_illness_related))) + 
  geom_bar(stat="identity", width = 0.7) + 
  labs(x = "Race", y = "percent", fill = "Mental Illness") + 
  theme_minimal(base_size = 14)

ggplot(wapo.data) + 
  geom_bar(aes(x = was_mental_illness_related), stat = "count", 
           fill = "grey") + 
  facet_wrap(~ race, nrow = 3)

# Step 4: Maps!
wapo.data.clean <- subset(wapo.data, !is.na(wapo.data$latitude))
wapo.data.clean$latitude <- round(as.numeric(wapo.data.clean$latitude),4)
wapo.data.clean$longitude <- round(as.numeric(wapo.data.clean$longitude),4)
wapo.data.clean <- subset(wapo.data.clean, wapo.data.clean$longitude > -160)

world <- ne_countries(scale = "medium", returnclass = "sf") # this builds a list of countries
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # this cleans up the US states

names(wapo.state) <- c("state", "Count", "Shooting.PCT", "State.Full")
plot_usmap(data = wapo.state, values = "Count", color = "red") + 
  scale_fill_continuous(name = "Events per state", 
                        label = scales::comma) +   
  theme(legend.position = "right") # by counts

ggplot(data = world) + 
  geom_sf() + 
  geom_point(data = wapo.data.clean, aes(x = longitude, y = latitude), 
             size = 2, shape = 23, fill = "black") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data.clean, aes(x = longitude, y = latitude), 
             size = 2, shape = 1, fill = "darkred") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  stat_density_2d(data = wapo.data.clean, 
                  aes(x = longitude, y = latitude, 
                      fill = stat(level)), bins = 4, 
                  geom = "polygon") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + geom_sf() + geom_sf(data = states, fill = NA) + geom_density_2d(data = wapo.data.clean, aes(x = longitude, y = latitude), bins = 5) + coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + geom_sf() + geom_sf(data = states, fill = NA) + geom_point(data = wapo.data.clean, aes(x = longitude, y = latitude), size = 2, shape = 1, fill = "darkred") + coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE) + facet_wrap(~ year, nrow = 4)

