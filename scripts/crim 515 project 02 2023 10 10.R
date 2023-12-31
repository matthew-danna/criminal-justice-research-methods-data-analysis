# Libraries
library(gtrendsR)
library(tidyverse)
install.packages('tidycensus')
library(tidycensus)

# Step 1: Data
## Police Shootings
wapo.data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv", 
                      stringsAsFactors = FALSE)

## Google Trends
today <- as.character(Sys.Date())
time.range <- paste("2015-01-01", today, sep = " ")
trends1 <- gtrends("police shooting", geo = "US", time = time.range)
plot(trends1)

# Step 2
wapo.data$date <- as.Date(wapo.data$date)
wapo.data$month <- substr(wapo.data$date, 6, 7)
wapo.data$year <- substr(wapo.data$date,0,4)
wapo.data$yearmonth <- paste(wapo.data$year, wapo.data$month, sep = "-")

unique(wapo.data$race)
wapo.data$race <- gsub("B;H", "O", wapo.data$race)

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

census.variables.2021 <- load_variables(2021, "acs5", cache = TRUE)

race.2021 <- get_acs(geography = "state", 
                     variables = c("B02008_001", "B02009_001", "B02010_001", "B02011_001", "B03001_003"), 
                     year = 2021)

race.2021$variable <- gsub("B02008_001", "White", race.2021$variable)
race.2021$variable <- gsub("B02009_001", "Black", race.2021$variable)
race.2021$variable <- gsub("B02010_001", "Native American", race.2021$variable)
race.2021$variable <- gsub("B02011_001", "Asian", race.2021$variable)
race.2021$variable <- gsub("B03001_003", "Hispanic", race.2021$variable)

race.total <- get_acs(geography = "us", 
                      variables = c("B02008_001", "B02009_001", "B02010_001", "B02011_001", "B03001_003"), 
                      year = 2021)

race.total$variable <- gsub("B02008_001", "White", race.total$variable)
race.total$variable <- gsub("B02009_001", "Black", race.total$variable)
race.total$variable <- gsub("B02010_001", "Native American", race.total$variable)
race.total$variable <- gsub("B02011_001", "Asian", race.total$variable)
race.total$variable <- gsub("B03001_003", "Hispanic", race.total$variable)

totalpop.2021 <- get_acs(geography = "state", variables = "B01003_001", year = 2021)
names(totalpop.2021) <- c("GEOID", "State", "Variable", "Population", "junk")
totalpop.2021$State.PCT <- round(totalpop.2021$Population/sum(totalpop.2021$Population)*100,2)

temp.race.population <- race.2021 %>% left_join(totalpop.2021, by = "GEOID")
race.population <- temp.race.population[c(1:4,8,10)]
names(race.population) <- c("GEOID", "State", "Race", "Population.Race", "Population.State", "State.PCT")
race.population$Race.PCT <- round(race.population$Population.Race/race.population$Population.State*100, 2)
race.total$PCT <- race.total$estimate/sum(totalpop.2021$Population)*100
names(race.total) <- c("GEOID", "Area", "Race", "Count", "moe", "Race.PCT")

wapo.race$race <- gsub("W", "White", wapo.race$race)
wapo.race$race <- gsub("B", "Black", wapo.race$race)
wapo.race$race <- gsub("H", "Hispanic", wapo.race$race)
wapo.race$race <- gsub("A", "Asian", wapo.race$race)
wapo.race$race <- gsub("N", "Native American", wapo.race$race)
wapo.race$race <- gsub("O", "Other", wapo.race$race)
names(wapo.race) <- c("Race", "Count", "Shooting.PCT")

wapo.state$State.Name <- state.name[match(wapo.state$state,state.abb)]
wapo.state$State.Name <- replace_na(wapo.state$State.Name, "District of Columbia")
names(wapo.state) <- c("oldstate", "Count", "Shooting.PCT", "State")

wapo.census.race <- wapo.race %>% left_join(race.total, by = "Race")
wapo.census.state <- wapo.state %>% left_join(totalpop.2021, by = "State")

