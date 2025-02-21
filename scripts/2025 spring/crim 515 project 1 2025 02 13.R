# LIBRARIES
#install.packages('tidyverse')
library(tidyverse)
install.packages('data.table')
library(data.table)

# GET DATA
## https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads
## Additional Datasets
hate.crime <- read.csv("C:/Users/mdanna2/Downloads/hate_crime.csv",
                       stringsAsFactors = FALSE)

# CLEANING DATA
hate.crime$incident_date <- as.Date(hate.crime$incident_date)
hate.crime$month <- substr(hate.crime$incident_date,6,7)
hate.crime$months <- months(hate.crime$incident_date)
hate.crime$yearmonth <- substr(hate.crime$incident_date,0,7)

# FILTERING
hate.subset <- subset(hate.crime, hate.crime$data_year == '2023')
hate.subset <- subset(hate.crime, hate.crime$data_year != '2023')
hate.subset <- subset(hate.crime, hate.crime$data_year > '2019')
hate.subset <- subset(hate.crime, hate.crime$data_year <= '1999')
hate.subset <- subset(hate.crime, hate.crime$data_year >= '2000' &
                        hate.crime$data_year <= '2020')
hate.subset <- subset(hate.crime, hate.crime$data_year == '2010' |
                        hate.crime$data_year == '2020')

# SUMMARIES
hate.sum.year <- hate.crime %>% 
  group_by(data_year) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

hate.sum.offense <- hate.crime %>% 
  group_by(offense_name) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

hate.sum.year.bias <- hate.crime %>% 
  group_by(data_year, bias_desc) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

# BASIC STATS
mean(hate.sum.year$count)

# T TESTS
### pick a break-point
hate.pre <- subset(hate.crime, hate.crime$yearmonth < '2020-03')
hate.post <- subset(hate.crime, hate.crime$yearmonth >= '2020-03')

### group our data
hate.pre.sum <- hate.pre %>%
  group_by(yearmonth) %>%
  summarise(count = n())

hate.post.sum <- hate.post %>%
  group_by(yearmonth) %>%
  summarise(count = n())

### run t test
t.test(hate.pre.sum$count, hate.post.sum$count)

##### EXAMPLE
hate.bias.asian <- hate.crime %>%
  filter(str_detect(bias_desc, "Anti-Asian"))

hate.bias.asian.recent <- subset(hate.bias.asian, 
                                 hate.bias.asian$data_year > 2015)

hate.bias.asian.pre <- subset(hate.bias.asian.recent,
                              hate.bias.asian.recent$yearmonth < '2020-03')
hate.bias.asian.post <- subset(hate.bias.asian.recent,
                               hate.bias.asian.recent$yearmonth >= '2020-03')

asian.pre <- hate.bias.asian.pre %>%
  group_by(yearmonth) %>%
  summarise(count = n())
asian.post <- hate.bias.asian.post %>%
  group_by(yearmonth) %>%
  summarise(count = n())

t.test(asian.pre$count, asian.post$count)

# CHI-SQUARE