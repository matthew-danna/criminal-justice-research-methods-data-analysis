##### Step 0
#install.packages('gtrendsR')
#install.packages('rvest')
#install.packages('Hmisc')
#install.packages('tidyverse')
library(gtrendsR)
library(Hmisc)
library(rvest)
library(tidyverse)

##### Step 1
### public interest data
trends <- gtrends("mass shooting", geo = "US", time = "all")
plot(trends)

trends1 <- gtrends("mass shooting", geo = "US", time = "2013-01-01 2023-09-18")
plot(trends1)

trends2 <- gtrends(c("mass shooting", "mass murder", "spree killing"), geo = "US", time = "all")
plot(trends2)

### mass shooting data
## Mother Jones
data.mj <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQBEbQoWMn_P81DuwmlQC0_jr2sJDzkkC0mvF6WLcM53ZYXi8RMfUlunvP1B5W0jRrJvH-wc-WGjDB1/pub?gid=0&single=true&output=csv",
                    stringsAsFactors = FALSE)

## Gun Violence Archive
# create objects
years <- as.character(2014:2023)
pages <- 0:30 # works if there's less than 775 shootings in a year (31 pages*25 events)
url <- 'https://www.gunviolencearchive.org/reports/mass-shooting'
urls <- paste0(url, "?page=", pages)

# build URLs
urls.all <- NA
for (file in urls) {
  tmp.url <- paste0(file, "&year=", years)
  urls.all <- c(tmp.url, urls.all)
}
urls.all <- urls.all[!is.na(urls.all)]

# create a function for scraping a table from each URL
get.gva <- function(site) {
  site %>%
    read_html()%>%
    html_nodes(xpath = '//*[@id="content"]/div/div/div') %>%
    html_table()
}

# go get the data
results <- sapply(urls.all, get.gva)

# transform the data into a table
data.gva <- data.frame()
for (urls in results) {
  tmp.gva <- data.frame(urls)
  data.gva <- rbind(tmp.gva, data.gva)
}

##### Step 2
### Mother Jones

# parse and clean the month
data.mj$temp.month <- substr(data.mj$date, 0, 2)
data.mj$temp.month <- gsub("/", "", data.mj$temp.month)
# parse and clean the day
data.mj$temp.day <- substr(data.mj$date, (nchar(data.mj$temp.month)+2), (nchar(data.mj$temp.month)+3))
data.mj$temp.day <- gsub("/", "", data.mj$temp.day)
# parse and clean the year
data.mj$temp.year <- substr(data.mj$date, nchar(data.mj$date)-1, nchar(data.mj$date))
# rebuild the date
data.mj$temp.date <- paste(data.mj$temp.month, data.mj$temp.day, data.mj$temp.year, sep = "/")
# format the date
data.mj$date <- as.Date(data.mj$temp.date, format = "%m/%d/%y")

# Add a source
data.mj$source <- "Mother Jones"

# Separate the city and the state
data.mj <- separate(data = data.mj, col = location, into = c("city", "state"), sep = ", ")

# Add a ‘Day of Week’ field
data.mj$day <- weekdays(data.mj$date)

# Only keep the relevant fields
data.mj <- data.mj[c(1,4,2,3,6,7,8,31,30)]

# Rename the fields
names(data.mj) <- c("event", "date", "city", "state", "killed", "injured", "total", "day", "source")

### Gun Violence Archive
# Format the dates correctly
data.gva$date <- as.Date(data.gva$Incident.Date, format = "%B %d, %Y")
# Add a source
data.gva$source <- "Gun Violence Archive"
# Calculate total victims
data.gva$total.victims <- data.gva$Victims.Killed + data.gva$Victims.Injured
# Add a ‘Day of Week’ field
data.gva$day <- weekdays(data.gva$date) 
# Only keep the relevant fields
data.gva <- data.gva[c(1,12,4,3,6,7,14,15,13)]
# Rename the fields
names(data.gva) <- c("event", "date", "city", "state", "killed", "injured", "total", "day", "source")
# Deduplicate the repeat rows
data.gva <- data.gva[!duplicated(data.gva), ]

##### Step 3
install.packages('explore')
library(explore)
explore(data.mj)

# Summary tables by state, year, and yearmonth
events.state <- data.mj %>%
  group_by(state) %>%
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)

events.state <- data.gva %>%
  group_by(state) %>%
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)

data.mj$year <- substr(data.mj$date, 0, 4)
events.year <- data.mj %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)
           
data.gva$year <- substr(data.gva$date, 0, 4)
events.year <- data.gva %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)
           
data.gva$month <- substr(data.gva$date, 6, 7)
events.month <- data.gva %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)
           
data.gva$yearmonth <- paste(data.gva$year, data.gva$month, sep = "-")
events.yearmonth <- data.gva %>%
  group_by(yearmonth) %>%
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)
           
### casualty counts
# victims by state
victims.state <- aggregate(data.mj$total, by = list(Category = data.mj$state), FUN = sum)
victims.state <- aggregate(data.gva$total, by = list(Category = data.gva$state), FUN = sum)
victims.state <- aggregate(data.mj$killed, by = list(Category = data.mj$state), FUN = sum)
victims.state <- aggregate(data.gva$killed, by = list(Category = data.gva$state), FUN = sum)

# victims by year
victims.year <- aggregate(data.gva$total, by = list(Category= data.gva$year), FUN = sum)

# victims by year-month
victims.yearmonth <- aggregate(data.gva$total, by = list(Category = data.gva$yearmonth), FUN = sum)

### t tests!
# Mother Jones events using Columbine as the break-point
# first, subset the data
events.pre <- subset(data.mj, data.mj$date < '1999-04-20')
events.post <- subset(data.mj, data.mj$date > '1999-04-20')

# second, group the data
year.pre <- events.pre %>%
  group_by(year) %>%
  summarise(count = n())
           
year.post <- events.post %>%
  group_by(year) %>%
  summarise(count = n())

t.test(year.pre$count, year.post$count)

# GVA 


