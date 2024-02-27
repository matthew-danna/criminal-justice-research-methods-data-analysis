# Step 0: packages
install.packages('gtrendsR')
install.packages('rvest')
install.packages('Hmisc')
install.packages('tidyverse')
install.packages('explore')
library(gtrendsR)
library(Hmisc)
library(rvest)
library(tidyverse)
library(explore)


# Step 1: public interest data
trends <- gtrends("mass shooting", geo = "US", time = "all")
plot(trends)

# Step 1: GVA event data
years <- as.character(2014:2024)
pages <- 0:35
url <- 'https://www.gunviolencearchive.org/reports/mass-shooting'
urls <- paste0(url, "?page=", pages)

urls.all <- NA
for (file in urls) {
  tmp.url <- paste0(file, "&year=", years)
  urls.all <- c(tmp.url, urls.all)
}
urls.all <- urls.all[!is.na(urls.all)]

get.gva <- function(site) {
  site %>%
    read_html()%>%
    html_nodes(xpath = '//*[@id="content"]/div/div/div') %>%
    html_table()
}

results <- sapply(urls.all, get.gva)

data.gva <- data.frame()
for (urls in results) {
  tmp.gva <- data.frame(urls)
  data.gva <- rbind(tmp.gva, data.gva)
}

# Step 1: MJ event data
data.mj <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQBEbQoWMn_P81DuwmlQC0_jr2sJDzkkC0mvF6WLcM53ZYXi8RMfUlunvP1B5W0jRrJvH-wc-WGjDB1/pub?gid=0&single=true&output=csv", stringsAsFactors = FALSE)

# Step 2: wrangling GVA
data.gva$date <- as.Date(data.gva$Incident.Date, format = "%B %d, %Y")
data.gva$source <- "Gun Violence Archive"
data.gva$total.victims <- data.gva$Victims.Killed + data.gva$Victims.Injured + data.gva$Suspects.Killed + data.gva$Suspects.Injured
data.gva$day <- weekdays(data.gva$date) 
data.gva <- data.gva[c(1,12,4,3,6:9,14,15,13)]
names(data.gva) <- c("event", "date", "city", "state", "victim.killed", "victim.injured", "suspect.killed", "suspect.injured", 
                     "total", "day", "source")
data.gva <- data.gva[!duplicated(data.gva), ]

# Step 2: wrangling MJ

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

data.mj$source <- "Mother Jones"
data.mj <- separate(data = data.mj, col = location, into = c("city", "state"), sep = ", ")
data.mj$day <- weekdays(data.mj$date)
# keep the original data and the qualitative content
data.mj.full <- data.mj
data.mj <- data.mj[c(1,4,2,3,6,7,8,31,30)]
names(data.mj) <- c("event", "date", "city", "state", "victim.killed", "victim.injured", "total", "day", "source")

# Step 3
explore(data.gva)
explore(data.mj)

events.state <- data.gva %>% 
  group_by(state) %>% 
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)

events.state <- data.mj %>% 
  group_by(state) %>% 
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)

data.gva$year <- substr(data.gva$date, 0, 4)
data.mj$year <- substr(data.mj$date, 0, 4)
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

victims.state <- aggregate(data.gva$total, 
                           by = list(Category = data.gva$state), FUN = sum)
victims.year <- aggregate(data.gva$total, 
                          by = list(Category= data.gva$year), FUN = sum)
victims.month <- aggregate(data.gva$total, 
                           by = list(Category = data.gva$month), FUN = sum)
victims.yearmonth <- aggregate(data.gva$total, 
                               by = list(Category= data.gva$yearmon), FUN = sum)

#### t test
# example: events per year with Columbine as the breakpoint
events.pre <- subset(data.mj, date < '1999-04-20') # excludes the attack
events.post <- subset(data.mj, date > '1999-04-20')

year.pre <- events.pre %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  mutate(PCT = count/sum(count)*100)
year.post <- events.post %>% 
  group_by(year) %>%  
  summarise(count = n()) %>% 
  mutate(PCT = count/sum(count)*100)

t.test(year.pre$count, year.post$count)

# example: total victims per month with Jan 6th as the breakpoint
victims.pre <- subset(data.gva, date < '2021-01-06') # excludes the attack
victims.post <- subset(data.gva, date > '2021-01-06')

month.pre <- aggregate(victims.pre$total, 
                               by = list(Category= victims.pre$yearmon), 
                       FUN = sum)
month.post <- aggregate(victims.post$total, 
                       by = list(Category= victims.post$yearmon), 
                       FUN = sum)

t.test(month.pre$x, month.post$x)
