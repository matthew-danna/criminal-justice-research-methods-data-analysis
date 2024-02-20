# Step 1: public interest data
install.packages('gtrendsR')
library(gtrendsR)

trends <- gtrends("mass shooting", geo = "US", time = "all")
plot(trends)

# Step 1: GVA event data
install.packages('rvest')
install.packages('Hmisc')
install.packages('tidyverse')
library(Hmisc)
library(rvest)
library(tidyverse)

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
library(tidyverse)
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
data.mj <- data.mj[c(1,4,2,3,6,7,8,31,30)]
names(data.mj) <- c("event", "date", "city", "state", "victim.killed", "victim.injured", "total", "day", "source")


