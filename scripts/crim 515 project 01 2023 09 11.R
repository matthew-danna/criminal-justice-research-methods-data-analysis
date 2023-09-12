##### Step 1
### public interest data
install.packages('gtrendsR')
library(gtrendsR)

trends <- gtrends("mass shooting", geo = "US", time = "all")
plot(trends)

trends1 <- gtrends("mass shooting", geo = "US", time = "2013-01-01 2023-09-11")
plot(trends1)

trends2 <- gtrends(c("mass shooting", "mass murder", "spree killing"), geo = "US", time = "all")
plot(trends2)

### mass shooting data
## Mother Jones
data.mj <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQBEbQoWMn_P81DuwmlQC0_jr2sJDzkkC0mvF6WLcM53ZYXi8RMfUlunvP1B5W0jRrJvH-wc-WGjDB1/pub?gid=0&single=true&output=csv",
                    stringsAsFactors = FALSE)

## Gun Violence Archive
# packages needed
install.packages('rvest')
install.packages('Hmisc')
install.packages('tidyverse')
library(Hmisc)
library(rvest)
library(tidyverse)

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
#install.packages('tidyverse')
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

# Add a source
data.mj$source <- "Mother Jones"

# Separate the city and the state
data.mj <- separate(data = data.mj, col = location, into = c("city", "state"), sep = ", ")

# Add a ‘Day of Week’ field
data.mj$day <- weekdays(data.mj$date)






