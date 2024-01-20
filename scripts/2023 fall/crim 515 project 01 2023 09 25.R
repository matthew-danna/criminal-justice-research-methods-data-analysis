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

trends1 <- gtrends("mass shooting", geo = "US", time = "2013-01-01 2023-09-25")
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
data.gva$year <- substr(data.gva$date, 0, 4)
victims.year <- aggregate(data.gva$total, by = list(Category= data.gva$year), FUN = sum)

# victims by year-month
data.gva$month <- substr(data.gva$date, 6, 7)
data.gva$yearmonth <- paste(data.gva$year, data.gva$month, sep = "-")
victims.yearmonth <- aggregate(data.gva$total, by = list(Category = data.gva$yearmonth), FUN = sum)

### t tests!
# make smart filters
ban.before <- subset(data.mj, data.mj$date < '1994-09-12')
ban.during <- subset(data.mj, data.mj$date >= '1994-09-12' & data.mj$date < '2004-09-14')
ban.after <- subset(data.mj, data.mj$date >= '2004-09-14' & data.mj$date < '2014-09-15')

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

# GVA events using 1/1/2019 as the break-point
events.pre <- subset(data.gva, data.gva$date < '2019-01-01')
events.post <- subset(data.gva, data.gva$date >= '2019-01-01')

yearmonth.pre <- events.pre %>%
  group_by(yearmonth) %>%
  summarise(count = n())
yearmonth.post <- events.post %>%
  group_by(yearmonth) %>%
  summarise(count = n())

t.test(yearmonth.pre$count, yearmonth.post$count)

# MJ total casualty counts using Columbine
events.pre <- subset(data.mj, data.mj$date < '1999-04-20')
events.post <- subset(data.mj, data.mj$date > '1999-04-20')

year.pre <- aggregate(events.pre$total, by = list(Category = events.pre$year), FUN = sum)
year.post <- aggregate(events.post$total, by = list(Category = events.post$year), FUN = sum)

t.test(year.pre$x, year.post$x)

year.pre <- aggregate(events.pre$killed, by = list(Category = events.pre$year), FUN = sum)
year.post <- aggregate(events.post$killed, by = list(Category = events.post$year), FUN = sum)

t.test(year.pre$x, year.post$x)

### Step 4: Visuals
# bar graphs
ggplot(events.year, aes(x = count, y = year)) +
  geom_bar(stat = "identity")

ggplot(events.year, aes(x = count, y = year)) +
  geom_bar(stat = "identity" ,color = 'purple', fill = 'white')

ggplot(events.year, aes(x = count, y = year)) + 
  geom_bar(stat = "identity", color = 'purple', fill = 'white') + 
  coord_flip() + 
  labs(title = "Add your title here", subtitle = "add a subtitle here") + 
  theme_classic() # with labels

ggplot(events.year, aes(count, year)) +  
  geom_point() + 
  coord_flip() + 
  labs(title = "Add your title here", subtitle = "add a subtitle here") + 
  theme_dark()

ggplot(data.gva, aes(x = state, y = total)) + 
  geom_boxplot(fill = "grey92") # events by state

ggplot(victims.yearmonth, aes(x = x, y = Category)) + 
  geom_bar(stat = "identity", color = 'purple', fill = 'white') + 
  coord_flip() + 
  labs(title = "Add your title here", subtitle = "add a subtitle here") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, size = 5)) 

# maps
install.packages('usmap')
library(usmap)

events.state$state <- state.abb[match(events.state$state, state.name)]
plot_usmap(data = events.state, values = "count", color = "grey") + 
  scale_fill_continuous(name = "Legend title here", label = scales::comma) +  
  theme(legend.position = "right")

# rainclouds
devtools::install_github('jorvlan/raincloudplots')
library(raincloudplots)
install.packages('plyr')

data.1x1 <- data_1x1(
  array_1 = data.mj$total[122:147], # use row ID numbers
  array_2 = data.mj$total[1:121], # update these relative to your break point
)
# for total victims over time
events.raincloud <- raincloud_1x1(
  data = data.1x1, 
  colors = (c('dodgerblue','darkorange')), 
  fills = (c('dodgerblue','darkorange')), 
  size = 1, 
  alpha = .6, 
  ort = 'h') +
  scale_x_continuous(breaks=c(1,2), labels=c("Early", "Late"), limits=c(0, 3)) +
  xlab("Groups") + 
  ylab("Total Victims") +
  theme_classic()
events.raincloud


# for events per year
# works best for events counts
events.year.1x1 <- data_1x1(
  array_1 = events.year$count[1:16],
  array_2 = events.year$count[17:39],
)
year.raincloud <- raincloud_1x1(
  data = events.year.1x1, 
  colors = (c('dodgerblue','darkorange')), 
  fills = (c('dodgerblue','darkorange')), 
  size = 1, 
  alpha = .6, 
  ort = 'v') + # change v to h to flip graph orientation
  scale_x_continuous(breaks=c(1,2), labels=c("Early", "Late"), limits=c(0, 3)) +
  xlab("Groups") + 
  ylab("Total Events") +
  theme_classic()
year.raincloud

# comparing means model
# this works best for victim count analyses
library(broom)
events.pre$group <- 0
events.post$group <- 1
events <- rbind(events.pre, events.post) # rebuilds the ‘data’ table with these new fields, based on the divide you did earlier
model <- lm(killed ~ factor(group), data = events)
result <- tidy(model) # builds a linear model for the casualties per event
install.packages('dabestr')
library(dabestr)
bootstrap <- dabest(events,
                    group,
                    killed,
                    idx = c("0", "1"),
                    paired = FALSE)
bootstrap_diff <- mean_diff(bootstrap)
plot(bootstrap_diff)

# Google Trends
plot(trends1)

trends.data <- data.frame(trends1$interest_over_time)
ggplot(trends.data, aes(x = date, y = hits)) + 
  geom_point() + 
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +  
  theme_bw() + 
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')

install.packages('shiny')
install.packages('plotly')
library(plotly)
p <- plot(trends1)
ggplotly(p)

install.packages('lubridate')
library(lubridate)
TrendsInterest <- trends1$interest_over_time
trends.new <- TrendsInterest %>% filter(year(date)>2003) %>% mutate(date = ymd(date), hits = as.numeric(hits))
pink <- "#FF8DC6"
blue <- "#56C1FF"
red = "#ff7f7f"
ggplot() + 
  geom_line(data=trends.new, aes(x=date, y=hits, group=keyword, color = keyword)) + 
  scale_color_manual(values=c(red, blue, pink)) + 
  theme_classic() + 
  theme(legend.position="bottom") + 
  labs(title = "YOUR TITLE HERE", subtitle = "Your subtitle here", caption = "Source: whatever you want here",  
       x = "Date", y = "Hits") 

install.packages('gifski')
library(gifski)
install.packages('gganimate')
library(gganimate)
install.packages('ggimage')
library(ggimage)
install.packages('usethis')
library(usethis)
install.packages('png')
library(png)
t <- p + transition_reveal(as.numeric(date)) 
gif <- animate(t, end_pause = 25, width = 800, height = 400, fps = 8)
gif

