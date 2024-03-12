# Step 0: packages
install.packages('gtrendsR')
install.packages('rvest')
install.packages('Hmisc')
install.packages('tidyverse')
install.packages('usmap')
library(usmap)
library(gtrendsR)
library(Hmisc)
library(rvest)
library(tidyverse)

# Step 1: public interest data
#### MANUALLY USE EITHER THE GOOGLE TRENDS OR STANFORD CABLE NEWS WEBSITES
#### 

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
gva.events.state <- data.gva %>% 
  group_by(state) %>% 
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)

mj.events.state <- data.mj %>% 
  group_by(state) %>% 
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)

data.gva$year <- substr(data.gva$date, 0, 4)
data.mj$year <- substr(data.mj$date, 0, 4)

gva.events.year <- data.gva %>% 
  group_by(year) %>% 
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)

mj.events.year <- data.mj %>% 
  group_by(year) %>% 
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)

data.gva$month <- substr(data.gva$date, 6, 7)

gva.events.month <- data.gva %>% 
  group_by(month) %>% 
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)

data.gva$yearmonth <- paste(data.gva$year, data.gva$month, sep = "-")

gva.events.yearmonth <- data.gva %>% 
  group_by(yearmonth) %>% 
  summarise(count = n()) %>%
  mutate(PCT = count/sum(count)*100)

gva.victims.state <- aggregate(data.gva$total, 
                           by = list(Category = data.gva$state), FUN = sum)
mj.victims.state <- aggregate(data.mj$total, 
                               by = list(Category = data.mj$state), FUN = sum)
gva.victims.year <- aggregate(data.gva$total, 
                          by = list(Category= data.gva$year), FUN = sum)
mj.victims.year <- aggregate(data.mj$total, 
                              by = list(Category= data.mj$year), FUN = sum)
gva.victims.month <- aggregate(data.gva$total, 
                           by = list(Category = data.gva$month), FUN = sum)
gva.victims.yearmonth <- aggregate(data.gva$total, 
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

# Step 4
## bar graph
ggplot(gva.events.year, aes(x = count, y = year)) + 
  geom_bar(stat = "identity") # basic

ggplot(gva.events.year, aes(x = count, y = year)) + 
  geom_bar(stat = "identity", color = 'purple', fill = 'white') # with color

ggplot(gva.events.year, aes(x = count, y = year)) + 
  geom_bar(stat = "identity", color = 'purple', fill = 'white') + 
  coord_flip() + 
  labs(title = "Add your title here", subtitle = "add a subtitle here") + 
  theme_classic() # with labels

ggplot(gva.events.yearmonth, aes(x = count, y = yearmonth)) + 
  geom_bar(stat = "identity", color = 'purple', fill = 'white') + 
  coord_flip() + 
  labs(title = "Add your title here", subtitle = "add a subtitle here") + 
  theme_classic() # with labels

ggplot(gva.victims.yearmonth, aes(x = Category, y = x)) + 
  geom_bar(stat = "identity", color = 'purple', fill = 'white') + 
  coord_flip() + 
  labs(title = "Your title here", subtitle = "A subtitle here") + 
  theme_bw()

## scatter plots
ggplot(mj.events.year, aes(count, year)) +  
  geom_point() + 
  coord_flip() + 
  labs(title = "Add your title here", subtitle = "add a subtitle here") + 
  theme_dark()

## boxplots
boxplot(gva.events.yearmonth$count) # for number of events per year-month
boxplot(mj.victims.year$x) # for number of victims per year

boxplot(month.pre$x)
boxplot(month.post$x)

ggplot(data.gva, aes(x = year, y = total)) + 
  geom_boxplot(fill = "grey92") # events by year

## maps
gva.events.state$state <- state.abb[match(gva.events.state$state, state.name)]

plot_usmap(data = gva.events.state, values = "count", color = "grey") + 
  scale_fill_continuous(name = "Legend title here", label = scales::comma) +  
  theme(legend.position = "right")

mj.events.pre <- events.pre %>%
  group_by(state) %>%
  summarise(count = n())
mj.events.pre$state <- state.abb[match(mj.events.pre$state, state.name)]
plot_usmap(data = mj.events.pre, values = "count", color = "grey") + 
  scale_fill_continuous(name = "Pre-Columbine Event Counts", label = scales::comma) +  
  theme(legend.position = "right")

mj.events.post <- events.post %>%
  group_by(state) %>%
  summarise(count = n())
mj.events.post$state <- state.abb[match(mj.events.post$state, state.name)]
plot_usmap(data = mj.events.post, values = "count", color = "grey") + 
  scale_fill_continuous(name = "Post-Columbine Event Counts", label = scales::comma) +  
  theme(legend.position = "right")

## raincloud plots
install.packages('devtools')
devtools::install_github('jorvlan/raincloudplots')
library(raincloudplots)
install.packages('plyr')

data.1x1 <- data_1x1(
  array_1 = data.mj$total[124:149], # use row ID numbers
  array_2 = data.mj$total[1:122], # update these relative to your break point
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
events.year.1x1 <- data_1x1(
  array_1 = mj.events.year$count[1:16], # based on row ID numbers
  array_2 = mj.events.year$count[17:39],
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






