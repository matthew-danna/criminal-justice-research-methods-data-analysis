# LIBRARIES
#install.packages('tidyverse')
library(tidyverse)
#install.packages('data.table')
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
hate.subset <- subset(hate.crime, hate.crime$data_year >= '1996' &
                        hate.crime$data_year <= '2005')

# SUMMARIES
hate.sum.year <- hate.subset %>% 
  group_by(data_year) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

hate.sum.offense <- hate.subset %>% 
  group_by(offense_name) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

hate.sum.year.bias <- hate.subset %>% 
  group_by(data_year, bias_desc) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

hate.sum.year.region <- hate.subset %>% 
  group_by(data_year, region_name) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

# BASIC STATS
mean(hate.sum.year$count)

hate.sum.year$compare <- hate.sum.year$count - 
  mean(hate.sum.year$count)

# T TESTS
### pick a break-point
hate.pre <- subset(hate.subset, hate.subset$yearmonth < '2001-09')
hate.post <- subset(hate.subset, hate.subset$yearmonth >= '2001-09')

### group our data
hate.pre.sum <- hate.pre %>%
  group_by(yearmonth) %>%
  summarise(count = n())

hate.post.sum <- hate.post %>%
  group_by(yearmonth) %>%
  summarise(count = n())

### run t test
t.test(hate.pre.sum$count, hate.post.sum$count)

sum.hate.yearmonth.region <- hate.subset %>% 
  group_by(yearmonth, region_name) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

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

# new example
hate.obama <- subset(hate.crime, hate.crime$data_year >= '2013' &
                       hate.crime$data_year <= '2016')
hate.trump <- subset(hate.crime, hate.crime$data_year >= '2017' &
                       hate.crime$data_year <= '2020')

sum.hate.obama <- hate.obama %>%
  group_by(yearmonth) %>%
  summarise(count = n())
sum.hate.trump <- hate.trump %>%
  group_by(yearmonth) %>%
  summarise(count = n())

t.test(sum.hate.obama$count, sum.hate.trump$count)

# t test graph
sum.hate.obama$CATEGORY <- "Obama"
sum.hate.trump$CATEGORY <- "Trump"

sum.presidents <- rbind(sum.hate.obama, sum.hate.trump)

xbreaks <- c("2013-06", "2013-12",
             "2014-06", "2014-12",
             "2015-06", "2015-12",
             "2016-06", "2016-12",
             "2017-06", "2017-12",
             "2018-06", "2018-12",
             "2019-06", "2019-12",
             "2020-06", "2020-12")

ggplot(sum.presidents) +
  geom_bar(aes(yearmonth, count, fill = CATEGORY), stat = "identity") +
  labs(title = "My Bar Graph",
       subtitle = "Something smart about my graph",
       x = "Time Period of some sort",
       y = "The Things i'm counting",
       fill = "President") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_blank()) +
  scale_fill_manual(values = c("darkblue", "red")) +
  scale_x_discrete(breaks = xbreaks)

