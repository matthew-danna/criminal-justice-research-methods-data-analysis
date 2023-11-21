##### 0: Libraries
library(tidyverse)

##### 1: Read the file(s) into RStudio
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", "1gRoL7lZlwm7sreA5F9WbPoH5su4n4iGS"))

##### 2: Select/subset any specific categories
unique(calls$type)

calls.summary <- calls %>%
  group_by(type) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT),2))

# for a single call type:
calls.subset1 <- subset(calls, calls$type == 'MISC')

# for two call types:
calls.subset2 <- subset(calls, calls$type == 'MISC' |
                         calls$type == 'SERVICE')

# for more than two calls:
calls.group <- c("MISC", "SERVICE", "UNKNOWN", "Special Detail")

calls.subset3 <- calls %>%
  filter(calls$type %in% calls.group)

##### 3: Summarize your events by a time measure
calls.subset3$date <- as.Date(calls.subset3$date)
calls.subset3$week <- week(ymd(calls.subset3$date))
calls.subset3$month <- substr(calls.subset3$date, 6,7)
calls.subset3$yearmonth <- paste(calls.subset3$year, calls.subset3$month, sep = "-")

# for seasons:
calls.summer <- subset(calls.subset3, calls.subset3$month == '06' |
                         calls.subset3$month == '07' |
                         calls.subset3$month == '08')
calls.winter <- subset(calls.subset3, calls.subset3$month == '11' |
                       calls.subset3$month == '12' |
                         calls.subset3$month == '01')
calls.spring <- subset(calls.subset3, calls.subset3$month == '02' |
                         calls.subset3$month == '03' |
                         calls.subset3$month == '04' |
                         calls.subset3$month == '05')
calls.fall <- subset(calls.subset3, calls.subset3$month == '09' |
                         calls.subset3$month == '10')
calls.summer$season <- "SUMMER"
calls.winter$season <- "WINTER"
calls.spring$season <- "SPRING"
calls.fall$season <- "FALL"
calls.seasons <- rbind(calls.summer, calls.winter, calls.spring, calls.fall)

##### 4: Identify trends over time
summary.calls <- calls.seasons %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count),2))

summary.season <- calls.seasons %>%
  group_by(season) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count),2))

summary.year.season <- calls.seasons %>%
  group_by(year, season) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count),2))

summary.type.season <- calls.seasons %>%
  group_by(type, season) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count),2))

summary.month <- calls.seasons %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count),2))

summary.year.month <- calls.seasons %>%
  group_by(yearmonth) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count),2))

##### 5: Determine a break-point

##### 6: Calculate a t test to identify statistically significant changes

##### 7: Visualize the findings
