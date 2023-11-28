##### Step 0: Libraries
library(tidyverse)

##### Step 1: Get data
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", "1gRoL7lZlwm7sreA5F9WbPoH5su4n4iGS"))

##### Step 2: Add date formats (week, season, quarter)
calls$date <- as.Date(calls$date)
calls$week <- week(ymd(calls$date))
calls$month <- substr(calls$date, 6,7)
calls$yearmonth <- paste(calls$year, calls$month, sep = "-")

# Quarters, based on City of Fairfax
calls.q1 <- subset(calls, calls$month == '07' |
                         calls$month == '08' |
                         calls$month == '09')
calls.q2 <- subset(calls, calls$month == '10' |
                         calls$month == '11' |
                         calls$month == '12')
calls.q3 <- subset(calls, calls$month == '01' |
                         calls$month == '02' |
                         calls$month == '03')
calls.q4 <- subset(calls, calls$month == '04' |
                        calls$month == '05' |
                        calls$month == '06')
calls.q1$quarter <- "Q1"
calls.q2$quarter <- "Q2"
calls.q3$quarter <- "Q3"
calls.q4$quarter <- "Q4"
calls2 <- rbind(calls.q1, calls.q2, calls.q3, calls.q4)

### Seasons - FIX THESE
calls.fall <- subset(calls2, calls2$month == '08' |
                     calls2$month == '09' |
                     calls2$month == '10')
calls.winter <- subset(calls2, calls2$month == '11' |
                     calls2$month == '12' |
                     calls2$month == '01')
calls.spring <- subset(calls2, calls2$month == '02' |
                     calls2$month == '03' |
                     calls2$month == '04')
calls.summer <- subset(calls2, calls2$month == '05' |
                     calls2$month == '06' |
                     calls2$month == '07')
calls.fall$season <- "FALL"
calls.winter$season <- "WINTER"
calls.spring$season <- "SPRING"
calls.summer$season <- "SUMMER"

calls.full <- rbind(calls.fall, calls.winter, calls.spring, calls.summer)

##### Step 3: Subset
# shows you what call types are available:
unique(calls$type)

calls.summary <- calls %>%
  group_by(type) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT),2))

# subsetting, one of three ways:
calls.subset <- subset(calls, calls$type == 'MISC')

# for two call types:
calls.subset <- subset(calls, calls$type == 'MISC' |
                          calls$type == 'SERVICE')

# for more than two calls:
calls.group <- c("MISC", "SERVICE", "UNKNOWN", "Special Detail")

calls.subset <- calls %>%
  filter(calls$type %in% calls.group)

##### Step 4: Groups (Presidents, COVID, plus 1)
# Obama
subset.obama1 <- subset(calls.subset, calls.subset$date >= '2009-01-20' &
                         calls.subset$date <= '2013-01-19')
subset.obama2 <- subset(calls.subset, calls.subset$date >= '2013-01-20' &
                          calls.subset$date <= '2017-01-19')
# Trump
subset.trump <- subset(calls.subset, calls.subset$date >= '2017-01-20' &
                          calls.subset$date <= '2021-01-19')
# Biden
subset.biden <- subset(calls.subset, calls.subset$date >= '2021-01-20' &
                          calls.subset$date <= '2025-01-19')
# COVID
subset.precovid <- subset(calls.subset, calls.subset$date >= '2016-07-01' &
                          calls.subset$date < '2020-03-01')
subset.postcovid <- subset(calls.subset, calls.subset$date >= '2020-03-01')

# January 6th attacks
subset.prejan6 <- subset(calls.subset, calls.subset$date >= '2018-01-01' &
                            calls.subset$date < '2021-01-06')
subset.postjan6 <- subset(calls.subset, calls.subset$date > '2021-01-06')

##### Step 5: Choose time measure
# you pick one: weeks, months, quarters, OR seasons
summary.obama1 <- subset.obama1 %>%
  group_by(year, week) %>%
  summarise(count = n())

summary.obama2 <- subset.obama2 %>%
  group_by(year, week) %>%
  summarise(count = n())

summary.trump <- subset.trump %>%
  group_by(year, week) %>%
  summarise(count = n())

summary.biden <- subset.biden %>%
  group_by(year, week) %>%
  summarise(count = n())

summary.precovid <- subset.precovid %>%
  group_by(year, week) %>%
  summarise(count = n())

summary.postcovid <- subset.precovid %>%
  group_by(year, week) %>%
  summarise(count = n())

summary.prejan6 <- subset.prejan6 %>%
  group_by(year, week) %>%
  summarise(count = n())

summary.postjan6 <- subset.postjan6 %>%
  group_by(year, week) %>%
  summarise(count = n())

##### Step 6: Run t tests
t.test(summary.obama1$count, summary.obama2$count)
t.test(summary.obama2$count, summary.trump$count)
t.test(summary.trump$count, summary.biden$count)

7. Pick best on
## Find best relationship. 
## Map hotspots and find changes in areas