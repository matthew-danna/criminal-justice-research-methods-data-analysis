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

3. Subset
4. Groups (presidents, COVID, plus 1)
5. Choose time measure
6. Run t tests
7. Pick best on
## Find best relationship. 
## Map hotspots and find changes in areas