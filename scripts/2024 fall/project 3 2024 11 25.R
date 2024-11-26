# 0. Load packages
install.packages('readxl')
library(readxl)
library(tidyverse)

# 1. Get Data
### 1a. CHDS: https://www.chds.us/sssc/data-map/
data.chds <- read_xlsx("/Users/matthewdanna/Downloads/ssdb_data_2022.xlsx",
                       sheet = "INCIDENT")
# 1970-2022
# K-12
# No coordinates

### 1b. Everytown: https://everytownresearch.org/maps/gunfire-on-school-grounds/
data.everytown <- read.csv("https://everytownresearch.org/wp-content/uploads/sites/4/etown-maps/gunfire-on-school-grounds/data.csv",
                           stringsAsFactors = FALSE)
# 2013-today
# all schools
# has coordinates

### Violence Prevention Project: https://www.theviolenceproject.org/databases/
### 1c. K-12
data.vpp.k12 <- read.csv("/Users/matthewdanna/Downloads/K-12 School Homicide Incidents.csv",
                         stringsAsFactors = FALSE)
# 2001-2023
# K-12

### 1d. college
data.vpp.college <- read.csv("/Users/matthewdanna/Downloads/Higher Education Homicide Incidents.csv",
                             stringsAsFactors = FALSE)
# 2000*-2023
# Only college

# 2. Clean Data
## Date cleaning
### 2a. CHDS
data.chds$DATE <- as.Date(data.chds$Date)
data.chds$YEAR <- substr(data.chds$DATE, 0, 4)

### 2b. Everytown
data.everytown$DATE <- as.Date(data.everytown$Incident.Date)
data.everytown$YEARMONTH <- substr(data.everytown$DATE, 0, 7)

### 2c. VPP K-12
data.vpp.k12$DATE <- as.Date(data.vpp.k12$Full_Date)
data.vpp.k12$YEAR <- substr(data.vpp.k12$DATE, 0, 4)
data.vpp.k12$YEARMONTH <- substr(data.vpp.k12$DATE, 0, 7)

### 2d. VPP College
data.vpp.college$DATE <- as.Date(data.vpp.college$Full_Date)
data.vpp.college$YEAR <- substr(data.vpp.college$DATE, 0, 4)
data.vpp.college$YEARMONTH <- substr(data.vpp.college$DATE, 0, 7)

## Victim Counts
### 2a. CHDS - no victim counts in this table

### 2b. Everytown
data.everytown$VICTIMS <- data.everytown$Number.Killed + data.everytown$Number.Wounded

### 2c. VPP K-12
data.vpp.k12$VICTIMS <- data.vpp.k12$Victims_Killed + data.vpp.k12$Victims_Injured

### 2d. VPP College
data.vpp.college$VICTIMS <- data.vpp.college$Victims_Killed + data.vpp.college$Victims_Wounded

# 3. Divide dataset based on a break-point
## Breakpoints should be meaningful and based on the underlying data
## IMPORTANT POINT TO CONSIDER: to you include the breakpoint in the pre, the post, or neither?

### 3a. CHDS
### Breakpoint: 1/1/1997
data.chds.pre <- subset(data.chds, data.chds$DATE < '1997-01-01')
data.chds.post <- subset(data.chds, data.chds$DATE > '1997-01-01')

### 3b. Everytown
### Breakpoint: 1/1/2019
data.everytown.pre <- subset(data.everytown, data.everytown$DATE < '2019-01-01')
data.everytown.post <- subset(data.everytown, data.everytown$DATE > '2019-01-01')

### 3c. VPP K-12
### Breakpoint: 1/1/2013
data.vpp.k12.pre <- subset(data.vpp.k12, data.vpp.k12$DATE < '2013-01-01')
data.vpp.k12.post <- subset(data.vpp.k12, data.vpp.k12$DATE > '2013-01-01')

### 3d. VPP College
### Breakpoint: 1/1/2013
data.vpp.college.pre <- subset(data.vpp.college, data.vpp.college$DATE < '2013-01-01')
data.vpp.college.post <- subset(data.vpp.college, data.vpp.college$DATE > '2013-01-01')

# 4. Summarize your data
## Summarize by time AND events OR victims
## Time: by year, or by month
## Events: counting incidents
## Victims: summing fatalities, wounded, or total

### 4a. CHDS - summarizing by year and events
chds.pre <- data.chds.pre %>%
  group_by(YEAR) %>%
  summarise(EVENT.COUNT = n())

chds.post <- data.chds.post %>%
  group_by(YEAR) %>%
  summarise(EVENT.COUNT = n())

### 4b. Everytown - summarizing by month and events
everytown.pre <- data.everytown.pre %>%
  group_by(YEARMONTH) %>%
  summarise(EVENT.COUNT = n())
            
everytown.post <- data.everytown.post %>%
  group_by(YEARMONTH) %>%
  summarise(EVENT.COUNT = n())

### 4c. VPP K-12 - summarizing by month and events
k12.pre <- data.vpp.k12.pre %>%
  group_by(YEARMONTH) %>%
  summarise(EVENT.COUNT = n())

k12.post <- data.vpp.k12.post %>%
  group_by(YEARMONTH) %>%
  summarise(EVENT.COUNT = n())

### 4d. VPP College - summarizing by year and events
college.pre <- data.vpp.college.pre %>%
  group_by(YEAR) %>%
  summarise(EVENT.COUNT = n())

college.post <- data.vpp.college.post %>%
  group_by(YEAR) %>%
  summarise(EVENT.COUNT = n())

# 5. t test
t.test(chds.pre$EVENT.COUNT, chds.post$EVENT.COUNT)
t.test(everytown.pre$EVENT.COUNT, everytown.post$EVENT.COUNT)
t.test(k12.pre$EVENT.COUNT, k12.post$EVENT.COUNT)
t.test(college.pre$EVENT.COUNT, college.post$EVENT.COUNT)
