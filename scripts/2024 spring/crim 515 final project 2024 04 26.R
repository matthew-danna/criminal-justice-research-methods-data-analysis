library(tidyverse)
library(leaflet)

##### NOTES
# pick one dataset
# and potentially a subset of that dataset
# t test
# hotspots
# changes over time
# trends
# top activity

##### get data
# arrests
arrests <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                          "1YdW0ov1OIm9MNmPLIomfpAy8RO4p6gxG"))
# calls
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                            "1X48tHqgfDVTbPTQ4wKxVPFazLyGjOfxz"))
# crashes
crashes <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                            "118xvIlrqOQFsrPVh35CvPmPkNXElenK1"))
# crime
crime <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                            "1Sg7lkf6EioI87dZiGDsn03un8gxT4zw6"))

##### enrichments
# arrests
arrests$date <- as.Date(arrests$date)
arrests$dow <- weekdays(arrests$date)
arrests$week <- format(as.Date(arrests$date), "%U")
arrests$month <- substr(arrests$date, 6, 7)
arrests$year.month <- paste(arrests$year, arrests$month, sep = "-")

# calls
calls$date <- as.Date(calls$date)
calls$dow <- weekdays(calls$date)
calls$week <- format(as.Date(calls$date), "%U")
calls$month <- substr(calls$date, 6, 7)
calls$year.month <- paste(calls$year, calls$month, sep = "-")

# crashes
crashes$date <- as.Date(crashes$date)
crashes$dow <- weekdays(crashes$date)
crashes$week <- format(as.Date(crashes$date), "%U")
crashes$month <- substr(crashes$date, 6, 7)
crashes$year.month <- paste(crashes$year, crashes$month, sep = "-")

# crime
crime$date <- as.Date(crime$date.report)
crime$dow <- weekdays(crime$date)
crime$week <- format(as.Date(crime$date), "%U")
crime$month <- substr(crime$date, 6, 7)
crime$year.month <- paste(crime$year, crime$month, sep = "-")

##### filter data to the Fairfax Circle area
# please adjust these coordinate as necessary!
fc.arrests <- subset(arrests, arrests$lat > 38.859298 & arrests$lon > -77.283521)
fc.calls <- subset(calls, calls$lat > 38.859298 & calls$lon > -77.283521)
fc.crashes <- subset(crashes, crashes$lat > 38.859298 & crashes$lon > -77.283521)
fc.crime <- subset(crime, crime$lat > 38.859298 & crime$lon > -77.283521)

#### unique values
# change table and attribute names as necessary!
unique(fc.arrests$statute.description)
unique(fc.arrests$crime.code.description)
type.arrests <- fc.arrests %>%
  group_by(statute.description) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count), 2))
type.year.arrests <- fc.arrests %>%
  group_by(statute.description, year) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count), 2))

fc.subset.arrests <- subset(fc.arrests, fc.arrests$statute.description == 'DRUNK IN PUBLIC (DIP)')

##### next steps
# OPTIONAL t test: counts of events by week or year.month with a date that matters for a breakpoint
# hotspots: transparent points, densities, or contours. facet by DOW, Hour, event type, year, month
# changes over time: based on your summary tables, maybe t test, graphs, and maps
# trends: what has changed? what has stayed the same?



# map data
leaflet(arrests.fc) %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = "", 
             clusterOptions = markerClusterOptions())

va.roads <- roads("VA", "Fairfax city")
va.outline <- county_subdivisions("VA", "Fairfax city")

