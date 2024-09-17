# Step 0
install.packages('tidyverse')
library(tidyverse)

# Step 1
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                          "1gniFP35GEfxycgpKGHAzkgSYmjTteTKm"))

# Step 2
ggplot()
ggplot(calls, aes(hour))
ggplot(calls, aes(hour)) + geom_bar(stat = "count")
ggplot(calls, aes(hour)) + geom_bar(stat = "count") +
  theme_minimal()
ggplot(calls, aes(hour)) + geom_bar(stat = "count") +
  facet_wrap(~year) + theme_classic()
ggplot(calls, aes(hour)) + geom_bar(stat = "count") +
  facet_wrap(~year) + theme_classic() +
  labs(title = "My Graph", x = "Hour of Day", y = "Call Count")

# Step 3
calls$date <- as.Date(calls$date, format = "%Y-%m-%d")
calls$month <- month(calls$date)
calls$months <- months(calls$date)
calls$dow <- weekdays(calls$date)
