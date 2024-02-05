# Step 0
install.packages('tidyverse')
library(tidyverse)

# Step 1
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                          "12PArV_t_kvonL098R6zSQt6PSTmP1GMt"))

# Step 2
ggplot()
ggplot(calls)
ggplot(calls, aes(type))
ggplot(calls, aes(type)) + geom_bar(stat = "count")
ggplot(calls, aes(type)) + geom_bar(stat = "count") +
  facet_wrap(~year)

# Step 3
### Enrich
calls$date <- as.Date(calls$date)
calls$month <- months(calls$date)
calls$month2 <- month(calls$date)
calls$dow <- weekdays(calls$date)

### Subsetting
##### specific year
calls.2023 <- subset(calls, calls$year == '2023')
ggplot(calls.2023, aes(dow)) + geom_bar(stat = "count")
##### specific call type
calls.noise <- subset(calls, calls$type == 'NOISE COMPLAINT')
ggplot(calls.noise, aes(month2)) + geom_bar(stat = 'count')
##### multiple call types
calls.random <- subset(calls, calls$type == 'NOISE COMPLAINT' | calls$type == 'TOW' |
                         calls$type == 'OVERDOSE' | calls$type == 'TRAFFIC STOP')

##### new graphs!
ggplot(calls.random, aes(type)) + geom_bar(stat = 'count', aes(fill = month))

ggplot(calls.random, aes(type)) + geom_bar(stat = 'count', aes(fill = month),
                                           position = position_stack(reverse = TRUE))
ggplot(calls.random, aes(type)) + geom_bar(stat = 'count', aes(fill = month),
                                           position = position_stack(reverse = TRUE)) +
  theme_classic()
ggplot(calls.random, aes(type)) + geom_bar(stat = 'count', aes(fill = month),
                                           position = position_stack(reverse = TRUE)) +
  theme_classic() + theme(legend.position = "bottom")
ggplot(calls.random, aes(type)) + geom_bar(stat = 'count', aes(fill = month),
                                           position = position_stack(reverse = TRUE)) +
  theme_classic() + theme(legend.position = "right") +
  facet_wrap(~hour)

# formatting
graph <- ggplot(calls.noise, aes(month2)) + geom_bar(stat = 'count')
graph + 
  labs(
    title = "My Favorite Graph",
    x = "Month of Year",
    y = "Call Count"
  ) + theme_classic()


