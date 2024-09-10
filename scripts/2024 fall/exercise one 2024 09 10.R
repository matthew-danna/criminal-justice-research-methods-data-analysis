##### Step 0: Prep
install.packages('tidyverse')
library(tidyverse)

##### Step 1: Get data
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                          "1gniFP35GEfxycgpKGHAzkgSYmjTteTKm"))

##### Step 1.5: subset data
calls <- subset(calls, calls$type == 'TOW' | calls$type == 'FORGERY' |
                  calls$type == 'ASSAULT' | calls$type == 'WELFARE CHECK' |
                  calls$type == 'SUSPICIOUS')

calls$hour <- as.factor(calls$hour)

##### Step 2: Graphs
ggplot()
ggplot(calls, aes(type))
ggplot(calls, aes(type)) + geom_bar(stat = "count")
ggplot(calls, aes(type)) + geom_bar(stat = 'count', aes(fill = hour))
ggplot(calls, aes(type)) + 
  geom_bar(stat = "count", aes(fill = hour), position = position_stack(reverse = TRUE))
ggplot(calls, aes(type)) + 
  geom_bar(stat = "count", aes(fill = hour), position = position_stack(reverse = TRUE)) +
  theme_classic()
ggplot(calls, aes(type)) + 
  geom_bar(stat = "count", aes(fill = hour), position = position_stack(reverse = TRUE)) +
  theme_classic() + theme(legend.position = "bottom")
ggplot(calls, aes(type)) + 
  geom_bar(stat = "count", aes(fill = hour), position = position_stack(reverse = TRUE)) +
  theme_classic() + facet_wrap(~year)

graph <- ggplot(calls, aes(type)) + 
  geom_bar(stat = "count", aes(fill = hour), position = position_stack(reverse = TRUE)) +
  theme_classic()

new.graph <- graph +
  labs(
    title = "MY GRAPH",
    x = "My x axis",
    y = "My y axis"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.ticks = element_blank()
  )

new.graph

ggplot(calls, aes(type)) + 
  geom_bar(stat = "count", aes(fill = hour), position = position_stack(reverse = TRUE)) +
  theme_classic() + facet_wrap(~year) + 
  labs(title = "My Other Graph",
       x = "Call Types",
       y = "Stuff") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.ticks = element_blank()
  )

##### Step 3: Better data?
calls$date <- as.Date(calls$date, format = '%Y-%m-%d')
calls$months <- months(calls$date)
calls$month <- month(calls$date)
calls$week <- week(calls$date)

