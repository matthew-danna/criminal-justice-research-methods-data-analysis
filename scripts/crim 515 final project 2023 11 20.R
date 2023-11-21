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

##### 3: Summarize your events by a time measure

##### 4: Identify trends over time

##### 5: Determine a break-point

##### 6: Calculate a t test to identify statistically significant changes

##### 7: Visualize the findings
