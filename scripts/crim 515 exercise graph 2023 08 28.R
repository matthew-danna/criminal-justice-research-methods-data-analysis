# Exercise: Graphing

# Step 0
install.packages('tidyverse')
library(tidyverse)

# Step 1
dc.data <- read.csv("https://opendata.arcgis.com/datasets/89561a4f02ba46cca3c42333425d1b87_5.csv",
                    stringsAsFactors = FALSE)

# Step 2
ggplot()
ggplot(dc.data, aes(SHIFT))
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count")
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count", aes(fill = OFFENSE))
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count", aes(fill = OFFENSE), 
                                       position = position_stack(reverse = TRUE))
ggplot(dc.data, aes(SHIFT)) + 
  geom_bar(stat = "count", aes(fill = OFFENSE), position = position_stack(reverse = TRUE)) + 
  coord_flip()
ggplot(dc.data, aes(SHIFT)) + 
  geom_bar(stat = "count", aes(fill = OFFENSE), position = position_stack(reverse = TRUE)) + 
  theme_classic()
ggplot(dc.data, aes(SHIFT)) + 
  geom_bar(stat = "count", aes(fill = OFFENSE), position = position_stack(reverse = TRUE)) + 
  theme_classic() +
  facet_wrap(~DISTRICT)

graph <- ggplot(dc.data, aes(SHIFT)) + 
  geom_bar(stat = "count", aes(fill = OFFENSE), position = position_stack(reverse = TRUE)) + 
  theme_classic()

graph1 <- graph + labs(title = "TITLE HERE",
                       x = "another label",
                       y = "yet another one",
                       subtitle = "your name")
graph1

ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count") + labs(title = "My Graph")
