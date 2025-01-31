# STEP 0: get libraries
install.packages('tidyverse')
library(tidyverse)

# STEP 1: get data
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                          "1wKXnsCgcEuipeU9Xv3xBUBAA-B2wprtq"))

# STEP 2: make graphs
ggplot()
ggplot(calls, aes(year))
ggplot(calls, aes(year)) + geom_bar(stat = "count")
ggplot(calls, aes(year)) + geom_bar(stat = "count", 
                                    aes(fill = as.factor(hour)))
ggplot(calls, aes(year)) + 
  geom_bar(stat = "count", aes(fill = as.factor(hour)), 
                                    position = position_stack(reverse = TRUE))

ggplot(calls, aes(year)) + 
  geom_bar(stat = "count", aes(fill = as.factor(hour)), 
           position = position_stack(reverse = TRUE)) +
  theme_classic()

ggplot(calls, aes(year)) + 
  geom_bar(stat = "count", aes(fill = as.factor(hour)), 
           position = position_stack(reverse = TRUE)) +
  theme_classic() + theme(legend.position = "bottom")

ggplot(calls, aes(as.factor(hour))) + 
  geom_bar(stat = "count") +
  theme_classic() + facet_wrap(~year)

ggplot(calls, aes(as.factor(hour))) + 
  geom_bar(stat = "count") +
  labs(
    title = "Your title here",
    x = "x axis label here",
    y = "y axis label here"
  )

calls$call.type <- case_when(
  calls$type %in%
    c(
      "BURGLARY",
      "VANDALISM/GRAFFITI",
      "LARCENY",
      "AUTO THEFT",
      "STOLEN AUTO") ~ "Property",
  calls$type %in%
    c("SEXUAL ASSAULT/RAPE", "ASSAULT", "ROBBERY", "STABBING")
  ~ "Person")

calls %>% filter(hour != "NA") %>%
  ggplot() +
  geom_line(aes(x = as.factor(hour)), stat = "count", group = 1, color = "blue", size = 1) +
  labs(title = "Calls by Hour Reported \n2007 - 2024", x = "Hour Reported", y = "Number of Calls (Thousands)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        panel.background = element_blank()) +
  scale_y_continuous(limits = c(0, 25000), breaks = c(0, 5000,10000,15000,20000,25000), labels = c(0,5,10,15,20,25))

