#### Data interactions identified in class:
# GVA (2021) + RAND Laws (2021) = Per State
# GVA (202x-2024) + Everytown (Current) = Per State
# GVA (2020-2024) + Suicides (2020-2024) = Per State
# GVA () + Seizures () = Mass Shootings per State per Year to Seizure per Year 
# MJ + Seizures = Per State and Year

install.packages('tidyverse')
library(tidyverse)

#### Create combined dataset
# GVA
data.gva.2024 <- read.csv("C:/Users/mdanna2/Downloads/gva2024.csv", stringsAsFactors = FALSE)
data.gva.2023 <- read.csv("C:/Users/mdanna2/Downloads/gva2023.csv", stringsAsFactors = FALSE)
data.gva.2022 <- read.csv("C:/Users/mdanna2/Downloads/gva2022.csv", stringsAsFactors = FALSE)
data.gva.2021 <- read.csv("C:/Users/mdanna2/Downloads/gva2021.csv", stringsAsFactors = FALSE)
data.gva.2020 <- read.csv("C:/Users/mdanna2/Downloads/gva2020.csv", stringsAsFactors = FALSE)

data.gva <- rbind(data.gva.2024, data.gva.2023, data.gva.2022, data.gva.2021, data.gva.2020)

data.gva$Date <- as.Date(data.gva$Incident.Date, format = "%B %d, %Y")
data.gva$Victims.Total <- data.gva$Victims.Killed + data.gva$Victims.Injured
data.gva$Year <- substr(data.gva$Date, 0, 4)

sum.gva.events <- data.gva %>%
  group_by(State, Year) %>%
  summarise(Event.Count = n())
sum.gva.events$ID <- paste(sum.gva.events$State, 
                           sum.gva.events$Year, sep = "-")

sum.gva.victims <- data.gva %>%
  group_by(State, Year) %>%
  summarise(Victim.Count = sum(Victims.Total))
sum.gva.victims$ID <- paste(sum.gva.victims$State,
                            sum.gva.victims$Year, sep = "-")

sum.gva <- sum.gva.events %>%
  full_join(sum.gva.victims, by = "ID")
sum.gva <- sum.gva[c(1:4,7)]
names(sum.gva) <- c("State", "Year", "Event.Count", "ID", 
                    "Victim.Count")  

# MJ
data.mj <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQBEbQoWMn_P81DuwmlQC0_jr2sJDzkkC0mvF6WLcM53ZYXi8RMfUlunvP1B5W0jRrJvH-wc-WGjDB1/pub?gid=0&single=true&output=csv", stringsAsFactors = FALSE)
data.mj$temp.month <- substr(data.mj$date, 0, 2)
data.mj$temp.month <- gsub("/", "", data.mj$temp.month)
data.mj$temp.day <- substr(data.mj$date, (nchar(data.mj$temp.month)+2), (nchar(data.mj$temp.month)+3))
data.mj$temp.day <- gsub("/", "", data.mj$temp.day)
data.mj$temp.year <- substr(data.mj$date, nchar(data.mj$date)-1, nchar(data.mj$date))
data.mj$temp.date <- paste(data.mj$temp.month, data.mj$temp.day, data.mj$temp.year, sep = "/")
data.mj$date <- as.Date(data.mj$temp.date, format = "%m/%d/%y")
data.mj <- separate(data.mj, location, into = c("city", "state"), sep = ", ")

sum.mj.events <- data.mj %>%
  group_by(year) %>%
  summarise(event.count = n())

sum.mj.victims <- data.mj %>%
  group_by(year) %>%
  summarise(victim.count = sum(total_victims))

sum.mj <- sum.mj.events %>%
  full_join(sum.mj.victims, by = 'year')
names(sum.mj) <- c("Year", "Events", "Victims")
sum.mj$Year <- as.character(sum.mj$Year)

# Laws
data.laws <- read.csv("C:/Users/mdanna2/Downloads/gun laws sample.csv",
                      stringsAsFactors = FALSE)
data.laws$ID <- paste(data.laws$State, data.laws$Year, sep = "-")

# Suicides
data.cdc.all <- read.delim("C:/Users/mdanna2/Downloads/state year all.txt")
data.cdc.gun <- read.delim("C:/Users/mdanna2/Downloads/state year firearm.txt")

data.cdc.all$ID <- paste(data.cdc.all$Occurrence.State, 
                         data.cdc.all$Year.Code, sep = "-")
data.cdc.gun$ID <- paste(data.cdc.gun$Occurrence.State, 
                         data.cdc.gun$Year.Code, sep = "-")

data.cdc.all2 <- subset(data.cdc.all, data.cdc.all$Notes == '')
data.cdc.gun2 <- subset(data.cdc.gun, data.cdc.gun$Notes == '')

data.cdc <- data.cdc.all2 %>%
  left_join(data.cdc.gun2, by = "ID")

data.cdc <- data.cdc[c(2,5,6,16,10)]
names(data.cdc) <- c("State", "Year", "Suicides.All",
                     "Suicides.Gun", "ID")

# CBP
data.cbp <- read.csv("C:/Users/mdanna2/Downloads/cbp seizures fixed.csv", stringsAsFactors = FALSE)
data.cbp2 <- subset(data.cbp, 
                    data.cbp$Inbound.Outbound == 'OUTBOUND' &
                      data.cbp$Seizure.Type == 'Weapons')
sum.cbp <- data.cbp2 %>%
  group_by(Year) %>%
  summarise(Gun.Count = sum(Quantity.Seized))
sum.cbp$Year <- as.character(sum.cbp$Year)

#### Analysis Methods

# joins
# GVA + Laws
join.gva.laws <- sum.gva %>% 
  full_join(data.laws, by = "ID")

# GVA + Suicides
join.gva.cdc <- sum.gva %>%
  full_join(data.cdc, by = "ID")
join.gva.cdc$Ownership <- (join.gva.cdc$Suicides.Gun/join.gva.cdc$Suicides.All)*100
  
# GVA + Seizures
join.gva.cbp <- sum.gva %>%
  full_join(sum.cbp, by = "Year")

# MJ + Seizures
join.mj.cbp <- sum.mj %>%
  full_join(sum.cbp, by = "Year")

##### VISUALS
# 5 fields to work with:
# State, Year, Events, Victims, Laws/Seizures/Ownership
library(tidyverse)

# 1. Find an insight
# 2. Subset your data based on that insight
# 3. Graph the subset

graph1 <- subset(join.gva.cdc, join.gva.cdc$Year.x == '2024')

ggplot(graph1, aes(x = State.x, y = Victim.Count)) +  
  geom_bar(stat = "identity") + coord_flip() +
  labs(
    title = "Mass Shooting Victims in 2024",
    x = "State",
    y = "Total Victims"
  )

ggplot(graph1, aes(x = State.x, y = Victim.Count)) +  
  geom_bar(stat = "identity") + 
  theme(axis.text.x= element_text(angle = 90)) + 
  labs(
    title = "Mass Shooting Victims in 2024",
    x = "State",
    y = "Total Victims"
  )

ggplot(graph1, aes(x = State.x, y = Victim.Count)) +  
  geom_bar(stat = "identity") + 
  theme(axis.text.x= element_text(angle = 90)) + 
  labs(
    title = "Mass Shooting Victims in 2024",
    x = "State",
    y = "Total Victims"
  )

graph2 <- subset(join.gva.cdc, join.gva.cdc$State.x == 'California')

ggplot(graph2, aes(x = Year.x, y = Event.Count)) +  
  geom_bar(stat = "identity") +
  labs(
    title = "Mass Shootings in California",
    x = "Year",
    y = "Event Count"
  )

ggplot(graph2, aes(x = Year.x, y = Ownership)) +  
  geom_bar(stat = "identity") +
  labs(
    title = "Firearm Ownership in California",
    x = "Year",
    y = "PCT of Suicides w/Firearm"
  )
