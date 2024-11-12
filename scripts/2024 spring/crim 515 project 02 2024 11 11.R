# Libraries
install.packages('tidyverse')
install.packages('gplots')
install.packages('graphics')
install.packages('vcd')
install.packages('corrplot')
library(tidyverse)
library(gplots)
library(graphics)
library(vcd)
library(corrplot)

##########
########## NYC Daily Inmate Data
##########

# Get Data
### update the file path
ny.inmates <- read.csv("C:/Users/mdanna2/Downloads/Daily_Inmates_In_Custody_20241111.csv",
                       stringsAsFactors = FALSE)

# subset gang and no gang
ny.inmates.gang <- subset(ny.inmates, ny.inmates$SRG_FLG == 'Y')
ny.inmates.nogang <- subset(ny.inmates, ny.inmates$SRG_FLG == 'N')

# Summary tables
### counts by sex, race, custody, mental observation

ny.sex.gang <- ny.inmates.gang %>%
  group_by(GENDER) %>%
  summarise(GANG = n())
ny.sex.ng <- ny.inmates.nogang %>%
  group_by(GENDER) %>%
  summarise(NOGANG = n())

ny.race.gang <- ny.inmates.gang %>%
  group_by(RACE) %>%
  summarise(GANG = n())
ny.race.ng <- ny.inmates.nogang %>%
  group_by(RACE) %>%
  summarise(NOGANG = n())

ny.cust.gang <- ny.inmates.gang %>%
  group_by(CUSTODY_LEVEL) %>%
  summarise(GANG = n())
ny.cust.ng <- ny.inmates.nogang %>%
  group_by(CUSTODY_LEVEL) %>%
  summarise(NOGANG = n())

ny.mental.gang <- ny.inmates.gang %>%
  group_by(BRADH) %>%
  summarise(GANG = n())
ny.mental.ng <- ny.inmates.nogang %>%
  group_by(BRADH) %>%
  summarise(NOGANG = n())

# formatting summary tables
ny.sex <- ny.sex.gang %>% left_join(ny.sex.ng, by = 'GENDER')
ny.race <- ny.race.gang %>% left_join(ny.race.ng, by = 'RACE')
ny.cust <- ny.cust.gang %>% left_join(ny.cust.ng, by = 'CUSTODY_LEVEL')
ny.mental <- ny.mental.gang %>% left_join(ny.mental.ng, by = 'BRADH')

colnames(ny.sex) <- c("CATEGORY", "GANG", "NOGANG")
colnames(ny.race) <- c("CATEGORY", "GANG", "NOGANG")
colnames(ny.cust) <- c("CATEGORY", "GANG", "NOGANG")
colnames(ny.mental) <- c("CATEGORY", "GANG", "NOGANG")

ny.sex <- subset(ny.sex, ny.sex$CATEGORY != "")
ny.race <- subset(ny.race, ny.race$CATEGORY != "")
ny.cust <- subset(ny.cust, ny.cust$CATEGORY != "")

ny.combo <- rbind(ny.sex, ny.race, ny.cust, ny.mental)

ny.sum <- ny.combo[c(2:3)]
row.names(ny.sum) <- c("FEMALE", "MALE", "ASIAN", "BLACK", "NAT AM", 
                       "OTHER", "UNK", "WHITE", "MAX", "MED", "MIN",
                       "MENTAL", "NON.MENTAL")

# Stats
ny.chisq <- chisq.test(ny.sum)
ny.chisq

ny.chisq$p.value
ny.chisq$observed
round(ny.chisq$expected,2)
round(ny.chisq$residuals, 3)
ny.contrib <- 100*ny.chisq$residuals^2/ny.chisq$statistic
round(ny.contrib, 3)

# Visuals
ny.table <- as.table(as.matrix(ny.sum))

# this is a good one to keep
balloonplot(t(ny.table), main ="Characteristics", xlab ="Inmates", 
            ylab="Components",
            label = FALSE, show.margins = FALSE)

# choose either a mosaic or association
mosaicplot(ny.table, shade = TRUE, las=2,
           main = "Characteristics")
assoc(head(ny.table, 11), shade = TRUE, las=3) 

# choose either a residual or contribution
corrplot(ny.chisq$residuals, is.cor = FALSE)
corrplot(ny.contrib, is.cor = FALSE)

##########
########## DEA NFLIS
##########

dea.seizures <- read.csv("C:/Users/mdanna2/Downloads/nflis.csv",
                         stringsAsFactors = FALSE)

unique(dea.seizures$Drug)

dea.subset <- subset(dea.seizures, dea.seizures$Drug == 'MDMA' |
                       dea.seizures$Drug == 'Oxycodone' |
                       dea.seizures$Drug == 'Fentanyl' |
                       dea.seizures$Drug == 'Phencyclidine (PCP)')

dea.states <- as.data.frame(t(dea.subset[,-1]))
colnames(dea.states) <- dea.subset$Drug
dea.states$Total <- dea.states$Fentanyl + dea.states$MDMA +
  dea.states$Oxycodone + dea.states$`Phencyclidine (PCP)`

# this filter is based on OH, AZ, PA, CA, NY, NJ
colnames(dea.subset)
dea.subset <- dea.subset[c(1,37,4,40,6,34,32)]
dea <- dea.subset[c(2:5)]
row.names(dea) <- c("MDMA", "Oxy", "Fentanyl", "PCP")

# Stats
dea.chisq <- chisq.test(dea)
dea.chisq

dea.chisq$p.value

dea.expected <- data.frame(round(dea.chisq$expected,2))
dea.residuals <- data.frame(round(dea.chisq$residuals, 3))
dea.contrib <- 100*dea.chisq$residuals^2/dea.chisq$statistic

# Visuals
dea.table <- as.table(as.matrix(dea))

balloonplot(t(dea.table), main ="Characteristics", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot(dea.table, shade = TRUE, las=2,
           main = "Characteristics")
assoc(head(dea.table, 11), shade = TRUE, las=3) 
corrplot(dea.chisq$residuals, is.cor = FALSE)
corrplot(dea.contrib, is.cor = FALSE)



