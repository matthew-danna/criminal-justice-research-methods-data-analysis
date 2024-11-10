##### FBI
# Download the NIBRS data
# Clean up the sheet in Excel 
# Delete headers and footers, adjust columns, and format numbers
# Save as a CSV
fbi.arrests <- read.csv("C:/Users/mdanna2/Downloads/fbi arrests.csv",
                        stringsAsFactors = FALSE)

##### Project Ideas
# NY or CA + DEA: PCT of NY to other states, PCT of NY activity over time compared to Inmate year
# NY + Census for Demographics: State, All inmates, Gang inmates, non-Gang inmates
# NY + FBI

# Choose NY, DEA, or FBI

# http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
library(tidyverse)
install.packages('gplots')
library(gplots)
library('graphics')
install.packages('vcd')
library('vcd')
library(corrplot)

# Download from website
# Read into R
ny.inmates <- read.csv("/Users/matthewdanna/Downloads/Daily_Inmates_In_Custody_20241109.csv", 
                       stringsAsFactors = FALSE)

# Get charge code data
# https://www.criminaljustice.ny.gov/crimnet/ccman/codedlawmanual.xlsx
# Clean it, save as CSV
ny.charges <- read.csv("/Users/matthewdanna/Downloads/codedlawmanual.csv", stringsAsFactors = FALSE)

# create a subset for gang members
ny.inmates.gang <- subset(ny.inmates, ny.inmates$SRG_FLG == 'Y')
# create a subset for non-gang members
ny.inmates.nogang <- subset(ny.inmates, ny.inmates$SRG_FLG == 'N')


# Summarize
ny.offense <- ny.inmates %>%
  group_by(TOP_CHARGE) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

# Gender
ny.sex.gang <- ny.inmates.gang %>%
  group_by(GENDER) %>%
  summarise(GANG = n())
ny.sex.ng <- ny.inmates.nogang %>%
  group_by(GENDER) %>%
  summarise(NOGANG = n())

ny.sex <- ny.sex.gang %>% left_join(ny.sex.ng, by = 'GENDER')
colnames(ny.sex) <- c("CATEGORY", "GANG", "NOGANG")
ny.sex <- subset(ny.sex, ny.sex$CATEGORY != "")

# Race
ny.race.gang <- ny.inmates.gang %>%
  group_by(RACE) %>%
  summarise(GANG = n())
ny.race.ng <- ny.inmates.nogang %>%
  group_by(RACE) %>%
  summarise(NOGANG = n())

ny.race <- ny.race.gang %>% left_join(ny.race.ng, by = 'RACE')
colnames(ny.race) <- c("CATEGORY", "GANG", "NOGANG")
ny.race <- subset(ny.race, ny.race$CATEGORY != "")

# Custody Level
ny.cust.gang <- ny.inmates.gang %>%
  group_by(CUSTODY_LEVEL) %>%
  summarise(GANG = n())
ny.cust.ng <- ny.inmates.nogang %>%
  group_by(CUSTODY_LEVEL) %>%
  summarise(NOGANG = n())

ny.cust <- ny.cust.gang %>% left_join(ny.cust.ng, by = 'CUSTODY_LEVEL')
colnames(ny.cust) <- c("CATEGORY", "GANG", "NOGANG")
ny.cust <- subset(ny.cust, ny.cust$CATEGORY != "")

# MENTAL HEALTH

# TOP CHARGE
ny.inmate.charges <- ny.inmates %>%
  group_by(TOP_CHARGE) %>%
  summarise(CHARGES = n()) %>%
  mutate(PCT = round(CHARGES/sum(CHARGES)*100,2))

# Combine
# this list depends on your choices
ny.combo <- rbind(ny.sex, ny.race, ny.cust)
ny.sum <- ny.combo[c(2:3)]
# these values depend on your choices
row.names(ny.sum) <- c("FEMALE", "MALE", "ASIAN", "BLACK", "NAT AM", "OTHER", "UNK", "WHITE", "MAX", "MED", "MIN")

# Stats
# The chi-square test of independence is used to analyze the frequency table (i.e. contingency table) 
# formed by two categorical variables. The chi-square test evaluates whether there is a significant 
# association between the categories of the two variables. 
# Chi-square test examines whether rows and columns of a contingency table are statistically 
# significantly associated.
# Null hypothesis (H0): the row and the column variables of the contingency table are independent.
# Alternative hypothesis (H1): row and column variables are dependent
# For each cell of the table, we have to calculate the expected value under null hypothesis.

chisq <- chisq.test(ny.sum)

# the result shows:
# statistic: the value the chi-squared test statistic.
# parameter: the degrees of freedom
# p.value: the p-value of the test
# observed: the observed count
# expected: the expected count
chisq

# the p-value
chisq$p.value
# observed counts
chisq$observed
# expected counts
round(chisq$expected,2)
# residuals
# Cells with the highest absolute standardized residuals contribute the most to the Chi-square score
round(chisq$residuals, 3)
# contribution percentage
# The relative contribution of each cell to the total Chi-square score give some indication of the 
# nature of the dependency between rows and columns of the contingency table.
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)

# Visuals
# reformat the data
ny.table <- as.table(as.matrix(ny.sum))

# Balloon plot: dot size = relative magnitude of component
balloonplot(t(ny.table), main ="Characteristics", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# Mosaic plot: 
# Blue = observed value is higher than the expected value if the data were random
# Red = observed value is lower than the expected value if the data were random
mosaicplot(ny.table, shade = TRUE, las=2,
           main = "Characteristics")

# Association plot (same legend as above)
# set this number to the number of rows in your table
assoc(head(ny.table, 11), shade = TRUE, las=3) 

# Positive residuals are in blue. 
# Positive values in cells show positive association (attraction) between the corresponding row and column variables.
# Negative residuals are in red. 
# This implies a repulsion (negative association) between the corresponding row and column variables.
corrplot(chisq$residuals, is.cor = FALSE)

# Visualize the contribution
corrplot(contrib, is.cor = FALSE)


##### DEA
# Download
# Unzip
# Open table 2 in Excel
# Delete rows 1 and 2
# Delete the last 11 rows, of footers
# Move rows of states into new columns
# Format every state column as a number
# Save as a CSV
# Read into R:
dea.seizures <- read.csv("/Users/matthewdanna/Downloads/2022NFLISWebsiteTable2.csv",
                         stringsAsFactors = FALSE)

# Pick drugs of interest
dea.subset <- subset(dea.seizures, 
                     dea.seizures$Drug == 'Cocaine' | 
                       dea.seizures$Drug == 'Fentanyl' |
                       dea.seizures$Drug == 'Heroin' |
                       dea.seizures$Drug == 'Methamphetamine')

# Pick states of interest
dea.states <- as.data.frame(t(dea.subset[,-1]))
colnames(dea.states) <- dea.subset$Drug
dea.states$Total <- dea.states$Cocaine + dea.states$Fentanyl + 
  dea.states$Heroin + dea.states$Methamphetamine

dea.subset <- dea.subset[c(1,6,45,37,34)]

dea <- dea.subset[c(2:5)]
row.names(dea) <- c("COCAINE", "FENTANYL", "HEROIN", "METH")

# stats
chisq <- chisq.test(dea)
chisq
chisq$p.value
dea.expected <- data.frame(round(chisq$expected,2))
dea.residentials <- data.frame(round(chisq$residuals, 3))
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)

# visuals
dea.table <- as.table(as.matrix(dea))
balloonplot(t(dea.table), main ="Characteristics", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot(dea.table, shade = TRUE, las=2,
           main = "Characteristics")
assoc(head(dea.table, 11), shade = TRUE, las=3) 
corrplot(chisq$residuals, is.cor = FALSE)
corrplot(contrib, is.cor = FALSE)

