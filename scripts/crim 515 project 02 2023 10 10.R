# Libaries
library(gtrendsR)

# Step 1: Data
wapo.data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv", 
                      stringsAsFactors = FALSE)

today <- as.character(Sys.Date())
time.range <- paste("2015-01-01", today, sep = " ")
trends1 <- gtrends("police shooting", geo = "US", time = time.range)
