##### New York Daily Inmate Data
# Download from website
# Read into R
ny.inmates <- read.csv("C:/Users/mdanna2/Downloads/Daily_Inmates_In_Custody_20241104.csv",
                       stringsAsFactors = FALSE)
# Format the date
ny.inmates$DATE <- as.Date(ny.inmates$ADMITTED_DT, format = '%m/%d/%Y')
# Add a year column
ny.inmates$YEAR <- substr(ny.inmates$DATE, 0, 4)
# create a subset for gang members
ny.inmates.gang <- subset(ny.inmates, ny.inmates$SRG_FLG == 'Y')
# create a subset for non-gang members
ny.inmates.nogang <- subset(ny.inmates, ny.inmates$SRG_FLG == 'N')

##### California Gang Data
# Download from website
# Save the FULL sheet as a CSV
# Read into R
ca.gangs <- read.csv('C:/Users/mdanna2/Downloads/calgang-ag-dataset-2023.csv',
                     stringsAsFactors = FALSE)

##### FBI
# Download the NIBRS data
# Clean up the sheet in Excel 
# Delete headers and footers, adjust columns, and format numbers
# Save as a CSV
fbi.arrests <- read.csv("C:/Users/mdanna2/Downloads/fbi arrests.csv",
                        stringsAsFactors = FALSE)

##### DEA
# Download the NFLIS data by year
# Clean up Table 2 in Excel
# Move rows of states into new columns
# Save as a CSV
dea.seizures <- read.csv("C:/Users/mdanna2/Downloads/nflis.csv",
                         stringsAsFactors = FALSE)

##### Project Ideas
# NY or CA + DEA: PCT of NY to other states, PCT of NY activity over time compared to Inmate year
# NY + Census for Demographics: State, All inmates, Gang inmates, non-Gang inmates
# NY + FBI

##### 1
# Get NY data
# Clean it
# Sum it
# Percentages and Sums for demographics for all, gang, non-gang
# join Census data for those demographics
# t tests for gang vs non-gang

# Get CA data
# Clean it
# Sum it
# join Census for county demographics
