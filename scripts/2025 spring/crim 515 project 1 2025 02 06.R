# mac
hate.crime <- read.csv("/Users/YOUR USER NAME/Downloads/hate_crime/hate_crime.csv",
                       stringsAsFactors = FALSE)

# windows
hate.crime <- read.csv("C:/Users/mdanna2/Downloads/hate_crime.csv",
                       stringsAsFactors = FALSE)

colnames(hate.crime)
unique(hate.crime$agency_type_name)
unique(hate.crime$location_name)
