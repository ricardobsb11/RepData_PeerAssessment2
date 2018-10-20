install.packages(c("data.table","R.utils","dplyr"))

library(R.utils)
library(data.table)
library(dplyr)

# Downloading and unpacking from the source URL.
if (!file.exists("repdata-data-StormData.csv.bz2")) 
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                      "repdata-data-StormData.csv.bz2", method = "curl")

if (!file.exists("repdata-data-StormData.csv"))
        bunzip2("repdata-data-StormData.csv.bz2", 
                destname = "repdata-data-StormData.csv", remove = F)

data <- read.csv("repdata-data-StormData.csv", stringsAsFactors = FALSE,na.strings="NA")
dim(data) # checking the dimensions of the data
summary(data)
colnames(data)

head(data)
# converting the format from the BGN_DATE variable to date
data$BGN_DATE <- as.Date(data$BGN_DATE,"%m/%d/%Y") 

# checking the EVTYPE  variable
data$EVTYPE <- toupper(data$EVTYPE) # capitalizing all the events type

# selecting some variables from original dataset
data_clean <- subset(data, select = c("STATE__","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP"))
head(data_clean)

# changing the variables names for a better comprehension
names(data_clean) <- c("State", "Event", "Fatalities", "Injuries", "Property.Damage", "Property.Damage.Exp", "Crop.Damage", "Crop.Damage.Exp")

# exploring the Event variable

data_clean %>% 
        group_by(Event) %>%
        summarize(total=n()) %>% 
        arrange(Event)
