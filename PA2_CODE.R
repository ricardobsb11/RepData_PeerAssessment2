install.packages(c("data.table","R.utils"))

library(R.utils)
library(data.table)

# Downloading and unpacking from the source URL.
if (!file.exists("repdata-data-StormData.csv.bz2")) 
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                      "repdata-data-StormData.csv.bz2", method = "curl")

if (!file.exists("repdata-data-StormData.csv"))
        bunzip2("repdata-data-StormData.csv.bz2", 
                destname = "repdata-data-StormData.csv", remove = F)

data <- read.csv("repdata-data-StormData.csv", stringsAsFactors = FALSE)
dim(data) # checking the dimensions of the data
summary(data)

# converting the format from the BGN_DATE variable to date
data$BGN_DATE <- as.Date(data$BGN_DATE,"%m/%d/%Y") 

# checking the EVTYPE  variable
data$EVTYPE <- toupper(data$EVTYPE) # capitalizing all the events type



