install.packages(c("data.table","R.utils","dplyr"))
install.packages("ggplot2")
library(R.utils)
library(data.table)
library(dplyr)
library(ggplot2)
library(plyr)

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

# selecting some variables from original dataset
data_clean <- subset(data, select = c("STATE__","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP"))

# changing the variables names for a better comprehension
names(data_clean) <- c("State", "Event", "Fatalities", "Injuries", "Property.Damage", "Property.Damage.Exp", "Crop.Damage", "Crop.Damage.Exp")
data_clean$Event <- toupper(data_clean$Event) # capitalizing all the events type
head(data_clean)

# exploring the Event variable
detach("package:plyr", unload=TRUE) 
event.summary = data_clean %>% 
        group_by(Event) %>%
        summarize(total=n()) %>% 
        arrange(desc(total))

# reducing the sub-categories caused by a lot of words with uppercase and lowercase 
data_clean$Event <- gsub("^(HEAT).*", "HEAT", data_clean$Event)
data_clean$Event <- gsub("^(RECORD HEAT).*", "HEAT", data_clean$Event)
data_clean$Event <- gsub("^(EXTREME HEAT).*", "HEAT", data_clean$Event)
data_clean$Event <- gsub("^(EXCESSIVE HEAT).*", "HEAT", data_clean$Event)
data_clean$Event <- gsub("^(TSTM).*", "THUNDER STORM", data_clean$Event)
data_clean$Event <- gsub("^(THUNDERSTORM).*", "THUNDER STORM", data_clean$Event)
data_clean$Event <- gsub("^(TROPICAL STORM).*", "TROPICAL STORM", data_clean$Event)
data_clean$Event <- gsub("^(FLASH FLOOD).*", "FLOOD", data_clean$Event)
data_clean$Event <- gsub("^(WIND).*", "WIND", data_clean$Event)
data_clean$Event <- gsub("^(STRONG WIND).*", "WIND", data_clean$Event)
data_clean$Event <- gsub("^(HIGH WIND).*", "WIND", data_clean$Event)
data_clean$Event <- gsub("^(HURRICANE).*", "HURICCANE", data_clean$Event)
data_clean$Event <- gsub("^(SNOW).*", "SNOW", data_clean$Event)
data_clean$Event <- gsub("^(HEAVY SNOW).*", "SNOW", data_clean$Event)
data_clean$Event <- gsub("^(FIRE).*", "FIRE", data_clean$Event)
data_clean$Event <- gsub("^(WILD/FOREST FIRE).*", "FIRE", data_clean$Event)
data_clean$Event <- gsub("^(WILDFIRE).*", "FIRE", data_clean$Event)
data_clean$Event <- gsub("^(WILD FIRES).*", "FIRE", data_clean$Event)
data_clean$Event <- gsub("^(HAIL).*", "HAIL", data_clean$Event)
data_clean$Event <- gsub("^(BLIZZARD).*", "BLIZZARD", data_clean$Event)
data_clean$Event <- gsub("^(COLD).*", "COLD", data_clean$Event)
data_clean$Event <- gsub("^(WINTER WEATHER).*", "COLD", data_clean$Event)
data_clean$Event <- gsub("^(EXTREME COLD).*", "COLD", data_clean$Event)
data_clean$Event <- gsub("^(RIP).*", "RIP", data_clean$Event)
data_clean$Event <- gsub("^(FOG).*", "FOG", data_clean$Event)
data_clean$Event <- gsub("^(DENSE FOG).*", "FOG", data_clean$Event)
data_clean$Event <- gsub("^(AVALANCHE).*", "AVALANCHE", data_clean$Event)
data_clean$Event <- gsub("^(AVALANCE).*", "AVALANCHE", data_clean$Event)
data_clean$Event <- gsub("^(RAIN).*", "RAIN", data_clean$Event)
data_clean$Event <- gsub("^(HEAVY RAIN).*", "RAIN", data_clean$Event)
data_clean$Event <- gsub("^(HIGH SURF).*", "SURF", data_clean$Event)
data_clean$Event <- gsub("^(HEAVY SURF).*", "SURF", data_clean$Event)
data_clean$Event <- gsub("^(SURF).*", "SURF", data_clean$Event)

# after cleaning the data, we´re creating a new object
event.summary.clean = data_clean %>% 
        group_by(Event) %>%
        summarize(total=n()) %>% 
        arrange(desc(total))

event.summary.clean

# The variables PROPDMGEXP and CROPDMGEXP have the factor of multiplicity of the variables PROPDMG and CROPDMG:

# checking the unique values of Property.Damage.Exp variable
unique(data_clean$Property.Damage.Exp)

# checking the unique values of Crop.Damage.Exp variable
unique(data_clean$Crop.Damage.Exp)

# creating three new variables called Property.Damage.Total and Crop.Damage.Total and Total.Damage
data_clean$Property.Damage.Total <- as.numeric(mapvalues(data_clean$Property.Damage.Exp,
                                                         c("K","M","", "B","m","+","0","5","6","?","4","2","3","h","7","H","-","1","8"), 
                                                         c(10^3,10^6, 1, 10^9,10^6,  1,  1,10^5,10^6,  1,10^4,10^2,10^3,  1,10^7,10^2,  1, 10,10^8))) * data_clean$Property.Damage

data_clean$Crop.Damage.Total <- as.numeric(mapvalues(data_clean$Crop.Damage.Exp,
                                                     c("","M","K","m","B","?","0","k","2"),
                                                     c( 1,10^6,10^3,10^6,10^9,1,1,10^3,10^2))) * data_clean$Crop.Damage

data_clean$Total.Damage <- data_clean$Property.Damage.Total + data_clean$Crop.Damage.Total


#summarizing the data by event type
event_total <- ddply(data_clean,.(Event), summarize,Property = sum(Property.Damage.Total),
                     Crop = sum(Crop.Damage.Total),PropertyDamage = sum(Total.Damage), Injuries= sum(Injuries), Fatalities = sum(Fatalities), PersonalDamage = sum(Injuries)+sum(Fatalities))

View(head(event_total))

# ordering by property damage
event_total_ordered1 <- event_total[order(event_total$PropertyDamage, decreasing = TRUE),]

# checking the top 10 property damage by event and plotting it 
head(event_total_ordered1,10)
graf_totalpropertydamage <- ggplot(head(event_total_ordered1,10), aes(x = Event, y = PropertyDamage)) + 
        geom_bar(stat = "identity",color = "red",fill="darkgreen") +
        ggtitle("Total Property Damage by events in U.S ($) ")
graf_totalpropertydamage

# plot of top 10 property damage
graf_propdamage <- ggplot(head(event_total[order(event_total$Property, decreasing = TRUE),],10), aes(x = Event, y = Property)) + 
        geom_bar(stat = "identity",color = "red",fill="blue") +
        ggtitle("Property damage by events in U.S ($)")
graf_propdamage
# plot of top 10 crop damage
graf_cropdamage <- ggplot(head(event_total[order(event_total$Crop, decreasing = TRUE),],10), aes(x = Event, y = Crop)) + 
        geom_bar(stat = "identity",color = "red",fill="green") +
        ggtitle("Crop damage by events in U.S ($)")
graf_cropdamage

# ordering by personal damage
event_total_ordered2 <- event_total[order(event_total$PersonalDamage, decreasing = TRUE),]

# checking the top 10 personal damage by event and plotting it
head(event_total_ordered2,10)
graf_totalpersonaldamage <- ggplot(head(event_total_ordered2,10), aes(x = Event, y = PersonalDamage)) + 
        geom_bar(stat = "identity",color = "red",fill="grey") +
        ggtitle("Total Personal Damage by events in U.S")
graf_totalpersonaldamage
# plot of injuries by event (top 10)
graf_injuries <- ggplot(head(event_total[order(event_total$Injuries, decreasing = TRUE),],10), aes(x = Event, y = Injuries)) + 
        geom_bar(stat = "identity",color = "red",fill="pink") +
        ggtitle("Total Injuries by events in U.S")
graf_injuries
# plot of fatalities by event
graf_fatalities <- ggplot(head(event_total[order(event_total$Fatalities, decreasing = TRUE),],10), aes(x = Event, y = Fatalities)) + 
        geom_bar(stat = "identity",color = "black",fill="red") +
        ggtitle("Total Fatalities by events in U.S")
graf_fatalities