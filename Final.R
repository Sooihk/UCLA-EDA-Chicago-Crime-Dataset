library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)
library(scales)
library(knitr)

# load data in R environment and look at data
crime_rawData <- read.csv("C:/Users/aduro/Desktop/Coding/UCLA/Exploratory Data/Crimes 2001toPresent.csv", header = TRUE)
head(crime_rawData)
str(crime_rawData)
# get column names of database
colnames(crime_rawData)

# obtain only relevant variables that pertain to answering the question
# narrow dataset to only include robberies
new_crimeData <- select(crime_rawData, Date, Year, Primary.Type, Location.Description,
                        District, Community.Area, Latitude, Longitude) %>% 
                filter(Primary.Type == "ROBBERY")

# Make transformations on Date variable in order for R to understand
new_crimeData <- mutate(new_crimeData, Date = mdy_hms(Date))
# Make new variable Hour and extract hour from Date
new_crimeData <- mutate(new_crimeData,Hour = substring(Date, 12, 13))
new_crimeData$Hour <- as.numeric(new_crimeData$Hour)
# Make new count variable, needed for aggregations
new_crimeData$Count <- 1
# Remove time from Data variable
new_crimeData$Date <- as.Date(new_crimeData$Date, format = "%m/%d/%Y")
# Create another variable named weekday to obtain day of the robbery
new_crimeData$weekday <- weekdays(new_crimeData$Date)
# remove 2021 year since it is not complete
new_crimeData <- new_crimeData[!new_crimeData$Year > 2020, ]
# remove 2001 year since so many variables have missing values
new_crimeData <- new_crimeData[!new_crimeData$Year < 2002, ]
# omit rows with missing data 
new_crimeData <- na.omit(new_crimeData)

# transform data location descriptions into categories more readable for charts
# First see all unique values in Location.Description column
unique(new_crimeData$Location.Description) # function used to find all unique locations robberies occured

new_crimeData$locationCat[new_crimeData$Location.Description %in% c('APARTMENT','CHA APARTMENT')] <- "APARTMENT"
new_crimeData$locationCat[new_crimeData$Location.Description %in% c('BAR OR TAVERN','ATHLETIC CLUB','TAVERN/LIQUOR STORE','TAVERN / LIQUOR STORE', 'HOTEL/MOTEL','HOTEL / MOTEL')] <- "BAR/HOTEL"
new_crimeData$locationCat[new_crimeData$Location.Description %in% c('ATM (AUTOMATIC TELLER MACHINE)', 'BRIDGE', 'SIDEWALK','STREET','ALLEY','HIGHWAY/EXPRESSWAY', 'LAKEFRONT/WATERFRONT/RIVERBANK', 'LAKEFRONT / WATERFRONT / RIVERBANK')] <- "STREET"
new_crimeData$locationCat[new_crimeData$Location.Description %in% c('CTA TRAIN', 'CTA BUS', 'VEHICLE NON-COMMERCIAL','OTHER COMMERCIAL TRANSPORTATION','VEHICLE-COMMERCIAL', 'TAXICAB', 'VEHICLE - OTHER RIDE SHARE SERVICE (E.G., UBER, LYFT)', 'VEHICLE - OTHER RIDE SHARE SERVICE (LYFT, UBER, ETC.)', 'VEHICLE - COMMERCIAL','VEHICLE - DELIVERY TRUCK','DELIVERY TRUCK','VEHICLE - OTHER RIDE SERVICE' )] <- "TRANSPORATION"
new_crimeData$locationCat[new_crimeData$Location.Description %in% c('BARBERSHOP', 'GAS STATION','DAY CARE CENTER','CAR WASH', 'SMALL RETAIL STORE','DRUG STORE', 'RESTAURANT','GROCERY FOOD STORE','CONVENIENCE STORE','DEPARTMENT STORE','PAWN SHOP','APPLIANCE STORE','CLEANING STORE', 'MEDICAL/DENTAL OFFICE', 'MEDICAL / DENTAL OFFICE','COMMERCIAL / BUSINESS OFFICE')] <- "SMALL BUSINESS"
new_crimeData$locationCat[new_crimeData$Location.Description %in% c('LIBRARY','CURRENCY EXCHANGE', 'BANK','CREDIT UNION',  'SPORTS ARENA/STADIUM', 'SPORTS ARENA / STADIUM', 'MOVIE HOUSE / THEATER', 'MOVIE HOUSE/THEATER',  'CTA PLATFORM','CTA STATION','CTA GARAGE / OTHER PROPERTY','CTA BUS STOP', 'GOVERNMENT BUILDING / PROPERTY','GOVERNMENT BUILDING/PROPERTY','OTHER RAILROAD PROP / TRAIN DEPOT', 'PARK PROPERTY','VACANT LOT/LAND','VACANT LOT / LAND','FACTORY/MANUFACTURING BUILDING', 'FACTORY / MANUFACTURING BUILDING','WAREHOUSE','CONSTRUCTION SITE','FOREST PRESERVE','CHURCH/SYNAGOGUE/PLACE OF WORSHIP','OTHER RAILROAD PROPERTY / TRAIN DEPOT', 'CTA TRACKS - RIGHT OF WAY','SAVINGS AND LOAN','POOL ROOM','AUTO / BOAT / RV DEALERSHIP','OTHER','CEMETARY','NEWSSTAND','COIN OPERATED MACHINE', 'BOAT / WATERCRAFT','OTHER (SPECIFY)', 'NURSING / RETIREMENT HOME','NURSING HOME/RETIREMENT HOME','FEDERAL BUILDING', 'CHA HALLWAY / STAIRWELL / ELEVATOR', 'CHA HALLWAY/STAIRWELL/ELEVATOR', 'CHA HALLWAY / STAIRWELL / ELEVATOR', 'HOSPITAL BUILDING/GROUNDS','ANIMAL HOSPITAL','HOSPITAL BUILDING / GROUNDS', 'AIRPORT/AIRCRAFT','AIRPORT TERMINAL LOWER LEVEL - NON-SECURE AREA','AIRPORT TERMINAL UPPER LEVEL - NON-SECURE AREA','AIRPORT EXTERIOR - NON-SECURE AREA','AIRPORT BUILDING NON-TERMINAL - NON-SECURE AREA','AIRPORT PARKING LOT','AIRPORT VENDING ESTABLISHMENT','AIRPORT TRANSPORTATION SYSTEM (ATS)')] <- "OTHERS"
new_crimeData$locationCat[new_crimeData$Location.Description %in% c('RESIDENCE', 'DRIVEWAY - RESIDENTIAL', 'RESIDENCE - PORCH / HALLWAY','RESIDENCE PORCH/HALLWAY', 'RESIDENCE - YARD (FRONT / BACK)', 'RESIDENCE - GARAGE','RESIDENTIAL YARD (FRONT/BACK)','RESIDENCE-GARAGE','ABANDONED BUILDING')] <- "RESIDENTIAL AREAS"
new_crimeData$locationCat[new_crimeData$Location.Description %in% c('POLICE FACILITY/VEH PARKING LOT','CHA PARKING LOT/GROUNDS','PARKING LOT/GARAGE(NON.RESID.)','CHA PARKING LOT / GROUNDS', 'PARKING LOT / GARAGE (NON RESIDENTIAL)',  'CTA PARKING LOT / GARAGE / OTHER PROPERTY')] <- "PARKING LOTS"
new_crimeData$locationCat[new_crimeData$Location.Description %in% c('SCHOOL, PUBLIC, GROUNDS', 'SCHOOL - PUBLIC GROUNDS', 'SCHOOL - PRIVATE GROUNDS', 'SCHOOL - PUBLIC BUILDING','COLLEGE/UNIVERSITY GROUNDS','SCHOOL, PUBLIC, BUILDING','SCHOOL, PRIVATE, GROUNDS','COLLEGE/UNIVERSITY RESIDENCE HALL','SCHOOL, PRIVATE, BUILDING')] <- "SCHOOLS"


# show new crime data set to be used
str(new_crimeData)


# get daily robberies arranged by Date
# 2001 data points contain missing values in community area, latitude and longitude
# Plot shows number of daily crimes from 2001 to 2021 has been decreasing yearly.Plot shows a periodic trend that most
# robberies tend to occur during minew_crimeDatale of the year, and every new year robberies are at its peak.
daily_robbery <- crime_rawData %>% filter(Primary.Type == "ROBBERY") %>% mutate(Date = as.Date(Date, "%m/%d/%Y")) %>% group_by(Date) %>% 
                 summarize(count = n()) %>% arrange(Date)
plot1 <- ggplot(daily_robbery, aes(Date, count)) + geom_line(colour = "steelblue") + geom_smooth(size = 0.5, colour = "black") + 
         theme() + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
         labs(y = "Number of Robberies", title = "Daily Robberies in Chicago from 2001 - 2021") +
         theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1), axis.text.y = element_text(face = "bold")) +
         theme(axis.title.x=element_blank())
plot1
# show trend of robberies from 2001-2020, need to aggregate data on another object
aggregated_robberies <- aggregate(Count ~ Year, data = new_crimeData, FUN = sum)
plot2 <- ggplot(aggregated_robberies, aes(Year, Count)) + geom_line(colour = "blue", size = 0.7) + geom_point(colour = "black") + theme() + 
  labs(x = "Date of Robbery", y = "Number of Robberies", title = "Rolling Sum of Robberies in Chicago from 2002 - 2020") + geom_text(aes(label = Year)) +
  theme(axis.title.x=element_text(face = "bold"))
# This plot shows sum of robberies per year. The year 2002 like 2001, has many missing values due to technology and less meticulous
# record keeping. There was a slight rise in robberies in the following years: 2008-2009 and 2016-2017, but overall the trend
# is decreasing. 


# Plot3 which locations in the city are robberies occruing the most
aggregated_locations <- aggregate(Count ~ locationCat, data = new_crimeData, FUN = sum)
# order the values of each location from highest to lowest
aggregated_locations$locationCat <- factor(aggregated_locations$locationCat, levels = aggregated_locations$locationCat[order(aggregated_locations$Count, decreasing = TRUE)])

plot3 <- ggplot(aggregated_locations, aes(locationCat, Count)) + theme_minimal() + geom_bar(stat = "identity", fill = "steelblue") +
         labs(y = "Number of Robberies", title = "Number of Robberies per Location") + 
         theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1), axis.text.y = element_text(face = "bold")) + theme(axis.title.x=element_blank(), axis.title.y = element_blank()) +
         geom_text(aes(label=Count), vjust=-0.1, size=3.5)
plot3

# Which community areas have the most robberies
# First make Community.Area column a factor variable
new_crimeData$Community.Area <- as.factor(new_crimeData$Community.Area)
# create aggregated dataset to hold sum of robberies for each area
aggregated_community <- aggregate(Count ~ Community.Area, data = new_crimeData, FUN = sum)
# order the values of areas from highest to lowest of the top 20 
aggregated_communityTopTen <- arrange(aggregated_community, desc(Count)) %>% slice(1:20) 

# Create plot showing community area of Chicago vs Number of Robberies
# order the plot by highest to lowest with reorder function
plot4 <- ggplot(aggregated_communityTopTen, aes(x = reorder(Community.Area, Count), Count, fill = Community.Area)) + geom_bar(stat = "identity") + coord_flip() + theme_minimal() +
         theme(legend.position = "none") + labs(x = "Community Areas",y = "Number of Robberies", title = "Top 20 Community Areas with Robberies") + geom_text(aes(label = Count), hjust = 1.1) +
         scale_y_continuous(expand = c(0,0))
plot4


# What hour of the day do most robberies occur?
plot5 <- ggplot(new_crimeData, aes(Hour)) + geom_bar(stat = "Count", fill = "steelblue", width = 0.9)+theme_minimal() + labs(x = "Hour of day", y = "# of Robberies", title = "Robberies per hour from 2002-2020") + 
  scale_x_continuous(expand = c(0,0),breaks = seq(0,23,1)) + scale_y_continuous(expand = c(0,0))
plot5

# Create heat map, first make aggregated dataset 
aggregated_time <- aggregate(Count ~ Hour + weekday, data = new_crimeData, FUN = sum)
# Create objects that will format heat map axis
week_format <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
hours_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))
# Then factorize the hour and weekday columns and relabel hour's values with AM or PM
aggregated_time$Hour <- factor(aggregated_time$Hour, levels = 0:23, labels = hours_format)
aggregated_time$weekday <- factor(aggregated_time$weekday, levels = rev(week_format))

plot6 <- ggplot(aggregated_time, aes(Hour, weekday, fill = Count)) + geom_tile() + theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.direction = "horizontal", legend.position = "top", legend.title = element_blank(), legend.key.height = unit(0.25, "cm"),legend.key.width = unit(2,"cm"), legend.margin = unit(-.5, "cm")) + 
  labs(x = "Hour of Robbery", y = "Day of the Week", title = "Robberies in Chicago from 2002-2020 shown by time") +
  scale_fill_gradientn(colours = colorspace::diverge_hcl(7))
plot6  
  
