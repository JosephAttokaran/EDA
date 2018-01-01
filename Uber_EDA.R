# UBER CASE STUDY - GRADED ASSIGNMENT
library(ggplot2)
library(stringr)
library(lubridate)
library(dplyr)

# Importing the "Uber Request Data" csv file
RequestData <- read.csv("Uber Request Data.csv")
RequestData_df <- RequestData

# To view the structure and summary of the csv file
str(RequestData_df)  # Date/Time has been considered as Factors 
summary(RequestData_df)

#CROSS CHECKING DATA CLEANING CHECKLIST
#Is there any summary, incorrect, extra rows? Answer : None
#Is there a need to merge/split/add(if not present)/rename/delete/align columns? Answer : None

# Analyse and handle Missing Values(if required)
length(which(is.na(RequestData_df))) # Total 6564 NA values
sapply(RequestData_df, function(x) length(which(is.na(x)))) # 2650 NAs in Driver.id/3914 NAs in Drop.timestamp
sapply(RequestData_df, function(x) length(which(x == ""))) # no blank values present

DriverId_NA <- RequestData_df[is.na(RequestData_df$Driver.id),]
DropTimestamp_NA <- RequestData_df[is.na(RequestData_df$Drop.timestamp),]

#Justification for NA values : 
# 1) Driver.id is NA only when Status is "No cars Available"
# 2) Drop.timestamp is NA only when the Status is "No Cars Available" or "Cancelled"
# 3) These NAs are part of the data and can be left as it is

#STANDARDISING VALUES
# Cleaning the date values for further analysis
#Date/Time Issues
# 1) Two different date formats dd/mm/yyyy and dd-mm-yyyy
# 2) In the timestamp seconds are missing whenever the seconds is "00"

RequestData_df$RequestTimestamp_Cleaned <- parse_date_time(RequestData_df$Request.timestamp, c("dmY_HMS","dmY_HM"))
RequestData_df$DropTimestamp_Cleaned <- parse_date_time(RequestData_df$Drop.timestamp, c("dmY_HMS","dmY_HM"))
str(RequestData_df)

# Two Columns were created in addition to the existing Request and Drop Timpstamps so 
# that the impact after cleansing can be seen side by side.

#CREATING NEW DATE AND TIME FIELDS(TYPE DRIVEN METRICS)

#Function to classify the time into segments based on the request timestamp
#to analyse the cab usage hours
TimeInterval <- function(x){
  if(x >= "00:00:00" & x <= "02:59:59" & complete.cases(x)) {return("12 AM - 03 AM")}
  else if(x >= "03:00:00" & x <= "05:59:59" & complete.cases(x)) {return("03 AM - 06 AM")}
  else if(x >= "06:00:00" & x <= "08:59:59" & complete.cases(x)) {return("06 AM - 09 AM")}
  else if(x >= "09:00:00" & x <= "11:59:59" & complete.cases(x)) {return("09 AM - 12 PM")}
  else if(x >= "12:00:00" & x <= "14:59:59" & complete.cases(x)) {return("12 PM - 03 PM")}
  else if(x >= "15:00:00" & x <= "17:59:59" & complete.cases(x)) {return("03 PM - 06 PM")}
  else if(x >= "18:00:00" & x <= "20:59:59" & complete.cases(x)) {return("06 PM - 09 PM")}
  else if(x >= "21:00:00" & x <= "23:59:59" & complete.cases(x)) {return("09 PM - 12 AM")}
  return(NA)
  }

# Deriving new variables from RequestTimeStamp
RequestData_df$Req_Date <- format(RequestData_df$RequestTimestamp_Cleaned, "%d")
RequestData_df$Req_Time <- format(RequestData_df$RequestTimestamp_Cleaned, "%H:%M:%S")
RequestData_df$Req_Hour <- format(RequestData_df$RequestTimestamp_Cleaned, "%H")
RequestData_df$Req_Day <- weekdays(RequestData_df$RequestTimestamp_Cleaned)

# Classifing Request hours into Hour bins for analysis based on Time Slot
RequestData_df$Req_TimeInterval <- factor(sapply(RequestData_df$Req_Time, TimeInterval)) 

#INVALID VALUES
# 1) The New Variables(Req_Data, Req_Time, Req_Hour) were not converted into Date and time formats
#as it was not necessary for the analysis at hand.
# 2) All other values looks correct

#FILTERING DATA
#Checking for Duplicate rows
sum(duplicated(RequestData_df$Request.id)) # No duplicate rows found

#Filtering columns relavant for analysis

RequestData_df <- RequestData_df[,c(-5,-6)] # removed the duplicate Request/Drop columns

#UNIVARIATE ANALYSIS
#Unordered Categorical Variable : Req_TimeInterval
#There are no ordered variable for analysis
TimeInterval_Requests <- aggregate(RequestData_df, list(Req_Interval = RequestData_df$Req_TimeInterval), length)
TimeInterval_Requests <- arrange(TimeInterval_Requests[,c(1,12)], desc(Req_TimeInterval))
TimeInterval_Requests$Rank <- rank(-TimeInterval_Requests$Req_TimeInterval)
#Plotting a Rank Frequency graph
# 1) A power law distribution can be observed from which we can infer that the Time slot of "06 PM - 09PM"
# has received the maximum(1475) cab requests followed by "06AM - 09AM". The time slot "12AM - 03AM" has the
# least number of cab requests
Rank_freq_plot <- ggplot(TimeInterval_Requests, aes(x = Rank, y = Req_TimeInterval)) + geom_line() 


#Quantitative Variable : 
#In the raw Data there are no quantitative vairables. 
#All Derived data are aggergated values added as separate columns for convenience

#SEGMENTED UNIVARIATE ANALYSIS
#Here "Status" and "Req_Hour" is considered as the Basis of Segmentation.
Request_Status <- aggregate(RequestData_df, list(Status = RequestData_df$Status), length)
Request_Status <- Request_Status[,c(1,2)]
colnames(Request_Status)[2] <- "Number of Requests"
Success_Ratio <- Request_Status[Request_Status$Status == "Trip Completed",2] / sum(Request_Status$`Number of Requests`) * 100

#Plotting a bar chart to show the number of Requests in each segment of "Status"
# 1) It can be observed here that there are more Cab booking failed than Completed Successfully
# 2) The Success percentage is a mere 42% which means only 42% of the demand could be handled.
# 3) The failed ones were either due to unavailability or cancellation
Cab_Request_Plot <- ggplot(RequestData_df, aes(x = Status)) + geom_bar()

#Plotting a bar chart to show the number requests during each hour of the day.
# 1) It can be that there is a surge in Requests between 5AM and 10AM but most failed(mostly cancelled)
# 2) Similarly between 5PM and 9PM with a minimum of 400 requests coming each hour when a
#    total is taken across the week. But most failed due to unavailability
Cab_Request_per_hour <- ggplot(RequestData_df, aes(x = Req_Hour, fill = Status)) + geom_bar()

#BIVARIATE ANALYSIS:
#Since there are no quantitative variales, correlation analysis is not done
#Bivariate analysis is done on the Status, Pickup point and other time variables
#Aggregating the Data to see the number of trips

#Function to return an aggregated table after grouping with one variable
aggregateby1 <- function(Status, aggregateby, ColName){
  Temp_grouped <- group_by(RequestData_df[RequestData_df$Status == Status,], !!aggregateby)
  Temp_grouped <- summarise(Temp_grouped, length(!!aggregateby))
  colnames(Temp_grouped)[2] <- ColName
  return(Temp_grouped)
}

#Number of trips under each status per Time_Interval: Bivariate(Status/Req_TimeInterval)
RequestData_df<- merge(RequestData_df, aggregateby1("Trip Completed", quo(Req_TimeInterval), "Trips_Completed per Interval"), by = "Req_TimeInterval", all = TRUE)
RequestData_df<- merge(RequestData_df, aggregateby1("Cancelled", quo(Req_TimeInterval), "Trips_Cancelled per Interval"), by = "Req_TimeInterval", all = TRUE)
RequestData_df<- merge(RequestData_df, aggregateby1("No Cars Available", quo(Req_TimeInterval), "Failed_Cab_Requests per Interval"), by = "Req_TimeInterval", all = TRUE)

#Number of requests in each status in all Time_Intervals
Agg_TimeInterval <- RequestData_df[-which(duplicated(RequestData_df$Req_TimeInterval)), c("Req_TimeInterval","Trips_Completed per Interval","Trips_Cancelled per Interval","Failed_Cab_Requests per Interval")]

#Plot to show barchart with all the requests in each status during 8 time intervals
#1) It can be seen that there is significant spike in the number of cab requests
#   failed due unavailability of cars during the slot 06PM - 09PM
#2) Significant cancellations occuring during the slot 06AM - 09AM
Interva_group <- group_by(RequestData_df, Status, Req_TimeInterval) %>% summarise(count = n())

Cab_Status_per_TimeInterval <- ggplot(Interva_group) + 
                                geom_bar(aes(x=Status,y=count),stat="identity")  + 
                                facet_wrap(~Req_TimeInterval,nrow=2) + 
                                geom_text(aes(x=Status,y=count, label = count),nudge_y = 100) + 
                                theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Number of trips under each status per Hour : Bivariate(Status/Req_Hour)
RequestData_df<- merge(RequestData_df, aggregateby1("Trip Completed", quo(Req_Hour), "Trips_Completed per Req_Hour"), by = "Req_Hour", all = TRUE)
RequestData_df<- merge(RequestData_df, aggregateby1("Cancelled", quo(Req_Hour), "Trips_Cancelled per Req_Hour"), by = "Req_Hour", all = TRUE)
RequestData_df<- merge(RequestData_df, aggregateby1("No Cars Available", quo(Req_Hour), "Failed_Cab_Requests per Req_Hour"), by = "Req_Hour", all = TRUE)

#Number of requests in each status during every hour
Agg_Hour <- RequestData_df[-which(duplicated(RequestData_df$Req_Hour)), c("Req_Hour","Trips_Completed per Req_Hour","Trips_Cancelled per Req_Hour","Failed_Cab_Requests per Req_Hour")]

#Plot to show barchart with all the requests in each status during every hour
Hour_group <- group_by(RequestData_df, Status, Req_Hour) %>% summarise(count = n())

Cab_Status_per_hour <- ggplot(Hour_group) + 
                        geom_bar(aes(x=Status,y=count),stat="identity")  + 
                        facet_wrap(~Req_Hour,nrow=3) + 
                        geom_text(aes(x=Status,y=count, label = count),nudge_y = 50) + 
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Number of trips under each status per Day : Bivariate(Status/Req_Day)
RequestData_df<- merge(RequestData_df, aggregateby1("Trip Completed", quo(Req_Day), "Trips_Completed per Req_Day"), by = "Req_Day", all = TRUE)
RequestData_df<- merge(RequestData_df, aggregateby1("Cancelled", quo(Req_Day), "Trips_Cancelled per Req_Day"), by = "Req_Day", all = TRUE)
RequestData_df<- merge(RequestData_df, aggregateby1("No Cars Available", quo(Req_Day), "Failed_Cab_Requests per Req_Day"), by = "Req_Day", all = TRUE)

#Number of requests in each status during every day
Agg_Day <- RequestData_df[-which(duplicated(RequestData_df$Req_Day)), c("Req_Day","Trips_Completed per Req_Day","Trips_Cancelled per Req_Day","Failed_Cab_Requests per Req_Day")]

#Plot to show barchart with all the requests in each status during each day
# No particular trend can be observed here!
Day_group <- group_by(RequestData_df, Status, Req_Day) %>% summarise(count = n())

Cab_Status_per_Day <- ggplot(Day_group) + 
                       geom_bar(aes(x=Status,y=count),stat="identity")  + 
                       facet_wrap(~Req_Day,nrow=1) + 
                       geom_text(aes(x=Status,y=count, label = count),nudge_y = 50) + 
                       theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Number of trips under each status per Pickup point : Bivariate(Status/Pickup.point)
RequestData_df<- merge(RequestData_df, aggregateby1("Trip Completed", quo(Pickup.point), "Trips_Completed from Pickup Point"), by = "Pickup.point", all = TRUE)
RequestData_df<- merge(RequestData_df, aggregateby1("Cancelled", quo(Pickup.point), "Trips_Cancelled from Pickup Point"), by = "Pickup.point", all = TRUE)
RequestData_df<- merge(RequestData_df, aggregateby1("No Cars Available", quo(Pickup.point), "Failed_Cab_Requests at Pickup Point"), by = "Pickup.point", all = TRUE)

#Number of requests in each status at each pickup point
Agg_Pickup_point <- RequestData_df[-which(duplicated(RequestData_df$Pickup.point)), c("Pickup.point","Trips_Completed from Pickup Point","Trips_Cancelled from Pickup Point","Failed_Cab_Requests at Pickup Point")]

#Plot to show barchart with all the requests in each status during each day
# 1) There is a huge number of cancellations for the "City to Airport" trips compared to "Airport to City"
# 2) Similarly there are more number of failed Requests at Airport due to Unavailability
Pickp_point_group <- group_by(RequestData_df, Status, Pickup.point) %>% summarise(count = n())

Cab_Status_per_PickupPoint <- ggplot(Pickp_point_group) + 
                               geom_bar(aes(x=Status,y=count),stat="identity")  + 
                               facet_wrap(~Pickup.point,nrow=1) + 
                               geom_text(aes(x=Status,y=count, label = count),nudge_y = 50)


#TRIVARIATE ANALYSIS : (Status/Pickup.point/Req_TimeInterval)
#Function to return an aggregated table after grouping with two variable
aggregateby2 <- function(Status, ColName){
  Temp_grouped <- group_by(RequestData_df[RequestData_df$Status == Status,], Pickup.point, Req_TimeInterval)
  Temp_grouped <- summarise(Temp_grouped, length(Pickup.point))
  colnames(Temp_grouped)[3] <- ColName
  return(Temp_grouped)
}

#Number of trips under each status per Pickup point within the Time Interval
RequestData_df<- merge(RequestData_df, aggregateby2("Trip Completed","Trips_Completed from Pickup Point in Time_Interval"), by = c("Pickup.point","Req_TimeInterval"), all = TRUE)
RequestData_df<- merge(RequestData_df, aggregateby2("Cancelled", "Trips_Cancelled from Pickup Point in Time_Interval"), by = c("Pickup.point","Req_TimeInterval"), all = TRUE)
RequestData_df<- merge(RequestData_df, aggregateby2("No Cars Available", "Failed_Cab_Requests at Pickup Point in Time_Interval"), by = c("Pickup.point","Req_TimeInterval"), all = TRUE)

#Number of requests in each status during each of the TimeInterval from all Pickup points
Agg_TimeInterval_Point <- RequestData_df[-which(duplicated(RequestData_df[c("Req_TimeInterval","Pickup.point")])), c("Req_TimeInterval","Pickup.point","Trips_Completed from Pickup Point in Time_Interval","Trips_Cancelled from Pickup Point in Time_Interval","Failed_Cab_Requests at Pickup Point in Time_Interval")]

#Plot to show barchart with all the requests in each status during each day
#1) There is large number of Cancellations in the slot 06AM - 09AM for "City to Airport" trips
#2) Huge demand for cabs in the slot "06PM - 09PM" from Airport, but significant shortage in supply
TimeInterval_point_group <- group_by(RequestData_df, Req_TimeInterval, Status, Pickup.point) %>% summarise(count = n())

Cab_Status_PickupPoint_TimeInterval <- ggplot(TimeInterval_point_group) + 
                                        geom_bar(aes(x=Status,y=count, fill = Pickup.point),stat="identity")  + 
                                        facet_wrap(~Req_TimeInterval,nrow=2) + 
                                        theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Calculating the Cancellation Ratio/ Non - Availability Ratio per hour and Time Interval
#Adding the results into the master dataframe.
RequestData_df <- mutate(RequestData_df, `Cancellation_Ratio Per Hour` = round(RequestData_df$`Trips_Cancelled per Req_Hour` / (RequestData_df$`Trips_Completed per Req_Hour` + RequestData_df$`Trips_Cancelled per Req_Hour`),2),  #Cancelled per hour / Total trips per hour
                         `Non_Availability_Ratio Per Hour` = round(RequestData_df$`Failed_Cab_Requests per Req_Hour` / (RequestData_df$`Trips_Completed per Req_Hour` + RequestData_df$`Trips_Cancelled per Req_Hour` + RequestData_df$`Failed_Cab_Requests per Req_Hour`),2), # non available response per hour/ total responses per hour
                         `Cancellation_Ratio Per Time_Interval` = round(RequestData_df$`Trips_Cancelled per Interval` / (RequestData_df$`Trips_Completed per Interval` + RequestData_df$`Trips_Cancelled per Interval`),2), #Cancelled per Interval/ Total trips per Interval
                         `Non_Availability_Ratio Per Time_Interval` = round(RequestData_df$`Failed_Cab_Requests per Interval` / (RequestData_df$`Trips_Completed per Interval` + RequestData_df$`Trips_Cancelled per Interval` + RequestData_df$`Failed_Cab_Requests per Interval`),2), # non available response per Interval/ total responses per Interval
                         `Cancellation_Ratio Per Day` = round(RequestData_df$`Trips_Cancelled per Req_Day` / (RequestData_df$`Trips_Completed per Req_Day` + RequestData_df$`Trips_Cancelled per Req_Day`),2), #Cancelled per Day/ Total trips per Day
                         `Non_Availability_Ratio Per Day` = round(RequestData_df$`Failed_Cab_Requests per Req_Day` / (RequestData_df$`Trips_Completed per Req_Day` + RequestData_df$`Trips_Cancelled per Req_Day` + RequestData_df$`Failed_Cab_Requests per Req_Day`),2), # non available response per Day/ total responses per Day
                         `Cancellation_Ratio Per Time_Interval in Pickup Point` = round(RequestData_df$`Trips_Cancelled from Pickup Point in Time_Interval` / (RequestData_df$`Trips_Completed from Pickup Point in Time_Interval` + RequestData_df$`Trips_Cancelled from Pickup Point in Time_Interval`),2), #Cancelled per Interval at Pickup Point/ Total trips per Interval in Pickup Point
                         `Non_Availability_Ratio Per Time_Interval in Pickup Point` = round(RequestData_df$`Failed_Cab_Requests at Pickup Point in Time_Interval` / (RequestData_df$`Trips_Completed from Pickup Point in Time_Interval` + RequestData_df$`Trips_Cancelled from Pickup Point in Time_Interval` + RequestData_df$`Failed_Cab_Requests at Pickup Point in Time_Interval`),2)) # non available response per Interval in Pickup Point/ total responses per Interval in Pickup Point

RequestData_df <- RequestData_df[c(5,6,1,2,7,4,3,8:34)]


