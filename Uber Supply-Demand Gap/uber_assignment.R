## Set working directory to the location as the file
## Read the file, 
uber_req <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)
str(uber_req)

## Loading all the required libraries.
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

## Cleaning and Preparing the data
## Changing few columns to factor variables

uber_req$Status <- as.factor(uber_req$Status)
uber_req$Pickup.point <- as.factor(uber_req$Pickup.point)

## formatting the Request.timestamp, Drop.timestamp variables
uber_req$Request.timestamp <- parse_date_time(uber_req$Request.timestamp, c("%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M"))
uber_req$Drop.timestamp <- parse_date_time(uber_req$Drop.timestamp, c("%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M"))

## Deriving new variables like Pickup Time Slot and Drop Time slot based on hours of day

your_breaks <- hour(hm("00:00", "4:59","7:59", "11:59", "17:59", "20:59", "23:59"))
Pickup.time <- cut(hour(uber_req$Request.timestamp), breaks = your_breaks, labels =  c("Night", "Early Morning","Morning", "Afternoon", "Evening", "Late Evening"), include.lowest = TRUE)

Drop.time <- cut(hour(uber_req$Drop.timestamp), breaks = your_breaks, labels =  c("Night", "Early Morning","Morning", "Afternoon", "Evening", "Late Evening"), include.lowest = TRUE)

## Adding that variable to data frame
uber_req <- cbind(uber_req, Pickup.time)
uber_req <- cbind(uber_req, Drop.time)

## View the dataframe
View(uber_req)

## Changing Pickup.time variable to factor
uber_req$Pickup.time <- as.factor(uber_req$Pickup.time)

## Analysis
## Lets do univariate analysis of Time Slots
## Plotting the number of requests against different time slots of requests using stacked bar graph
plot1 <- ggplot(uber_req, aes(x = Pickup.time)) + geom_bar(position = "stack", fill = "red") + ylab("Number of Requests") + ggtitle("Number of requests against Status of requests")
plot1

## By looking at the graph we can say that non availability of Cars is a major issue, although number of cancelled requests is also in thousands.

## Lets do bivariate analysis using status and time slot
## Plot 2
## Plotting the number of requests in each time slot and the status of the requests using stacked bar graph
plot2 <- ggplot(uber_req, aes(x = Pickup.time, fill = Status)) + geom_bar(position = "stack") + ggtitle(label = "Requests of Cabs in different Time Slots") + xlab("Time Slots") + ylab("Number of Requests")
plot2

## By looking at the graph, we can say that there are no cars available during evening slot and most of the requests get cancelled in early morning slot.
## Lets look at the numbers to verify the above conclusion

uber_demand_cancelled <- uber_req[uber_req$Status == "Cancelled", ]
request_grp <- group_by(uber_demand_cancelled, Pickup.time)
uber_demand_gap1 <- arrange(summarise(request_grp, count = n()), desc(count))
uber_demand_gap1

uber_demand_nocars <- uber_req[uber_req$Status == "No Cars Available", ]
request_grp2 <- group_by(uber_demand_nocars, Pickup.time)
uber_demand_gap2 <- arrange(summarise(request_grp2, count = n()), desc(count))
uber_demand_gap2

## The numbers also deduce the same as plot.

## Now let's drill down to pick-up location i.e., Airport or City for Early Morning and Evening Time slots
## Plotting a stacked bar graph for requests where no cars available or cars cancelled against specific time slots, along with pickup point. 
plot3 <- ggplot(uber_req[uber_req$Status != "Trip Completed" , ], aes(x = Pickup.time, fill = Status)) + geom_bar(position = "stack") + ggtitle(label = "Requests of Cabs not fulfilled in different slots and location") + ylab("No. of requestes Cancelled/ No cars available") + xlab("Time Slots") + facet_grid(.~Pickup.point)
plot3

## The plot shows that Most no. of requests cancelled in early morning slot for going to Airport and a high demand of cab requests in the evening slot for city
## Now let's verify the same using the table

uber_supply_demand1 <- uber_demand_cancelled[uber_demand_cancelled$Pickup.time == "Early Morning" | uber_demand_cancelled$Pickup.time == "Evening", ]
request_grp3 <- group_by(uber_supply_demand1, Pickup.time, Pickup.point)
uber_demand_gap3 <- arrange(summarise(request_grp3, count = n()), desc(count))
uber_demand_gap3

uber_supply_demand2 <- uber_demand_nocars[uber_demand_nocars$Pickup.time == "Early Morning" | uber_demand_nocars$Pickup.time == "Evening", ]
request_grp4 <- group_by(uber_supply_demand2, Pickup.time, Pickup.point)
uber_demand_gap4 <- arrange(summarise(request_grp4, count = n()), desc(count))
uber_demand_gap4

## We can conclude same as the above bar graph, that most number of cancellations are done in early morning from city and no cars are available in evening from airport.