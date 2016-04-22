main <- function(){

  getAvgDailyAvailabilityPlot(park_data)
  #assign("park_data", park_data, envir = .GlobalEnv) # assign data to global variable
  
  ## Other Plots
  #getAvgDailyAvailabilityHist(park_data) # histogram
  getAvgPerWeekdayPlot(park_data)
  
}

install_libraries <- function(){
  # if(!require(ggplot2)){
  #   install.packages("ggplot2", dependencies = TRUE, repos='http://cran.us.r-project.org') 
  # }
  # if(!require(plyr)){
  #   install.packages("plyr", dependencies = TRUE, repos='http://cran.us.r-project.org') 
  # }
  # if(!require(lubridate)){
  #   install.packages("lubridate", dependencies = TRUE, repos='http://cran.us.r-project.org') 
  # }
  list.of.packages <- c("ggplot2", "plyr", "lubridate")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  library(ggplot2)
  library(plyr)
  library(lubridate)
}

loadData <- function(){
  # Load Parking Data
  if(!exists("park_data")){
    data_files <- paste("data/",dir(path="data/", pattern="santa_monica.*"), sep = "")
    # lapply takes in a list and outputs the manipulated list
    # data_table_list is a list of data frames
    data_table_list <- lapply(data_files, read.table, header=TRUE, sep=",", stringsAsFactors=FALSE)
    # take  alist of data frames and turning it into a 
    park_data <- ldply(data_table_list, .fun=rbind) # combine data sets into one dataframe
    park_data <- subset(park_data, select=-c(X)) # drop added index column
  
    park_data <- dateStrToPosix(park_data)
    park_data <- addTimeColumns(park_data)
    park_data <- addWeekdayColumn(park_data)
  
  }

  return(park_data)
}

dateStrToPosix <- function(data){
  # Extract POSIX datetime from Date.Time character string
  data$Date.Time <- parse_date_time(data$Date.Time,"%Y/%m/%d %H!:%M:%S", tz="America/Los_Angeles")
  return(data)
}

getLotNames <- function(data){
  lot_names <- levels(factor(park_data$Lot))  
}

addTimeColumns <- function(data){
  # Add month, day, hour, min columns for aggregating data
  data$month <- month(data$Date.Time)
  data$day <- day(data$Date.Time)
  data$hour <- hour(data$Date.Time)
  data$minute <- minutes(data$Date.Time)
  return(data)
}

addWeekdayColumn <- function(data){
  data$weekday <- weekdays(data$Date.Time)
  return(data)
}

getAvgDailyAvailability <- function(data){
  # Calculate mean availability per lot per day each year
  park_data <- ddply(data, c("month", "day"), summarise, mean=mean(Available), sd=sd(Available) )
  return(park_data)
}

getAvgDailyAvailabilityPlot <- function(data){
  
  data <- getAvgDailyAvailability(data)
  # Create POSIX date column from strings of format "Month/Date"
  date_strs = sprintf("%i/%i", data$month, data$day)
  dates <- parse_date_time(date_strs, "md", tz="America/Los_Angeles")
  data$month_day <- dates
  
  # Plot Average Availability per Date
  park_plot <- ggplot(data)
  park_plot <- park_plot + geom_line(aes(x=month_day, y=mean)) # Available[1] = Mean Availability
  park_plot <- park_plot + labs(title="Average Parking Availability per Day", x="Date", y="Available Parking Spots") # Title + Axis Labels
  return(park_plot)
}

getAvgDailyAvailabilityHist <- function(data){
  # Avg Daily Availability Histogram
  data <- getAvgDailyAvailability(data)
  park_hist <- ggplot(data) + geom_histogram(aes(x=mean), binwidth = 10)
  park_hist <- park_hist + labs(title="Average Daily Parking Availability Histogram", x= "Average Parking Spots Available", y="Count")
  return(park_hist)
}

getAvgPerWeekday <- function(weekday_data){
  # Avg Spots Available per Weekday
  weekday_agg <- ddply(weekday_data, c("weekday"), summarise, mean=mean(Available))
  return(weekday_agg)
}

# to call use:
# x <- loadData()
# getAvgPerWeekdayPlot(x)
getAvgPerWeekdayPlot <- function(data){
  weekday_agg <- getAvgPerWeekday(data)
  # we call ggplot and the first param is the dataset, second is aes- meaning aestetic
  week_plot <- ggplot(weekday_agg, aes(x=weekday, y=mean)) + geom_bar(stat="identity")
  week_plot <- week_plot + labs(title="Average Availability per Weekday", x="Weekdays", y="Avg Parking Spots Available")
  return(week_plot)
}

isWeekday <- function(weekday){
  weekday_strs <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  if(weekday %in% weekday_strs){
    return(TRUE)
  }
  return(FALSE)
}

# Get Time data only from a Date
getTime <- function(dtime){
  time_str <- strftime(dtime, format="%H:%M:%S") #convert weekday times to strings
  time_only <- as.POSIXct(time_str, format="%H:%M:%S") # only extract hr/min/sec info from time strings
  return(time_only)
}

## Lot Availability Per Weekday
getLotWeekdayPlot <- function(data, lot, weekday_name){
  if(!isWeekday(weekday_name)){
    return(NULL)
  }
  
  day_data <- subset(data, Lot == lot & weekday == weekday_name, select = c(Date.Time, Available))
  day_hr_agg <- ddply(day_data, ~ getTime(Date.Time), summarize, mean=mean(Available))
  names(day_hr_agg)[1] <- "Date.Time"
  lotw_plot <- ggplot(day_hr_agg) + geom_point(aes(x=getTime(Date.Time), y=mean))
  
  title <- paste(lot, "Parking on a", weekday_name)
  lotw_plot <- lotw_plot + labs(title=title, x="Time of Day", y="Avg Parking Spots Available")
  
  return(lotw_plot)
}

## General Weekday Plot function
getWeekdayPlot <- function(data, weekday_name){
  ### Generate Plot for a given weekday
  if(!isWeekday(weekday_name)){
    return(NULL)
  }
  
  ### The weekday's Avg Availbaility
  day_data <- data[data$weekday == weekday_name,]
  
  # remove month/day/year data from datetime objects - for plotting
  
  day_data$time <- getTime(day_data$Date.Time)
  available_day <- ddply(day_data, c("time"), summarise, mean=mean(Available)) # avg availability per time of day

  # create plot
  day_plot_name <- paste(weekday_name, "_plot", sep="")
  day_plot <- ggplot(available_day) + geom_point(aes(x=time, y=mean))
  
  # add labels
  day_title <- paste(weekday_name, "Parking Availability", sep=" ")
  day_plot <- day_plot + labs(title=day_title, x="Time of Day", y="Total Available Spots")
  
  return(day_plot)
}

## Weekday Plots

getMondayPlot <- function(data){
  getWeekdayPlot(data, "Monday")
}

getTuesdayPlot <- function(data){
  getWeekdayPlot(data, "Tuesday")
}

getWednesdayPlot <- function(data){
  getWeekdayPlot(data, "Wednesday")
}

getThursdayPlot <- function(data){
  getWeekdayPlot(data, "Thursday")
}

getFridayPlot <- function(data){
  getWeekdayPlot(data, "Friday")
}

getSaturdayPlot <- function(data){
  getWeekdayPlot(data, "Saturday")
}

getSundayPlot <- function(data){
  getWeekdayPlot(data, "Sunday")
}

getHourlyBoxplot <- function(data){
  
  # Divide dataset by hour into groups: 1-3, 4-6, 7-9, 10-12, 1-3, 4-6, 7-9, 10-12
  x_labs <- c("1-3am", "4-6am", "7-9am", "10-12pm", "1-3pm", "4-6pm", "7-9pm", "10-12pm")
  
  data$hgroup <- hoursToGroups(data$hour)
  day_plot <- ggplot(data) + geom_boxplot(aes(x=factor(hgroup), y=Available, group=hgroup))
  
  title <- "Box and Whiskers Every 3 Hours"
  day_plot <- day_plot + labs(title=title, x="Time of Day", y="Available Parking Spots")
  day_plot <- day_plot + scale_x_discrete(breaks=1:8, labels = x_labs)
  
  return(day_plot)
}

getWeekdayBoxPlot <- function(data, weekday_name){
  weekday_data <- data[data$weekday == weekday_name,]
  day_plot <- getHourlyBoxplot(weekday_data)
  title <- paste("Box and Whiskers Every 3 Hours on", weekday_name, sep=" ")
  day_plot <- day_plot + labs(title=title)
  return(day_plot)
}

getParkingStructureBoxPlot <- function(data, parking_structure_name){
  parking_structure_data <- data[data$Lot == parking_structure_name,]
  day_plot <- getHourlyBoxplot(parking_structure_data)
  title <- paste("Box and Whiskers Every 3 Hours on", parking_structure_name, sep=" ")
  day_plot <- day_plot + labs(title=title)
  return(day_plot)
}

hourToGroup <- function(hour){
  if(hour %in% 1:3){
    return(1)
  } else if(hour %in% 4:6){
    return(2)
  } else if(hour %in% 7:9){
    return(3)
  } else if(hour %in% 10:12){
    return(4)
  } else if(hour %in% 13:15) {
    return(5)
  } else if(hour %in% 16:18) {
    return(6)
  } else if(hour %in% 19:21) {
    return(7)
  } else if(hour %in% c(22, 23, 0)) {
    return(8)
  }
}

hoursToGroups <- function(hours){
  hours <- lapply(hours, hourToGroup)
  hours <- unlist(hours)
  return(hours)
}
