main <- function(){
  require(ggplot2)
  require(plyr)
  require(lubridate)
  
  # Load Parking Data
  if(!exists("park_data")){
    data_files <- paste("data/",dir(path="data/", pattern="santa_monica.*"), sep = "")
    data_table_list <- lapply(data_files, read.table, header=TRUE, sep=",", stringsAsFactors=FALSE)
    park_data <- ldply(data_table_list, .fun=rbind) # combine data sets into one dataframe
    park_data <- subset(park_data, select=-c(X)) # drop added index column
  }
  
  park_data <- dateStrToPosix(park_data)
  park_data <- addTimeColumns(park_data)
  park_data <- addWeekdayColumn(park_data)

  getAvgDailyAvailabilityPlot(park_data)
  #assign("park_data", park_data, envir = .GlobalEnv) # assign data to global variable
  
  ## Other Plots
  #getAvgDailyAvailabilityHist(park_data) # histogram
  getAvgPerWeekdayPlot(park_data)
  
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

getAvgPerWeekdayPlot <- function(data){
  weekday_agg <- getAvgPerWeekday(data)
  week_plot <- ggplot(weekday_agg, aes(x=weekday, y=mean)) + geom_bar(stat="identity")
  week_plot <- week_plot + labs(title="Average Availability per Weekday", x="Weekdays", y="Avg Parking Availability")
  return(week_plot)
}

## General Weekday Plot function
getWeekdayPlot <- function(data, weekday_name){
  ### Generate Plot for a given weekday
  weekday_strs <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  if(!(weekday_name %in% weekday_strs)){
    return(NULL)
  }
  ### The weekday's Avg Availbaility
  day_data <- data[data$weekday == weekday,]
  
  # remove month/day/year data from datetime objects - for plotting
  time_str <- strftime(day_data$Date.Time, format="%H:%M:%S") #convert weekday times to strings
  time_only <- as.POSIXct(time_str, format="%H:%M:%S") # only extract hr/min/sec info from time strings
  day_data$time <- time_only
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
