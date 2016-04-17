require(ggplot2)
require(plyr)
require(lubridate)

# Helper Functions
source("multiplot.R")

# Load Parking Data

park_data <- read.table(file="data/Parking_Lot_Counts.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

# Extract POSIX datetime from Date.Time character string
park_data$Date.Time <- parse_date_time(park_data$Date.Time,"%m/%d/%Y %I:%M:%S %p", tz="America/Los_Angeles")

lot_names <- levels(factor(park_data$Lot))

# Add month, day, hour, min columns for aggregating data
park_data$month <- month(park_data$Date.Time)
park_data$day <- day(park_data$Date.Time)
park_data$hour <- hour(park_data$Date.Time)
park_data$minute <- minutes(park_data$Date.Time)

# Calculate mean availability per lot per day each year
park_graph_data <- ddply(park_data, c("month", "day"), summarise, mean=mean(Available), sd=sd(Available) )

# Create POSIX date column from strings of format "Month/Date"
date_strs = sprintf("%i/%i", park_graph_data$month, park_graph_data$day)
dates <- parse_date_time(date_strs, "md", tz="America/Los_Angeles")
park_graph_data$month_day <- dates

# Plot Average Availability per Date
park_plot <- ggplot(park_graph_data)
park_plot <- park_plot + geom_line(aes(x=month_day, y=mean)) # Available[1] = Mean Availability
#park_plot <- park_plot + geom_line(aes(x=month_day, y=mean+sd)) # +Std Deviation
#park_plot <- park_plot + geom_line(aes(x=month_day, y=mean-sd))

# Add Title and Axis Labels
park_plot <- park_plot + labs(title="Average Parking Availability per Day", x="Date", y="Available Parking Spots")

# Histrogram
park_hist <- ggplot(park_graph_data) + geom_histogram(aes(x=mean), binwidth = 10)
park_hist <- park_hist + labs(title="Average Daily Parking Availability Histogram", x= "Average Parking Spots Available", y="Count")

