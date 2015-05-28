#This script is designed to crawl the basis data and look for creative ways to plot it

#Clear console and load libraries
rm(list=ls(all=TRUE))
library(plyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(scales)

#Get list of files
Basis_Metrics_Files <- list.files("F:/LinuxVM_Storage/Basis_Data", "metrics", full.names=T, recursive=T)

#Read in Basis Data
Metrics_Data <- ldply(Basis_Metrics_Files, fread)

#Split up timestamp info
Metrics_Data$timestamp <- as.POSIXlt(Metrics_Data$timestamp)
Metrics_Data$Date <- as.Date(Metrics_Data$timestamp)
Metrics_Data$Time <- format(Metrics_Data$timestamp, "%H:%M")
Metrics_Data$Month <- month(Metrics_Data$Date, label=T)
Metrics_Data$Year <- year(Metrics_Data$Date)
Metrics_Data$Day <- weekdays(as.Date(Metrics_Data$Date))
Metrics_Data$Day <- factor(Metrics_Data$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                      "Friday", "Saturday", "Sunday"))

#Explore the ranges of data
quantile(Metrics_Data$heartrate, na.rm=T)
# 0%  25%  50%  75% 100% 
# 30   60   65   71  162
quantile(Metrics_Data$steps, na.rm=T)  
# 0%  25%  50%  75% 100% 
# 0    0    0    0  188
quantile(Metrics_Data$calories, na.rm=T)
# 0%  25%  50%  75% 100% 
#0.6  1.2  1.7  2.0 65.5
quantile(Metrics_Data$gsr, na.rm=T) 
#      0%      25%      50%      75%     100% 
#2.65e-04 4.63e-04 5.49e-04 8.99e-04 3.15e+01
quantile(Metrics_Data$skintemp, na.rm=T) 
#  0%  25%  50%  75% 100% 
#69.8 88.4 90.2 91.6 97.9
quantile(Metrics_Data$airtemp, na.rm=T)         
#   0%   25%   50%   75%  100% 
# 32.0  82.8  85.1  87.1 104.7
#######################################################################################################
ggplot(Metrics_Data, aes(x=Day, y=heartrate)) +
     geom_boxplot()
#######################################################################################################
SecondDay <- Metrics_Data[Metrics_Data$Date=="2014-01-23", ]
SecondDay$Time <- as.POSIXlt(SecondDay$Time, "%H:%M", tz="GMT")

ggplot(SecondDay, aes(x=Time, y=heartrate)) +
     geom_point() +
     scale_x_datetime(breaks="1 hours", labels = date_format("%H")) +
     ylab("Heart Rate") +
     ggtitle("Heart Rate measured each minute on 23-Jan-2015") +
     scale_y_continuous(breaks=c(seq(30, 130, 10)), labels=c(seq(30, 130, 10)), limits=c(30, 130)) +
     geom_smooth(method="loess")

ggsave("Heart Rate measured each minute on 23-Jan-2015.png", height=8, width=16)

ggplot(SecondDay, aes(x=Time, y=steps)) +
     geom_point() +
     scale_x_datetime(breaks="1 hours", labels = date_format("%H")) +
     ylab("Steps") +
     ggtitle("Steps measured each minute on 23-Jan-2015") +
     scale_y_continuous(breaks=c(seq(0, 130, 10)), labels=c(seq(0, 130, 10)), limits=c(0, 130))

ggsave("Steps measured each minute on 23-Jan-2015.png", height=8, width=16)

ggplot(SecondDay, aes(x=Time, y=calories)) +
     geom_point() +
     scale_x_datetime(breaks="1 hours", labels = date_format("%H"))

ggplot(SecondDay, aes(x=Time, y=gsr)) +
     geom_point() +
     scale_x_datetime(breaks="1 hours", labels = date_format("%H"))

ggplot(SecondDay, aes(x=Time, y=skintemp)) +
     geom_point() +
     scale_x_datetime(breaks="1 hours", labels = date_format("%H")) +
     ylab("Skin Temp (F)") +
     ggtitle("Skin Temp measured each minute on 23-Jan-2015") +
     scale_y_continuous(breaks=c(seq(70, 100, 5)), labels=c(seq(70, 100, 5)), limits=c(70, 100))

ggsave("Skin Temp measured each minute on 23-Jan-2015.png", height=8, width=16)
#######################################################################################################
#Stats by Date
Stats_by_Date <- ddply(Metrics_Data, .(Date), summarize,
                       Date=unique(Date),
                       Day=unique(Day),
                       Mean_HR=mean(heartrate, na.rm = T),
                       Median_HR=median(heartrate, na.rm = T),
                       Total_Steps=sum(steps, na.rm = T),
                       Total_Calories=sum(calories, na.rm = T),
                       Mean_Skintemp=mean(skintemp, na.rm = T),
                       Median_Skintemp=median(skintemp, na.rm = T),
                       Mean_Airtemp=mean(airtemp, na.rm = T),
                       Median_Airtemp=median(airtemp, na.rm = T))

ggplot(Stats_by_Date, aes(x=Date, y=Mean_HR)) +
     geom_point() +
     geom_smooth(method = "loess") +
     scale_y_continuous(breaks=c(seq(54, 80, 4)), labels=c(seq(54, 80, 4))) +
     ylab("Mean Heart Rate") +
     ggtitle("Mean Heart Rate Per Day")

ggsave(paste("Mean Heart Rate Per Day from ", min(Stats_by_Date$Date), " to ", max(Stats_by_Date$Date),
             ".png", sep=""), height=8, width=16)

ggplot(Stats_by_Date, aes(x=Date, y=Median_HR)) +
     geom_point() +
     geom_smooth(method = "loess") +
     scale_y_continuous(breaks=c(seq(54, 80, 4)), labels=c(seq(54, 80, 4)))

ggplot(Stats_by_Date, aes(x=Date, y=Total_Steps)) +
     geom_point() +
     geom_smooth(method = "loess") +
     ylab("Total Steps") +
     ggtitle("Total Steps Per Day")

ggsave(paste("Total Steps Per Day from ", min(Stats_by_Date$Date), " to ", max(Stats_by_Date$Date),
             ".png", sep=""), height=8, width=16)

ggplot(Stats_by_Date, aes(x=Date, y=Total_Calories)) +
     geom_point() +
     geom_smooth(method = "loess") +
     ylab("Total Calories") +
     ggtitle("Total Calories Per Day")

ggsave(paste("Total Calories Per Day from ", min(Stats_by_Date$Date), " to ", max(Stats_by_Date$Date),
             ".png", sep=""), height=8, width=16)

#Stats by Month
Stats_by_Month <- ddply(Metrics_Data, .(Month, Year), summarize,
                        Month=unique(Month),
                        Year=as.character(unique(Year)),
                        Mean_HR=mean(heartrate, na.rm = T),
                        Median_HR=median(heartrate, na.rm = T),
                        Total_Steps=sum(steps, na.rm = T),
                        Total_Calories=sum(calories, na.rm = T),
                        Mean_Skintemp=mean(skintemp, na.rm = T),
                        Median_Skintemp=median(skintemp, na.rm = T),
                        Mean_Airtemp=mean(airtemp, na.rm = T),
                        Median_Airtemp=median(airtemp, na.rm = T))

ggplot(Stats_by_Month, aes(x=Month, y=Mean_HR, color=Year)) +
     geom_point() +
     scale_colour_manual(values = c("2014" = "red","2015" = "blue")) +
     scale_y_continuous(breaks=c(seq(60, 70, 2)), labels=c(seq(60, 70, 2)), limits=c(60, 70))

#Stats by Day of the Week
Stats_by_Weekday <- ddply(Metrics_Data, .(Day), summarize,
                          Day=unique(Day),
                          Mean_HR=mean(heartrate, na.rm = T),
                          Median_HR=median(heartrate, na.rm = T),
                          Total_Steps=sum(steps, na.rm = T),
                          Total_Calories=sum(calories, na.rm = T),
                          Mean_Skintemp=mean(skintemp, na.rm = T),
                          Median_Skintemp=median(skintemp, na.rm = T),
                          Mean_Airtemp=mean(airtemp, na.rm = T),
                          Median_Airtemp=median(airtemp, na.rm = T))

ggplot(Stats_by_Weekday, aes(x=Day, y=Mean_HR)) +
     geom_point() +
     scale_y_continuous(breaks=c(seq(60, 70, 2)), labels=c(seq(60, 70, 2)), limits=c(60, 70))


#Stats by Time
Stats_by_Time <- ddply(Metrics_Data, .(Time), summarize,
                       Time=unique(Time),
                       Mean_HR=mean(heartrate, na.rm = T),
                       Median_HR=median(heartrate, na.rm = T),
                       Total_Steps=sum(steps, na.rm = T),
                       Total_Calories=sum(calories, na.rm = T),
                       Mean_Skintemp=mean(skintemp, na.rm = T),
                       Median_Skintemp=median(skintemp, na.rm = T),
                       Mean_Airtemp=mean(airtemp, na.rm = T),
                       Median_Airtemp=median(airtemp, na.rm = T))

Stats_by_Time$Time <- as.POSIXlt(Stats_by_Time$Time, "%H:%M", tz="GMT")

ggplot(Stats_by_Time, aes(x=Time, y=Mean_HR)) +
     geom_point() +
     scale_x_datetime(breaks="1 hours", labels = date_format("%H")) +
     geom_vline(xintercept=as.numeric(as.POSIXct("05:45", "%H:%M", tz="GMT")), linetype=2, colour="red") +
     ylim(58, 74)

ggplot(Stats_by_Time, aes(x=Time, y=Median_HR)) +
     geom_point() +
     scale_x_datetime(breaks="1 hours", labels = date_format("%H")) +
     geom_vline(xintercept=as.numeric(as.POSIXct("05:45", "%H:%M", tz="GMT")), linetype=2, colour="red") +
     ylim(58, 72)

ggplot(Stats_by_Time, aes(x=Time, y=Total_Steps)) +
     geom_point() +
     scale_x_datetime(breaks="1 hours", labels = date_format("%H")) +
     ggtitle(paste("Total Steps Per Minute from\n", min(Stats_by_Date$Date), " to ",
                   max(Stats_by_Date$Date), sep=""))

ggsave(paste("Total Steps Per Minute from ", min(Stats_by_Date$Date), " to ", max(Stats_by_Date$Date),
             ".png", sep=""), height=8, width=16)

ggplot(Stats_by_Time, aes(x=Time, y=Total_Calories)) +
     geom_point() +
     scale_x_datetime(breaks="1 hours", labels = date_format("%H")) +
     ggtitle(paste("Total Calories Per Minute from\n", min(Stats_by_Date$Date), " to ",
                   max(Stats_by_Date$Date), sep="")) +
     scale_y_continuous(breaks=c(seq(0, 1300, 100)), labels=c(seq(0, 1300, 100)), limits=c(0, 1300))

ggsave(paste("Total Calories Per Minute from ", min(Stats_by_Date$Date), " to ", max(Stats_by_Date$Date),
             ".png", sep=""), height=8, width=16)

