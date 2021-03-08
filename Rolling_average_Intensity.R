setwd("X:/School/SFU/CMPT 318/Term Project Files")



install.packages("depmixS4")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("zoo")

library(nnet)
library(MASS)
library(Rsolnp)
library(nlme)
library(depmixS4)
library(lubridate)
library(ggplot2)
library(dplyr)
library(zoo)

#-------------------- Choose which data to use 

df = read.table("TermProjectData.txt", header = TRUE, sep = ",")

#df = read.table("Data1(WithAnomalies).txt", header = TRUE, sep = ",")

#df = read.table("Data2(WithAnomalies).txt", header = TRUE, sep = ",")

#df = read.table("Data3(WithAnomalies).txt", header = TRUE, sep = ",")



df$Date <- as.POSIXct(df$Date, format = "%d/%m/%Y")
df <- na.omit(df)



dfTime <- df[df$Time >= "06:00:00" & df$Time <= "20:00:00",]
dfTime$Days <- c(weekdays(dfTime$Date, abbreviate = FALSE))


dfTime <- dfTime[dfTime$Days == "Monday" | dfTime$Days == "Tuesday" | dfTime$Days == "Wednesday" | 
                   dfTime$Days == "Thursday" | dfTime$Days == "Friday",]



#combines dates and times
Dates <- with(dfTime, ymd(Date) + hms(Time))


#Converts 3 days into minutes (window)
n <- 1*60*24*3


#calculates moving average values
#Global_intensity
#Global_intenstity
ma <- rollmean(dfTime$Global_intenstity, n)

#array of na values
beg <- rep(NA, n - 1)

#append NA to values at the beginning of moving average
ma2 <- append(ma, beg, 0)


#Converts 12 hours into minutes (window)
m <- 1*60*12


#calculates moving average values
#Global_intensity
#Global_intenstity

ma3 <- rollmean(dfTime$Global_intenstity, m)

#array of na values
beg2 <- rep(NA, m - 1)

#append NA to values at the beginning of moving average
ma4 <- append(ma3, beg2, 0)




rm(ma, ma3)



#create data frame with moving average
#Global_intensity
anomplotdata <- data.frame(dfTime$Global_intenstity, Dates, dfTime$Time, ma2, ma4)


#plot line and moving average on plot
plot1 <- ggplot(anomplotdata, aes(Dates, dfTime.Global_intenstity))  + geom_line(size = 0.1, alpha =.5) + 
  geom_line(aes(y = ma4), size = 1.5, color = "midnightblue" )+
  geom_line(aes(y = ma2), size = 1.5, color = "darkred" ) + theme(legend.position="top")
plot(plot1)
