library(devtools)
library(ggplot2)
library(ggbiplot)
library(depmixS4)
library(dplyr)
library(zoo)
library(lubridate)

getwd()
setwd("D:/Homework & Work/SFU/CMPT 318/CMPT 318 - Term Project/R Code")

df <- read.table("TermProjectData.txt", header = TRUE, sep = ",")
df <- na.omit(df)

df[df$Global_active_power == 0, "Global_active_power"] <- NA
df <- na.omit(df)
set.seed(1)

df$Date <- as.POSIXct(df$Date, format = "%d/%m/%Y")
df$Days <- c(weekdays(df$Date, abbreviate = FALSE))

weekdays <- df[df$Days == "Monday" | df$Days == "Tuesday" | df$Days == "Wednesday" | 
                 df$Days == "Thursday" | df$Days == "Friday",]

# Time window is 6:00 AM to 8:00 PM
weekdaysTime <- weekdays[weekdays$Time >= "06:00:00" & weekdays$Time <= "20:00:00",]

# ----------------------------

# PCA

weekdays.pca <- prcomp(weekdaysTime[,c(3:9)], center = TRUE, scale = TRUE)

summary(weekdays.pca)
ggbiplot(weekdays.pca)

# ------------------------------

# Univariate HMM training

mod1 <- depmix(response = Global_active_power ~ 1, data = weekdaysTime, nstates = 11)
fm1 <- fit(mod1)

summary(fm1)
print(fm1)

# ---------------------------

# Polynomial Regression for anomalies

# Read data here - change between .txt files
anom1 <- read.table("Data3(WithAnomalies).txt", header = TRUE, sep = ",")
anom1 <- na.omit(anom1)

anom1$Date <- as.POSIXct(anom1$Date, format = "%d/%m/%Y")
anom1$Days <- c(weekdays(anom1$Date, abbreviate = FALSE))

anom1Weekdays <- anom1[anom1$Days == "Monday" | anom1$Days == "Tuesday" | anom1$Days == "Wednesday" | 
                         anom1$Days == "Thursday" | anom1$Days == "Friday",]

anom1WeekdaysTime <- anom1Weekdays[anom1Weekdays$Time >= "06:00:00" & anom1Weekdays$Time <= "20:00:00",]

anom1WeekdaysTime$Time <- as.POSIXct(anom1WeekdaysTime$Time, format = "%H:%M:%S")

startTime <- as.POSIXct("06:00:00", format = "%H:%M:%S")
endTime <- as.POSIXct("20:00:00", format = "%H:%M")

anom1Avg <- data.frame(Time = double(), Average = double())

while (startTime <= endTime) {
  time <- filter(anom1WeekdaysTime, anom1WeekdaysTime$Time == startTime)
  
  # Change value here - Global_active_power vs Global_intensity
  avg <- mean(time$Global_active_power)
  
  t <- strftime(startTime, format = "%H:%M:%S")
  tempdf <- c(t, avg)
  anom1Avg <- rbind(anom1Avg, tempdf)
  names(anom1Avg) <- c("Time", "Average")
  
  startTime <- startTime + 1*60
}

anom1Avg$Time <- as.POSIXct(anom1Avg$Time, format = "%H:%M:%S")
anom1Avg$Average <- as.numeric(anom1Avg$Average)

degree <- 11

# Only graphs polynomial
ggplot() +
  geom_point(data = anom1Avg, mapping = aes(x = Time, y = Average, color = "Points")) +
  geom_smooth(data = anom1Avg, mapping = aes(x = Time, y = Average, color = "Polynomial"),
              method = "lm", formula = y ~ poly(x, degree), se = FALSE) +
  scale_color_manual(name = "Legend", values = c("black", "red")) +
  labs(title = "Global_active_power")

# --------------------------

# Linear and Polynomial Regression for trained model

weekdaysTime$Time <- as.POSIXct(weekdaysTime$Time, format = "%H:%M:%S")

startTime <- as.POSIXct("06:00:00", format = "%H:%M:%S")
endTime <- as.POSIXct("20:00:00", format = "%H:%M")

weekdayAvg <- data.frame(Time = double(), Average = double())

while (startTime <= endTime) {
  time <- filter(weekdaysTime, weekdaysTime$Time == startTime)

  # Change value here
  avg <- format(round(mean(time$Global_active_power), 2), nsmall = 2)

  t <- strftime(startTime, format = "%H:%M:%S")
  tempdf <- c(t, avg)
  weekdayAvg <- rbind(weekdayAvg, tempdf)
  names(weekdayAvg) <- c("Time", "Average")

  startTime <- startTime + 1*60
}

weekdayAvg$Time <- as.POSIXct(weekdayAvg$Time, format = "%H:%M:%S")
weekdayAvg$Average <- as.numeric(weekdayAvg$Average)

degree <- 11

# Only graphs polynomial or linear (change the formula)
ggplot() +
  geom_point(data = weekdayAvg, mapping = aes(x = Time, y = Average, color = "Points")) +
  geom_smooth(data = weekdayAvg, mapping = aes(x = Time, y = Average, color = "Polynomial"),
              method = "lm", formula = y ~ poly(x, degree), se = FALSE) +
  scale_color_manual(name = "Legend", values = c("black", "red")) +
  labs(title = "Global_active_power")

# Graphs both polynomial and linear
ggplot() +
  geom_point(data = weekdayAvg, mapping = aes(x = Time, y = Average, color = "Points")) +
  geom_smooth(data = weekdayAvg, mapping = aes(x = Time, y = Average, color = "Linear"),
              method = "lm", formula = y ~ x, se = FALSE) +
  geom_smooth(data = weekdayAvg, mapping = aes(x = Time, y = Average, color = "Polynomial"),
              method = "lm", formula = y ~ poly(x, degree), se = FALSE) +
  scale_color_manual(name = "Legend", values = c("red", "black", "purple")) +
  labs(title = "Global_active_power")

# -----------------------------