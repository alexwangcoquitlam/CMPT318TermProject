
setwd("X:/School/SFU/CMPT 318/Term Project Files")

library(nnet)
library(MASS)
library(Rsolnp)
library(nlme)
library(depmixS4)
library(dplyr)


#-------------------- Read main data from text file and turn dates to POSIXct Omiting NA values

df = read.table("TermProjectData.txt", header = TRUE, sep = ",")
df <- na.omit(df)
df$Date <- as.POSIXct(df$Date, format = "%d/%m/%Y")

#-------------------- Time window between 6:00 and 20:00 adding column corresponding to the day
dfTime <- df[df$Time >= "06:00:00" & df$Time <= "20:00:00",]
dfTime$Days <- c(weekdays(dfTime$Date, abbreviate = FALSE))

#-------------------- Filtering data so it is only weekdays
dfTime <- dfTime[dfTime$Days == "Monday" | dfTime$Days == "Tuesday" | dfTime$Days == "Wednesday" | 
                      dfTime$Days == "Thursday" | dfTime$Days == "Friday",]


split <- as.POSIXct("01/01/2009", format = "%d/%m/%Y")


#-------------------- Split main data into train and test data from date 01/01/2009
trainData <- dfTime[dfTime$Date < split, ]


testData <- dfTime[dfTime$Date >= split, ]

# n is number of states
n <- 15

rm(df, dfTime)

#Train HHM
set.seed(1)
mod <- depmix(list(Global_active_power~ 1, Global_intensity ~ 1), data = testData, nstates = n,
               family = list(gaussian(), gaussian()))

modNew <- depmix(list(Global_active_power~ 1, Global_intensity ~ 1), data = trainData, nstates = n, family = list(gaussian(), gaussian()))


modnew <- setpars(modNew, getpars(mod))

#Test HHM
fm <- fit(modnew)

summary(fm)
print(fm)




#-------------------------anomaly 1 Test

#-------------------- Read Data with anomaly 1 Omiting NA values
dfAn1 = read.table("Data1(WithAnomalies).txt", header = TRUE, sep = ",")
dfAn1$Date <- as.POSIXct(dfAn1$Date, format = "%d/%m/%Y")
dfAn1 <- na.omit(dfAn1)

#-------------------- Time window between 6:00 and 20:00 adding column corresponding to the day
dfTimeAn1 <- dfAn1[dfAn1$Time >= "06:00:00" & dfAn1$Time <= "20:00:00",]
dfTimeAn1$Days <- c(weekdays(dfTimeAn1$Date, abbreviate = FALSE))

#-------------------- Filtering data so it is only weekdays
testData1 <- dfTimeAn1[dfTimeAn1$Days == "Monday" | dfTimeAn1$Days == "Tuesday" | dfTimeAn1$Days == "Wednesday" | 
                         dfTimeAn1$Days == "Thursday" | dfTimeAn1$Days == "Friday",]



#Test HHM
set.seed(1)
mod1 <- depmix(list(Global_active_power~ 1, Global_intensity ~ 1), data = trainData, nstates = n,
               family = list(gaussian(), gaussian()))

modNew1 <- depmix(list(Global_active_power~ 1, Global_intensity ~ 1), data = testData1, nstates = n, family = list(gaussian(), gaussian()))


modnew1 <- setpars(modNew1, getpars(mod1))


#Fit HHM
fm1 <- fit(modnew1)

#summary(fm1)
print(fm1)


#-----------------------------Anomaly 2

#-------------------- Read Data with anomaly 2 Omiting NA values
dfAn2 = read.table("Data2(WithAnomalies).txt", header = TRUE, sep = ",")
dfAn2$Date <- as.POSIXct(dfAn2$Date, format = "%d/%m/%Y")
dfAn2 <- na.omit(dfAn2)

#-------------------- Time window between 6:00 and 20:00 adding column corresponding to the day
dfTimeAn2 <- dfAn2[dfAn2$Time >= "06:00:00" & dfAn2$Time <= "20:00:00",]
dfTimeAn2$Days <- c(weekdays(dfTimeAn2$Date, abbreviate = FALSE))

#-------------------- Filtering data so it is only weekdays
testData2 <- dfTimeAn2[dfTimeAn2$Days == "Monday" | dfTimeAn2$Days == "Tuesday" | dfTimeAn2$Days == "Wednesday" | 
                         dfTimeAn2$Days == "Thursday" | dfTimeAn2$Days == "Friday",]

#Test HHM
set.seed(1)
mod2 <- depmix(list(Global_active_power~ 1, Global_intensity ~ 1), data = trainData, nstates = n,
               family = list(gaussian(), gaussian()))

modNew2 <- depmix(list(Global_active_power~ 1, Global_intensity ~ 1), data = testData2, nstates = n, family = list(gaussian(), gaussian()))


modnew2 <- setpars(modNew2, getpars(mod2))


#Fit HHM
fm2 <- fit(modnew2)

#summary(fm2)
#print(fm2)





#----------------------------Anomaly 3


#-------------------- Read Data with anomaly 2 Omiting NA values
dfAn3 = read.table("Data3(WithAnomalies).txt", header = TRUE, sep = ",")
dfAn3$Date <- as.POSIXct(dfAn3$Date, format = "%d/%m/%Y")
dfAn3 <- na.omit(dfAn3)

#-------------------- Time window between 6:00 and 20:00 adding column corresponding to the day
dfTimeAn3 <- dfAn3[dfAn3$Time >= "06:00:00" & dfAn3$Time <= "20:00:00",]
dfTimeAn3$Days <- c(weekdays(dfTimeAn3$Date, abbreviate = FALSE))

#-------------------- Filtering data so it is only weekdays
testData3 <- dfTimeAn3[dfTimeAn3$Days == "Monday" | dfTimeAn3$Days == "Tuesday" | dfTimeAn3$Days == "Wednesday" | 
                         dfTimeAn3$Days == "Thursday" | dfTimeAn3$Days == "Friday",]

#Test HHM
set.seed(1)
mod3 <- depmix(list(Global_active_power~ 1, Global_intensity ~ 1), data = trainData, nstates = n,
               family = list(gaussian(), gaussian()))

modNew3 <- depmix(list(Global_active_power~ 1, Global_intensity ~ 1), data = testData3, nstates = n, family = list(gaussian(), gaussian()))


modnew3 <- setpars(modNew3, getpars(mod3))


#Fit HMM
fm3 <- fit(modnew3)

#summary(fm3)
#print(fm3)



#-------------------- print HMM log likelyhoods
print(fm1)

print(fm2)

print(fm3)


