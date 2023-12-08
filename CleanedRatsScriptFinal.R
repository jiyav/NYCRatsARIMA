install.packages("lubridate")
library("lubridate")
install.packages("ggplot2")
install.packages("forecast")
library(forecast)
library(ggplot2)


Rat2018 <- read.csv("Rodent_Inspection2018.csv")
Rat2019 <- read.csv("Rodent_Inspection2019.csv")
Rat2020 <- read.csv("Rodent_Inspection2020.csv")
Rat2021 <- read.csv("Rodent_Inspection2021.csv")
Rat2022 <- read.csv("Rodent_Inspection2022.csv")
Rat2023 <- read.csv("Rodent_Inspection2023.csv")

#remove all columns expect borough code, inspection data, and result
Rat2018 <- subset(Rat2018, select = c(BORO_CODE, INSPECTION_DATE, RESULT))
Rat2019 <- subset(Rat2019, select = c(BORO_CODE, INSPECTION_DATE, RESULT))
Rat2020 <- subset(Rat2020, select = c(BORO_CODE, INSPECTION_DATE, RESULT))
Rat2021 <- subset(Rat2021, select = c(BORO_CODE, INSPECTION_DATE, RESULT))
Rat2022 <- subset(Rat2022, select = c(BORO_CODE, INSPECTION_DATE, RESULT))
Rat2023 <- subset(Rat2023, select = c(BORO_CODE, INSPECTION_DATE, RESULT))

Rat18to23 <- rbind(Rat2018,Rat2019,Rat2020, Rat2021, Rat2022, Rat2023)
RatInspectionTotal <- Rat18to23


#need to remove NA/null date entries for as.Date() to work
NYCRatActivity <- Rat18to23[Rat18to23$RESULT=='Rat Activity',]
NYCRatActivity <-NYCRatActivity[NYCRatActivity$BORO_CODE==1,]
RatInspectionTotal <- RatInspectionTotal[RatInspectionTotal$BORO_CODE==1,]
Rat18to23 <- NYCRatActivity

#parse the inspection datetime into a date and time. 
inspectionDateTime <- parse_date_time(Rat18to23$INSPECTION_DATE, "%m/%d/%y %I:%M:%S %p")
Rat18to23$inspectionDateTime <- inspectionDateTime

inspectionDateTime1 <- parse_date_time(RatInspectionTotal$INSPECTION_DATE, "%m/%d/%y %I:%M:%S %p")
RatInspectionTotal$inspectionDateTime1 <- inspectionDateTime1


#extract only the Date using as.Date() function
#as.Date() ignores all values after the date string, so it ignores time
Rat18to23$inspectionDate <- as.Date(inspectionDateTime)
RatInspectionTotal$inspectionDate <- as.Date(inspectionDateTime1)


#now we want to make a month and a year column
Rat18to23$year <- strftime(Rat18to23$inspectionDate, "%Y")    # Create year column
Rat18to23$month <- strftime(Rat18to23$inspectionDate, "%m") 
Rat18to23$RatActivity <- 1

RatInspectionTotal$year <- strftime(RatInspectionTotal$inspectionDate, "%Y")    # Create year column
RatInspectionTotal$month <- strftime(RatInspectionTotal$inspectionDate, "%m") 
RatInspectionTotal$RatInspection <- 1

Rat18to23 <- aggregate(RatActivity ~ month + year,       # Aggregate data
                       Rat18to23,
                       FUN = sum)
Rat18to23agg <- Rat18to23
Rat18to23agg

RatInspectionTotal <- aggregate(RatInspection ~ month + year,       # Aggregate data
                                RatInspectionTotal,
                                FUN = sum)
RatInspectionTotalagg <- RatInspectionTotal
RatInspectionTotalagg

PercentageAgg <- Rat18to23agg$RatActivity/RatInspectionTotalagg$RatInspection
PercentageAgg
plot(PercentageAgg)

#turn Rat activity data frame into time series
RatsTest <- Rat18to23$RatActivity
RatsTestTS <- ts(RatsTest, start=c(2018), end=c(2023), frequency = 12)

#turn total rat inspection data frame into time series
RatInspectionTotal <- RatInspectionTotal$RatInspection
RatInspectionTotalTS <- ts(RatInspectionTotal, start=c(2018), end=c(2023), frequency = 12) 

#plot total rat inspections and rat activity
plot(RatInspectionTotalTS, col = 2, ylim = c(0, 10000), xlab = "Time", ylab = "Inspections")
lines(RatsTestTS, col = 3)
legend("top", c("Total Rat Inspections", "Rat Activity Found"), lty = 1, col = 2:3, cex = 0.5)
title(main = "Inspection Result vs Time")

#create ratio of rat activity to rat inspection
Percentage <- RatsTestTS/RatInspectionTotalTS
plot(Percentage, col = 2, ylim = c(0, 1), xlab = "Time", ylab = "Ratio")
title(main="Rat Activity to Total Inspection Ratio")



#perform decomposition
decomp <- decompose(RatsTestTS, type="additive")
plot(decomp)

#create auto.arima model
model <-auto.arima(RatsTestTS,D=1)

#create forecast from auto.arima() model
fc <-forecast(model,h=36)

#turn forecast into dataframe
forecast <- data.frame(fc)
View(forecast)

#plot forecast
plot(fc, ylim = c(0,7000))
