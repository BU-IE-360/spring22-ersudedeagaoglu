library(readxl)
library(skimr)
library(janitor)
library(readr)
library(tidyverse)
library(fBasics)
library(dplyr)
library(data.table)
library(writexl)
library(scales)
library(plyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(forecast)
library(corrplot)
library(ggcorrplot)
library(xts)

setwd("C:/Users/Erkut/Desktop/ie 360")

data <- read.csv("IE360_Spring22_HW2_data_.csv")
data <- data[,c(1:11)]
# data <- data[data$Unleaded.Gasoline.Sale..UGS.!="",]
str(data)
data$Quarter <- as.yearqtr(data$Quarter, format = "%Y_Q%q")
data$Quarter
str(data)
data_ugs <- data[,2]
data_ugs <- data_ugs[!is.na(data_ugs)]

datats <- ts(data_ugs,freq=4,start=c(2000,1))
str(datats)
datats
ts.plot(datats,xlab = "Year", ylab = "Sale",main=" Unleaded Gasoline Sale (in 1000 m3)")
dec_data<-decompose(datats, type="additive")
plot(dec_data) 
  
acf(data_ugs, lag = 12)

trend <- 1:nrow(data)
data$trend <- trend
data$quarter <- rep(1:4,8)
data$lag1 <- NA
data$lag4 <- NA

data$quarter <- as.factor(data$quarter)

for(i in 1:28){
  data$lag1[i+1] = data$Unleaded.Gasoline.Sale..UGS.[i]
}

for(i in 1:28){
  data$lag4[i+4] = data$Unleaded.Gasoline.Sale..UGS.[i]
}


model <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - lag4, data = data[1:28,])
summary(model)

temp_data <- data[!is.na(data$Unleaded.Gasoline.Sale..UGS. & data$lag1 & data$lag4),]
corr = cor(temp_data[,unlist(lappl(ytemp_data, is.numeric))])
ggcorrplot(corr,
           hc.order = TRUE,
           type='lower',
           lab=TRUE)

model2 <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - Price.of.Diesel.Gasoline..PG., data = data[1:28,])
summary(model2)

model3 <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - Price.of.Diesel.Gasoline..PG.
             - GNP.Agriculture, data = data[1:28,])
summary(model3)

model4 <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - Price.of.Diesel.Gasoline..PG.
             -GNP.Agriculture - GNP.Total  , data = data[1:28,])
summary(model4)

model5 <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - Price.of.Diesel.Gasoline..PG.
             -GNP.Agriculture - GNP.Total - GNP.Commerce , data = data[1:28,])
summary(model5)

model6 <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - Price.of.Diesel.Gasoline..PG.
             -GNP.Agriculture - GNP.Total - GNP.Commerce - RNUV, data = data[1:28,])
summary(model6)

model7 <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - Price.of.Diesel.Gasoline..PG.
             -GNP.Agriculture - GNP.Total - GNP.Commerce - RNUV - X..LPG.Vehicles..NLPG., data = data[1:28,])
summary(model7)

best_model <- lm(Unleaded.Gasoline.Sale..UGS. ~ Price.of.Unleaded.Gasoline..PU.
                 + X..Unleaded.Gasoline.Vehicles..NUGV. + X..of.Diesel.Gasoline.Vehicles..NDGV.
                 + quarter + trend + lag1, data = data[1:28,])
summary(best_model)

checkresiduals(best_model)

data

pred = c()
for(i in 1:4){
  pred[i] = predict(best_model,newdata=data[i+28,])
  if(i+28 < 32){
    data$lag1[i+29] = pred[i]
  }
}


