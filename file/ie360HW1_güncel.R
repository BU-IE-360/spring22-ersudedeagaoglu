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


setwd("C:/Users/Erkut/Desktop/ie 360")

mb <- read_excel("EVDS-.xlsx")
mb <- mb[c(1:48),]
summary(mb)
str(mb)
mb$`TP KKHARTUT KT10` <- as.numeric(mb$`TP KKHARTUT KT10`)
mb$`TP DK USD A YTL` <- as.numeric(mb$`TP DK USD A YTL`)
mb$Tarih=ym(mb$Tarih)
date <- mb$Tarih
air_spending <- mb$`TP KKHARTUT KT10`
usdrate <- mb$`TP DK USD A YTL`
trPI <- mb$`TP FG J07`

ticket <- read.csv("ticketsearch_.csv")
ticket <- ticket[2:49,]
ticket <- as.numeric(ticket)


df <- data.frame("Date" = date, "Year" = format(date, "%Y"),
                 "Month" = format(date, "%m"), "Usd_rate" = usdrate, 
                 "airline_spending" = air_spending,
                 "Transportation_PI" = trPI, "Ticket_search" = ticket)

str(df)

spending_df <- df[,c(1,2,3,5)]
exchange_df <- df[,c(1,2,3,4)]
tPI_df <- df[,c(1,2,3,6)]
tsearch_df <- df[,c(1,2,3,7)]


## histograms
ggplot(spending_df,aes(x=Month,y=airline_spending))+ geom_bar(stat='identity')+ 
  facet_wrap(~Year)+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  ylab("Spendings")+ ggtitle("Airline spendings in Turkey from 2016 June to 2018 June")

ggplot(exchange_df,aes(x=Month,y=Usd_rate))+ geom_bar(stat='identity')+ 
  facet_wrap(~Year)+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  ylab("USD Exchange Rates")+ ggtitle("Dolar / TL from 2016 June to 2018 June")

ggplot(tPI_df,aes(x=Month,y=Transportation_PI))+ geom_bar(stat='identity')+ 
  facet_wrap(~Year)+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  ylab("Price Index")+ ggtitle("Transportation Price index in Turkey(base = 2003) from 2016 June to 2018 June")

ggplot(tsearch_df,aes(x=Month,y=Ticket_search))+ geom_bar(stat='identity')+ 
  facet_wrap(~Year)+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  ylab("Number of Search")+ ggtitle("Number of search for -Uçak Bileti- in Turkey from 2016 June to 2018 June")


### boxplots

ggplot(spending_df,aes(x=factor(Year),y=airline_spending))+ geom_boxplot(aes(fill=factor(Year)))+ 
  xlab("Years") + ylab("spendings")+ ggtitle("Airline Spendings")+
  theme(legend.position = "none")

ggplot(exchange_df,aes(x=factor(Year),y=Usd_rate))+ geom_boxplot(aes(fill=factor(Year)))+ 
  xlab("Years") + ylab("Dolar / TL ")+ ggtitle("Dolar/TL Exchange Rate")+
  theme(legend.position = "none")

ggplot(tPI_df,aes(x=factor(Year),y=Transportation_PI))+ geom_boxplot(aes(fill=factor(Year)))+ 
  xlab("Years") + ylab("Price Index")+ ggtitle("Transportation Price Index")+
  theme(legend.position = "none")

ggplot(tsearch_df,aes(x=factor(Year),y=Ticket_search))+ geom_boxplot(aes(fill=factor(Year)))+ 
  xlab("Years") + ylab("Number of search")+ ggtitle("Number of search for -Uçak Bileti-")+
  theme(legend.position = "none")

### lines

ggplot(spending_df,aes(x=Date))+
  geom_line(size=1,color="red",aes(y=airline_spending))+
  geom_smooth(color='blue',linetype='dashed',size=1,fill=NA,aes(y=airline_spending))
ggtitle("Airline spendings in Turkey",
        subtitle="Between 2016-06 and 2018-06")

ggplot(exchange_df,aes(x=Date))+
  geom_line(size=1,color="purple",aes(y=Usd_rate))+
  geom_smooth(color='black',linetype='dashed',size=1,fill=NA,aes(y=Usd_rate))
ggtitle("Exchange Rates(USD/TL)",
        subtitle="Between 2016-06 and 2018-06")

ggplot(tPI_df,aes(x=Date))+
  geom_line(size=1,color="blue",aes(y=Transportation_PI))+
  geom_smooth(color='red',linetype='dashed',size=1,fill=NA,aes(y=Transportation_PI))
ggtitle("Transportatin PI (base = 2003) in Turkey",
        subtitle="Between 2016-06 and 2018-06")

ggplot(tsearch_df,aes(x=Date))+
  geom_line(size=1,color="black",aes(y=Ticket_search))+
  geom_smooth(color='orange',linetype='dashed',size=1,fill=NA,aes(y=Ticket_search))
ggtitle("Airline spendings in Turkey",
        subtitle="Between 2016-06 and 2018-06")


#### correlation check  
corr = cor(df[,c(4,5,6,7)])

ggcorrplot(corr,
           hc.order = TRUE,
           type='lower',
           lab=TRUE)

pairs(df[,c(4,5,6,7)])



