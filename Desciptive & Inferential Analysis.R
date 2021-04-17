#####################################################
#Descriptive Analysis 

#####################################################
#import data
df <- read.csv("C:/Users/Asus/Desktop/FINALDATA2.csv") ##getwd() and setwd() shouldve worked but i kept getting the wrong directory
df
#####################################################

#Crime number time series
## Create dataframe of dates and number of crimes
df_ts <- data.frame(unique(df$"Date"))
df_ts$count <- NA
for(i in 1:nrow(df_ts)){
  df_ts$count[i] <- sum(df$Date==df_ts$unique.df.Date.[i])
}
head(df_ts)

## Check type of data
str(df_ts) ### Date is in char format, need to be changed to date time

## Changing to datetime
library(lubridate)
df_ts$unique.df.Date. <- dmy(df_ts$unique.df.Date.)
str(df_ts)

## sort the dates
df_ts <- df_ts[order(as.Date(df_ts$unique.df.Date., format="%d/%m/%Y")),]
head(df_ts)
colnames(df_ts) <- c("Date","Count")
write.csv(df_ts,"timeseries-final.csv")

## Plot
### Import the csv
df_ts <- read.csv("timeseries-final.csv")
df_ts$Date <- ymd(df_ts$Date)

### Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

### Usual area chart
p <- df_ts %>%
  ggplot( aes(x=Date, y=Count)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("Daily Reported Crimes") +
  xlab("Date")+
  theme_ipsum()

### Turn it interactive with ggplotly
p <- ggplotly(p)
p

####Anomaly
df
df_anomaly <- subset(df,df$Date=="31/12/2020")
df_anomaly <- subset(df_anomaly,select=c("OFFENSE_BROAD_CAT"))
df_anomaly_count <- data.frame(Category=c("Monetary","Drugs / Alcohol Abuse","Sex Offense",
                                          "Violence / Risk of Safety","Weapons/Firearms",
                                          "Motor Vehicle Accident / Violation","Other"))
df_anomaly_count$Count <- NA
for(i in 1:nrow(df_anomaly_count)){
  df_anomaly_count$Count[i] <- sum(df_anomaly$OFFENSE_BROAD_CAT==df_anomaly_count$Category[i])
}
df_anomaly_count
p_anom <- ggplot(df_anomaly_count, aes(x=Category,y=Shootings)) +
  geom_bar(stat='identity',color="blue", fill=rgb(0.1,0.4,0.5,0.7))+
  ylab("Total Shootings")
p_anom
###############################################################
# Total yearly crime numbers
## Creation of dataset
df_yearly <- data.frame(2016:2020)
colnames(df_yearly) <- "Year"
df_yearly$Count <- NA
for(i in 1:nrow(df_yearly)){
  df_yearly$Count[i] <- sum(df$YEAR==df_yearly$Year[i])
}
df_yearly
write.csv(df_yearly,"YearlyBarchart.csv")
## Plot
df_yearly <- read.csv("YearlyBarchart.csv")
p2 <-ggplot(df_yearly, aes(x=Year,y=Count)) +
  geom_bar(stat='identity',color="blue", fill=rgb(0.1,0.4,0.5,0.7))+
  ylab("Total Reported Crimes")+
  geom_text(aes(label = Count), vjust = -0.2)
p2

##################################################################
# Total yearly crime numbers, by category
## Creation of dataset
df_yearly_cat <- data.frame(Year=rep(c(2016,2017,2018,2019,2020),7))
df_yearly_cat$Cat <- c(rep("Monetary",5),rep("Motor Vehicle Accident / Violation",5),
                       rep("Sex Offence",5),rep("Violence / Risk of Safety",5),
                       rep("Drugs / Alcohol Abuse",5),rep("Weapons/Firearms",5),rep("Other",5))
df_yearly_cat$Count <- NA
for(i in 1:nrow(df_yearly_cat)){
  df_yearly_cat$Count[i] <- sum(df$OFFENSE_BROAD_CAT==df_yearly_cat$Cat[i]&
                                  df$YEAR==df_yearly_cat$Year[i])
}
write.csv(df_yearly_cat,"CategoricalYearly.csv")
## Plot
p3 <- ggplot(df_yearly_cat,aes(fill=Cat,y=Count,x=Year))+
  geom_bar(position='dodge',stat='identity')+
  ylab("Total Reported Crimes")
p3
### Without other
df_yearly_cat2 <- slice(df_yearly_cat, -(31:35))
p3b <- ggplot(df_yearly_cat2,aes(fill=Cat,y=Count,x=Year))+
  geom_bar(position='dodge',stat='identity')+
  ylab("Total Reported Crimes")
p3b

#####################################################################
#Shooting per year
## Creation of dataset
df_shoot <- data.frame(Year=c(2016,2017,2018,2019,2020))
df_shoot$Shootings <- NA
for(i in 1:nrow(df_shoot)){
  df_shoot$Shootings[i] <- sum(df$SHOOTING==1&df$YEAR==df_shoot$Year[i])
}

df_shoot
write.csv(df_shoot,"ShootingCount.csv")
## Plot
p4 <-ggplot(df_shoot, aes(x=Year,y=Shootings)) +
  geom_bar(stat='identity',color="blue", fill=rgb(0.1,0.4,0.5,0.7))+
  ylab("Total Shootings")+
  geom_text(aes(label = Shootings),vjust = -0.2)
p4

#########################################################################
#Shooting By Category, yearly
## Creation of dataset
df_shoot_cat_yearly <- data.frame(Category=rep(c("Monetary","Motor Vehicle Accident / Violation",
                                                 "Sex Offence","Violence / Risk of Safety",
                                                 "Drugs / Alcohol Abuse","Weapons/Firearms","Other"),5))
df_shoot_cat_yearly$Year <- c(rep(2016,7),rep(2017,7),rep(2018,7),rep(2019,7),rep(2020,7))
df_shoot_cat_yearly$Shooting <- NA
for(i in 1:nrow(df_shoot_cat_yearly)){
  df_shoot_cat_yearly$Shooting[i] <- sum(df$SHOOTING==1&df$YEAR==df_shoot_cat_yearly$Year[i]&
                                           df$OFFENSE_BROAD_CAT==df_shoot_cat_yearly$Category[i])
}
df_shoot_cat_yearly
write.csv(df_shoot_cat_yearly,"YearlyCategorizedShooting.csv")
## Plot
p5 <- ggplot(df_shoot_cat_yearly,aes(fill=Category,y=Shooting,x=Year))+
  geom_bar(position='dodge',stat='identity')+
  ylab("Total Shootings")
p5

#########################################################################
#Shooting Association Total
## Creation of dataset
df_shoot_assoc <- data.frame(Category=c("Monetary","Motor Vehicle Accident / Violation",
                                        "Sex Offence","Violence / Risk of Safety",
                                        "Drugs / Alcohol Abuse","Weapons/Firearms","Other"))
df_shoot_assoc$ShootingRate <- NA
for(i in 1:nrow(df_shoot_assoc)){
  df_shoot_assoc$ShootingRate[i] <- (sum(df$SHOOTING==1&
                                           df$OFFENSE_BROAD_CAT==df_shoot_assoc$Category[i])/
                                       sum(df$OFFENSE_BROAD_CAT==df_shoot_assoc$Category[i]))
}
df_shoot_assoc$ShootingRate <- round(df_shoot_assoc$ShootingRate,digits = 6)
## Rename for simplicity
df_shoot_assoc$Category <- c("Monetary","Motor Vehicle","Sex Offence","Violence",
                             "Drugs/Alcohol","Firearms","Other")
df_shoot_assoc$ShootingRate <- df_shoot_assoc$ShootingRate*100
df_shoot_assoc
write.csv(df_shoot_assoc,"CategoricalShootingRate.csv")
## Plot
p6 <-ggplot(df_shoot_assoc, aes(x=Category,y=ShootingRate)) +
  geom_bar(stat='identity',color="blue", fill=rgb(0.1,0.4,0.5,0.7))+
  ylab("Rate of Shootings")+
  geom_text(aes(label = ShootingRate),vjust = -0.2)
p6

###########################################################################
# Yearly Category of Crime Proportion
## 2016
### Creation of dataset
df_year_comp_2016 <- data.frame(Category=c("Monetary","Motor Vehicle Accident / Violation",
                                           "Sex Offence","Violence / Risk of Safety",
                                           "Drugs / Alcohol Abuse","Weapons/Firearms","Other"))
df_year_comp_2016$Count <- NA
for(i in 1:nrow(df_year_comp_2016)){
  df_year_comp_2016$Count[i] <- sum(df$OFFENSE_BROAD_CAT==df_year_comp_2016$Category[i]&df$YEAR==2016)
}
df_year_comp_2016$Percentage <- NA
for(i in 1:nrow(df_year_comp_2016)){
  df_year_comp_2016$Percentage[i] <- (df_year_comp_2016$Count[i]/sum(df_year_comp_2016$Count))*100
}
df_year_comp_2016$Percentage <- round(df_year_comp_2016$Percentage,digits = 2)
df_year_comp_2016$Year <- c(rep(2016,7))

#### Rename for simplicity
df_year_comp_2016$Category <- c("Monetary","Motor Vehicle","Sex Offence",
                                "Violence","Drugs/Alcohol","Firearms","Other")
df_year_comp_2016

### Plot
p7_2016 <- ggplot(df_year_comp_2016, aes(x = Category, y = Percentage)) +
  geom_col() +
  geom_text(aes(label = paste(Percentage,"%",sep="")), vjust = -0.2)+
  ggtitle("Percentage of Categories, 2016")
p7_2016

## 2017
### Creation of dataset
df_year_comp_2017 <- data.frame(Category=c("Monetary","Motor Vehicle Accident / Violation",
                                           "Sex Offence","Violence / Risk of Safety",
                                           "Drugs / Alcohol Abuse","Weapons/Firearms","Other"))
df_year_comp_2017$Count <- NA
for(i in 1:nrow(df_year_comp_2017)){
  df_year_comp_2017$Count[i] <- sum(df$OFFENSE_BROAD_CAT==df_year_comp_2017$Category[i]&df$YEAR==2017)
}
df_year_comp_2017$Percentage <- NA
for(i in 1:nrow(df_year_comp_2017)){
  df_year_comp_2017$Percentage[i] <- (df_year_comp_2017$Count[i]/sum(df_year_comp_2017$Count))*100
}
df_year_comp_2017$Percentage <- round(df_year_comp_2017$Percentage,digits = 2)
df_year_comp_2017$Year <- c(rep(2017,7))

#### Rename for simplicity
df_year_comp_2017$Category <- c("Monetary","Motor Vehicle","Sex Offence","Violence","Drugs/Alcohol","Firearms","Other")
df_year_comp_2017

### Plot
p7_2017 <- ggplot(df_year_comp_2017, aes(x = Category, y = Percentage)) +
  geom_col() +
  geom_text(aes(label = paste(Percentage,"%",sep="")), vjust = -0.2)+
  ggtitle("Percentage of Categories, 2017")
p7_2017

## 2018
### Creation of dataset
df_year_comp_2018 <- data.frame(Category=c("Monetary","Motor Vehicle Accident / Violation",
                                           "Sex Offence","Violence / Risk of Safety",
                                           "Drugs / Alcohol Abuse","Weapons/Firearms","Other"))
df_year_comp_2018$Count <- NA
for(i in 1:nrow(df_year_comp_2018)){
  df_year_comp_2018$Count[i] <- sum(df$OFFENSE_BROAD_CAT==df_year_comp_2018$Category[i]&df$YEAR==2018)
}
df_year_comp_2018$Percentage <- NA
for(i in 1:nrow(df_year_comp_2018)){
  df_year_comp_2018$Percentage[i] <- (df_year_comp_2018$Count[i]/sum(df_year_comp_2018$Count))*100
}
df_year_comp_2018$Percentage <- round(df_year_comp_2018$Percentage,digits = 2)
df_year_comp_2018$Year <- c(rep(2018,7))

#### Rename for simplicity
df_year_comp_2018$Category <- c("Monetary","Motor Vehicle","Sex Offence","Violence","Drugs/Alcohol","Firearms","Other")
df_year_comp_2018

### Plot
p7_2018 <- ggplot(df_year_comp_2018, aes(x = Category, y = Percentage)) +
  geom_col() +
  geom_text(aes(label = paste(Percentage,"%",sep="")), vjust = -0.2)+
  ggtitle("Percentage of Categories, 2018")
p7_2018

## 2019
### Creation of dataset
df_year_comp_2019 <- data.frame(Category=c("Monetary","Motor Vehicle Accident / Violation",
                                           "Sex Offence","Violence / Risk of Safety",
                                           "Drugs / Alcohol Abuse","Weapons/Firearms","Other"))
df_year_comp_2019$Count <- NA
for(i in 1:nrow(df_year_comp_2019)){
  df_year_comp_2019$Count[i] <- sum(df$OFFENSE_BROAD_CAT==df_year_comp_2019$Category[i]&df$YEAR==2019)
}
df_year_comp_2019$Percentage <- NA
for(i in 1:nrow(df_year_comp_2019)){
  df_year_comp_2019$Percentage[i] <- (df_year_comp_2019$Count[i]/sum(df_year_comp_2019$Count))*100
}
df_year_comp_2019$Percentage <- round(df_year_comp_2019$Percentage,digits = 2)
df_year_comp_2019$Year <- c(rep(2019,7))

#### Rename for simplicity
df_year_comp_2019$Category <- c("Monetary","Motor Vehicle","Sex Offence","Violence","Drugs/Alcohol","Firearms","Other")
df_year_comp_2019

### Plot
p7_2019 <- ggplot(df_year_comp_2019, aes(x = Category, y = Percentage)) +
  geom_col() +
  geom_text(aes(label = paste(Percentage,"%",sep="")), vjust = -0.2)+
  ggtitle("Percentage of Categories, 2019")
p7_2019

## 2020
### Creation of dataset
df_year_comp_2020 <- data.frame(Category=c("Monetary","Motor Vehicle Accident / Violation",
                                           "Sex Offence","Violence / Risk of Safety",
                                           "Drugs / Alcohol Abuse","Weapons/Firearms","Other"))
df_year_comp_2020$Count <- NA
for(i in 1:nrow(df_year_comp_2020)){
  df_year_comp_2020$Count[i] <- sum(df$OFFENSE_BROAD_CAT==df_year_comp_2020$Category[i]&df$YEAR==2020)
}
df_year_comp_2020$Percentage <- NA
for(i in 1:nrow(df_year_comp_2020)){
  df_year_comp_2020$Percentage[i] <- (df_year_comp_2020$Count[i]/sum(df_year_comp_2020$Count))*100
}
df_year_comp_2020$Percentage <- round(df_year_comp_2020$Percentage,digits = 2)
df_year_comp_2020$Year <- c(rep(2020,7))

#### Rename for simplicity
df_year_comp_2020$Category <- c("Monetary","Motor Vehicle","Sex Offence","Violence","Drugs/Alcohol","Firearms","Other")
df_year_comp_2020

### Plot
p7_2020 <- ggplot(df_year_comp_2020, aes(x = Category, y = Percentage)) +
  geom_col() +
  geom_text(aes(label = paste(Percentage,"%",sep="")), vjust = -0.2)+
  ggtitle("Percentage of Categories, 2020")
p7_2020

###########################################################################
###Inferential Analysis

###Read package
library("GGally")
library(ggplot2)
library(dplyr)

###Import file
df_by_year <- read.table("Inferential_Analysis.csv", header=TRUE, sep=",")
df_by_month <- read.table("Data.csv", header=TRUE, sep=",")
df_by_month$Date <- as.Date(df_by_month$Date, '%Y/%m/%d')

before_covid <- subset(df_by_month, Number_Covid='NA')
after_covid <- subset(df_by_month, Number_Covid!='NA')

## Correlation heatamp
ggcorr(data=df_by_year, palette = "RdYlGn", label = TRUE, 
       hjust = .85, size = 3, label_color = "black")

## Rate_Unemployment and No_Shooting have the strong correlation

## Correlation test
cor.test(df_by_year$No_Shooting, df_by_year$Rate_Unemployment, 
         method=c("pearson"))

## Regression
scatter.smooth(x=after_covid$Rate_Unemployment , y=after_covid$Number_Shooting, 
               main="Number_Shooting ~ Rate_Unemployment")
scatter.smooth(x=df_by_month$Rate_Unemployment , y=df_by_month$Number_Shooting, 
               main="Number_Shooting ~ Rate_Unemployment")

a = lm(Number_Shooting ~ Rate_Unemployment, data = df_by_month,)
summary(a)

## T test, check covid influences Rate_Unemployment or not
before_covid <- subset(df_by_month, Number_Covid='NA')
after_covid <- subset(df_by_month, Number_Covid!='NA')

a = t.test(before_covid$Number_Shooting, after_covid$Number_Shooting, 
       "two.sided", var.equal=TRUE, conf.level=0.95)	

b = t.test(before_covid$Sex.Offence, after_covid$Sex.Offence, 
       "two.sided", var.equal=TRUE, conf.level=0.95)	

c = t.test(before_covid$Drugs...Alcohol.Abuse, after_covid$Drugs...Alcohol.Abuse, 
           "two.sided", var.equal=TRUE, conf.level=0.95)	

d = t.test(before_covid$Motor.Vehicle.Accident...Violation, after_covid$Motor.Vehicle.Accident...Violation, 
       "two.sided", var.equal=TRUE, conf.level=0.95)	

e = t.test(before_covid$Monetary, after_covid$Monetary, 
       "two.sided", var.equal=TRUE, conf.level=0.95)	

f = t.test(before_covid$Violence...Risk.of.Safety, after_covid$Violence...Risk.of.Safety, 
       "two.sided", var.equal=TRUE, conf.level=0.95)	

g = t.test(before_covid$Weapons.Firearms, after_covid$Weapons.Firearms, 
       "two.sided", var.equal=TRUE, conf.level=0.95)	

h = t.test(before_covid$Total, after_covid$Total, 
           "two.sided", var.equal=TRUE, conf.level=0.95)


c(a$estimate , b$estimate , c$estimate , d$estimate , e$estimate , f$estimate , g$estimate , h$estimate )

## t test, check covid influences Rate_Unemployment or not

df_by_month$Date <- as.Date(df_by_month$Date, '%Y/%m/%d')

before_covid <- subset(df_by_month, Number_Covid='NA')
after_covid <- subset(df_by_month, Number_Covid!='NA')

plot(df_by_month$Date, df_by_month$Number_Shooting, pch=19, type="o", col="black", xlab="Date", ylab="Number") 
abline(h=c(mean(before_covid$Number_Shooting), mean(after_covid$Number_Shooting)), col=c("red", "blue"), lty=c(1,2))  
legend("topleft", legend=c("Mean before Covid","Mean After Covid"), col=c("red", "blue"), lty=1:2, cex=0.8) 
title(main="Number of Number_Shooting")

t.test(before_covid$Number_Shooting, after_covid$Number_Shooting, "two.sided", var.equal=TRUE, conf.level=0.95)	

############################################3
##Time Series

install.packages("forecast")
library(ggplot2)
library(forecast)
library(dplyr)
library(lubridate)
library(readr)
library(Metrics)
install.packages("Metrics")

#import data
Time<-read.csv("C:\\Users\\RYUN\\Dropbox\\My PC (LAPTOP-K1HP3M4G)\\Desktop\\Time.csv")

#splitting data into train and valid sets
train = Time[1:50,]
valid = Time[51:nrow(Time),]

#removing "Month" column
train$Month = NULL

#training model
model <- auto.arima(train)

#model summary
summary(model)

#forecast
model.fore <- forecast(model, h = 6)
model.fore
plot(model.fore)


