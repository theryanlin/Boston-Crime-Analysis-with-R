##import data
boston<-read.csv("C:\\Users\\RYUN\\Dropbox\\My PC (LAPTOP-K1HP3M4G)\\Desktop\\Boston\\crime.csv")
#2019
boston1 <- read.csv("C:\\Users\\RYUN\\Dropbox\\My PC (LAPTOP-K1HP3M4G)\\Desktop\\2019.csv")
#2020
boston2 <- read.csv("C:\\Users\\RYUN\\Dropbox\\My PC (LAPTOP-K1HP3M4G)\\Desktop\\2020.csv")

#Remove duplicates
boston <- unique(boston)
boston1 <- unique(boston1)
boston2 <- unique(boston2)

#split time data
library(tidyverse) 
boston <- separate(data = boston, col = OCCURRED_ON_DATE, into = c("Date", "Time"), sep = " ")
boston1 <- separate(data = boston1, col = OCCURRED_ON_DATE, into = c("Date", "Time"), sep = " ")
boston2 <- separate(data = boston2, col = OCCURRED_ON_DATE, into = c("Date", "Time"), sep = " ")

#export
write.table(boston,"boston.csv",row.names=FALSE,col.names=TRUE,sep=",")
write.table(boston1,"boston1.csv",row.names=FALSE,col.names=TRUE,sep=",")
write.table(boston2,"boston2.csv",row.names=FALSE,col.names=TRUE,sep=",")

