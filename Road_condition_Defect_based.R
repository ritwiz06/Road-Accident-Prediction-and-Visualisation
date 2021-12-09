library("dplyr")
#install.packages('purrr')
#install.packages("janitor")
library(janitor)
library('purrr')
library(ggplot2)
library(tidyverse)

library("reshape2") 
setwd("D:\\SEM5\\Foundation of data analytics\\J comp\\GitHub\\Road-Accident-Prediction-and-Visualisation\\Datasets")
road_condition = read.csv("Road_condition.csv")
road_condition =  road_condition[1:36,2:98]
road_condition = road_condition %>% arrange(State..UT)
road_condition = road_condition[-c(9,6,10,8),]
#View(road_condition)
sum(is.na(road_condition))
dim(road_condition)
colnames(road_condition)
#Selection based on Accident, Killed and Injured
#Killed
rck1<- road_condition%>%dplyr:: select (matches('State..UT|Killed'))
#2014
rck14<- road_condition%>%dplyr:: select (matches('State..UT|2014'))
rck114<- rck14%>%dplyr:: select (matches('State..UT|Killed'))
#View(rck114)
dlrck114 <- melt(rck114, id = "State..UT")
Scatplrck14 <- ggplot(dlrck114,aes(x = State..UT,y = value,color = variable , group = 1)) +  geom_point(size=2) +theme_set(theme_minimal())+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("State Wise No. of People Killed in Road Accidents Based on road Condition in year 2014")
Scatplrck14
#2016
rck16<- road_condition%>%dplyr:: select (matches('State..UT|2016'))
rck116<- rck16%>%dplyr:: select (matches('State..UT|Killed'))
#View(rck116)
dlrck116 <- melt(rck116, id = "State..UT")
Scatplrck16 <- ggplot(dlrck116,aes(x = State..UT,y = value,color = variable , group = 1)) +  geom_point(size=2) +theme_set(theme_minimal())+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("State Wise No. of People Killed in Road Accidents Based on road Condition in year 2016")
Scatplrck16
#Accident 
rca1<- road_condition%>%dplyr:: select (matches('State..UT|Accident'))
#2014
rca14<- road_condition%>%dplyr:: select (matches('State..UT|2014'))
rca114<- rca14%>%dplyr:: select (matches('State..UT|Accident'))
#View(rca114)
dlrca114 <- melt(rca114, id = "State..UT")
Scatplrca14 <- ggplot(dlrca114,aes(x = State..UT,y = value,color = variable , group = 1)) +  geom_point(size=2) +theme_set(theme_minimal())+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("State Wise No. of Accidents Based on road Condition in year 2014")
Scatplrca14
#2016
rca16<- road_condition%>%dplyr:: select (matches('State..UT|2016'))
rca116<- rca16%>%dplyr:: select (matches('State..UT|Accident'))
#View(rca116)
dlrca116 <- melt(rca116, id = "State..UT")
Scatplrca16 <- ggplot(dlrca116,aes(x = State..UT,y = value,color = variable , group = 1)) +  geom_point(size=2) +theme_set(theme_minimal())+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("State Wise No. of Accidents Based on road Condition in year 2016")
Scatplrca16
#injured
rci1<- road_condition%>%dplyr:: select (matches('State..UT|Injured'))
#View(rci1)
#2014
rci14<- rci1%>%dplyr:: select (matches('State..UT|2014'))
rci114<- rci14%>%dplyr:: select (matches('State..UT|Injured'))
#View(rci114)
dlrci114 <- melt(rci114, id = "State..UT")
Scatplrci14 <- ggplot(dlrci114,aes(x = State..UT,y = value,color = variable , group = 1)) +  geom_point(size=2) +theme_set(theme_minimal())+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("State Wise No. of People Injured in Road Accidents Based on road Condition in year 2014")
Scatplrci14
#2016
rci16<- rci1%>%dplyr:: select (matches('State..UT|2016'))
rci116<- rci16%>%dplyr:: select (matches('State..UT|Injured'))
#View(rci116)
dlrci116 <- melt(rci116, id = "State..UT")
Scatplrci16 <- ggplot(dlrci116,aes(x = State..UT,y = value,color = variable , group = 1)) +  geom_point(size=2) +theme_set(theme_minimal())+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("State Wise No. of People Injured in Road Accidents Based on road Condition in year 2016")
Scatplrci16



#Adding all the columns to get total in a new column
rca1<- rca1%>% mutate(Total_Accidents = rca1%>%select(contains('Accident'))%>%rowSums)
#View(rca1)
rck1<- rck1%>% mutate(Total_Killed = rck1%>%select(contains('Killed'))%>%rowSums)
#View(rck1)
rci1<- rci1%>% mutate(Total_Injured = rci1%>%select(contains('Injured'))%>%rowSums)
#View(rci1)

##Accidents
#Col total
rca1<- rca1%>% adorn_totals("row")
#View(rca1)
rcat<-t(rca1)
#View(rcat)
#transpose and changing col names
my.names <- rcat[1,]

colnames(rcat) <- my.names
rcat<-rcat[-1,]
#View(rcat)

rcat <- cbind(Road_Condition = rownames(rcat), rcat)
rownames(rcat) <- NULL
#View(rcat)
#ggplot visualisation
rcat<- transform(rcat, Total = as.numeric(Total))
###sapply(rcat,mode)
rcat1<-head(rcat,-1)
#View(rcat1)
rcat1$Road_Condition<-factor(rcat1$Road_Condition, levels = rcat1$Road_Condition)
pa1<-ggplot(data=rcat1, aes(x=Road_Condition, y=Total))+ geom_bar(stat="identity", fill="blue")+ggtitle("Number of Accidents based on Road Condition")+scale_y_continuous(labels = scales::comma)+theme_minimal()+theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

pa1

##Killed
#Col total
rck1<- rck1%>% adorn_totals("row")
#View(rck1)
rckt<-t(rck1)
#View(rckt)
#transpose and changing col names
my.namesrck <- rckt[1,]

colnames(rckt) <- my.namesrck
rckt<-rckt[-1,]
#View(rckt)

rckt <- cbind(Road_Condition = rownames(rckt), rckt)
rownames(rckt) <- NULL
#View(rckt)
#ggplot visualisation
rckt<- transform(rckt, Total = as.numeric(Total))
###sapply(rcat,mode)
rckt1<-head(rckt,-1)
#View(rckt1)
rckt1$Road_Condition<-factor(rckt1$Road_Condition, levels = rckt1$Road_Condition)
pk1<-ggplot(data=rckt1, aes(x=Road_Condition, y=Total))+ geom_bar(stat="identity", fill="blue")+ggtitle("Number of People Killed in Road Accidents based on Road Condition")+scale_y_continuous(labels = scales::comma)+theme_minimal()+theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

pk1

##Killed
#Col total
rci1<- rci1%>% adorn_totals("row")
#View(rci1)
rcit<-t(rci1)
#View(rcit)
#transpose and changing col names
my.namesrci <- rcit[1,]

colnames(rcit) <- my.namesrci
rcit<-rcit[-1,]
#View(rcit)

rcit <- cbind(Road_Condition = rownames(rcit), rcit)
rownames(rcit) <- NULL
#View(rcit)
#ggplot visualisation
rcit<- transform(rcit, Total = as.numeric(Total))
###sapply(rcat,mode)
rcit1<-head(rcit,-1)
#View(rcit1)
rcit1$Road_Condition<-factor(rcit1$Road_Condition, levels = rcit1$Road_Condition)
pi1<-ggplot(data=rcit1, aes(x=Road_Condition, y=Total))+ geom_bar(stat="identity", fill="blue")+ggtitle("Number of People Injured in Road Accidents based on Road Condition")+ scale_y_continuous(labels = scales::comma)+theme_minimal()+theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

pi1
