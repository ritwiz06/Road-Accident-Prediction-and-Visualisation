library("dplyr")
library(ggplot2)
setwd("D:\\SEM5\\Foundation of data analytics\\J comp\\GitHub\\Road-Accident-Prediction-and-Visualisation\\Datasets")

# Accidents due to vehicular defects
road_c2 = read.csv("Road_condition.csv")
road_c2 = road_c2[1:36,2:98]
road_c2 = road_c2 %>% arrange(State..UT)
road_c2 = road_c2[-c(9,6,10,8),]
View(road_c2)
sum(is.na(road_c2))
dim(road_c2)


rownames(road_c2) <- road_c2$State..UT

states = seq(1:32)
road_c2$Total_2014 = 0
road_c2$Total_2016 = 0
tot_acc_14 = seq(2,73,3)
tot_acc_16 = seq(74,97,3)
tot_kil_14 = seq(3,73,3)
tot_kil_16 = seq(75,97,3)
tot_inj_14 = seq(4,73,3)
tot_inj_16 = seq(76,97,3)
for (i in states)
{
  road_c2$Total_2014_Acc[i] = sum(road_c2[i,tot_acc_14])
  road_c2$Total_2016_Acc[i] = sum(road_c2[i,tot_acc_16])
  road_c2$Total_2014_Kil[i] = sum(road_c2[i,tot_kil_14])
  road_c2$Total_2016_Kil[i] = sum(road_c2[i,tot_kil_16])
  road_c2$Total_2014_Inj[i] = sum(road_c2[i,tot_inj_14])
  road_c2$Total_2016_Inj[i] = sum(road_c2[i,tot_inj_16])
}

par(mar = c(10,5,5,5), bg="#CDCDCD")
barplot(road_c2$Total_2014_Acc, main = "Total accidents in 2014 due to Vehicular defects" ,ylab="accidents",names.arg = road_c2$State..UT,las=2,col = rainbow(150),ylim = c(0,340000))

barplot(road_c2$Total_2016_Acc, main = "Total accidents in 2016 due to Vehicular defects" ,ylab="accidents",names.arg = road_c2$State..UT,las=2,col = rainbow(150),ylim = c(0,75000))

barplot(road_c2$Total_2014_Kil, main = "Total Persons Killed in 2014 due to Vehicular defects" ,ylab="accidents",names.arg = road_c2$State..UT,las=2,col = rainbow(150),ylim = c(0,90000))

barplot(road_c2$Total_2016_Kil, main = "Total Persons Killed in 2016 due to Vehicular defects" ,ylab="accidents",names.arg = road_c2$State..UT,las=2,col = rainbow(150),ylim = c(0,25000))

barplot(road_c2$Total_2014_Inj, main = "Total Persons Injured in 2014 due to Vehicular defects" ,ylab="accidents",names.arg = road_c2$State..UT,las=2,col = rainbow(150),ylim = c(0,350000))

barplot(road_c2$Total_2016_Inj, main = "Total Persons Injured in 2016 due to Vehicular defects" ,ylab="accidents",names.arg = road_c2$State..UT,las=2,col = rainbow(150),ylim = c(0,80000))