###############################################
############## CWT Reconstruction #############
###############################################
library(dplyr)
library(tidyr)
library(ggplot2)
options(scipen=999)
setwd("C:/Users/echen/Box/Sacramento River Fall Chinook/Methodology Review")
#############################################
#####        Release data           #########
#############################################
CWT_Releases<-read.csv("CWTReleased.csv")
Recruits<-CWT_Releases %>%
  filter(hatchery_location_name == "COLEMAN NFH") %>%
  group_by(brood_year) %>%
  summarise(Individuals_Released = sum(Total_Released))
#############################################
#####        Recovery data           #########
#############################################
Cohort<-readRDS("CWT Spawning Grounds.Rds") #Escapement by Year and Age using CWT expansions, for 1000 iterations
To_Hatchery<-read.csv("CWT Hatchery.csv") #Hatchery Escapement
River_Harvest<-readRDS("CWT River Harvest.Rds")
Fishing_Impact<-readRDS("Impact CWT Bootstrap.Rds")
test1<-Cohort[[1]]
Fishing_Harvest<-readRDS("Harvest CWT Bootstrap.Rds")
#selecting years for fishing harvest (not done for specific hatcheries)
for(i in 1:1000){
  Fishing_Harvest[[i]]<-Cohort[[i]] %>% 
    left_join(Fishing_Harvest[[i]], by = "brood_year")
  Cohort[[i]][is.na(Cohort[[i]])] <- 0
}
#Combining escapement info for each brood year
for(i in 1:1000){
  Cohort[[i]]<-Cohort[[i]] %>% 
    left_join(Recruits, by = "brood_year") %>%#merging with recruits
    # mutate(Age1Sp = 0) %>% #For CNFH and NFH
    select(brood_year, Individuals_Released,Age1Sp,Age2Sp,Age3Sp,Age4Sp,Age5Sp) %>% 
    left_join(To_Hatchery, by="brood_year") %>%
    left_join(River_Harvest[[i]],by="brood_year") %>%
    left_join(Fishing_Impact[[i]], by = "brood_year") 
  Cohort[[i]][is.na(Cohort[[i]])] <- 0
}
###############################################
########## *Let the fun begin * ###############
###############################################
#Age.Start of Month (includes fish that will die naturally that month 
#but not fished)
for (i in 1:1000){
  # 7 Year Old Fishing
  Cohort[[i]]$Age7.8<-Cohort[[i]]$Aug7
  Cohort[[i]]$Age7.7<-Cohort[[i]]$Age7.8/(1-0.0184)+Cohort[[i]]$Jul7
  Cohort[[i]]$Age7.6<-Cohort[[i]]$Age7.7/(1-0.0184)+Cohort[[i]]$Jun7
  Cohort[[i]]$Age7.5<-Cohort[[i]]$Age7.6/(1-0.0184) +Cohort[[i]]$May7 
  # 6 Year Old Fishing
  Cohort[[i]]$Age6.9<-Cohort[[i]]$Age7.5/(1-0.1382) #86% survival for 8 months
  Cohort[[i]]$Age6.8<-Cohort[[i]]$Age6.9/(1-0.0184)+Cohort[[i]]$Aug6 
  Cohort[[i]]$Age6.7<-Cohort[[i]]$Age6.8/(1-0.0184)+Cohort[[i]]$Jul6
  Cohort[[i]]$Age6.6<-Cohort[[i]]$Age6.7/(1-0.0184)+Cohort[[i]]$Jun6

  #5 Year Old Fishing
  Cohort[[i]]$Age5.10<-Cohort[[i]]$Age6.6/(1-0.1382)+Cohort[[i]]$Oct5 #86% survival for 8 months
  # 5 Year Old Spawners
  Cohort[[i]]$Age5.9<-(Cohort[[i]]$Age5.10)/(1-0.0184)+Cohort[[i]]$Sept5+Cohort[[i]]$Age5Sp+Cohort[[i]]$Age5Hat+Cohort[[i]]$InRiver5 #Remove InRiver5 for NFH
  #Fourth Year at Sea. 5 year olds are caught
  Cohort[[i]]$Age5.8<-Cohort[[i]]$Age5.9/(1-0.0184)+Cohort[[i]]$Aug5
  Cohort[[i]]$Age5.7<-Cohort[[i]]$Age5.8/(1-0.0184)+Cohort[[i]]$Jul5
  Cohort[[i]]$Age5.6<-Cohort[[i]]$Age5.7/(1-0.0184)+Cohort[[i]]$Jun5
  Cohort[[i]]$Age5.5<-Cohort[[i]]$Age5.6/(1-0.0184)+Cohort[[i]]$May5
  Cohort[[i]]$Age5.4<-Cohort[[i]]$Age5.5/(1-0.0184)+Cohort[[i]]$Apr5
  Cohort[[i]]$Age5.3<-Cohort[[i]]$Age5.4/(1-0.0184)+Cohort[[i]]$Mar5
  Cohort[[i]]$Age5.2<-Cohort[[i]]$Age5.3/(1-0.0184)+Cohort[[i]]$Feb5 
  Cohort[[i]]$Age4.11<-Cohort[[i]]$Age5.2/(1-0.0543)+Cohort[[i]]$Nov4 #95% survival for 3 months
  Cohort[[i]]$Age4.10<-Cohort[[i]]$Age4.11/(1-0.0184)+Cohort[[i]]$Oct4
  #4 Year Old Spawners (monthly)
  Cohort[[i]]$Age4.9<-(Cohort[[i]]$Age4.10)/(1-0.0184)+Cohort[[i]]$Sept4+Cohort[[i]]$Age4Sp+Cohort[[i]]$Age4Hat+Cohort[[i]]$InRiver4
  Cohort[[i]]$Age4.8<-Cohort[[i]]$Age4.9/(1-0.0184)+Cohort[[i]]$Aug4
  Cohort[[i]]$Age4.7<-Cohort[[i]]$Age4.8/(1-0.0184)+Cohort[[i]]$Jul4
  Cohort[[i]]$Age4.6<-Cohort[[i]]$Age4.7/(1-0.0184)+Cohort[[i]]$Jun4
  Cohort[[i]]$Age4.5<-Cohort[[i]]$Age4.6/(1-0.0184)+Cohort[[i]]$May4
  Cohort[[i]]$Age4.4<-Cohort[[i]]$Age4.5/(1-0.0184)+Cohort[[i]]$Apr4
  Cohort[[i]]$Age4.3<-Cohort[[i]]$Age4.4/(1-0.0184)+Cohort[[i]]$Mar4
  Cohort[[i]]$Age4.2<-Cohort[[i]]$Age4.3/(1-0.0184)+Cohort[[i]]$Feb4 
  Cohort[[i]]$Age3.12<-Cohort[[i]]$Age4.2/(1-0.0365)+Cohort[[i]]$Dec3 #96% survival for 2 months
  Cohort[[i]]$Age3.11<-Cohort[[i]]$Age3.12/(1-0.0184)+Cohort[[i]]$Nov3
  Cohort[[i]]$Age3.10<-Cohort[[i]]$Age3.11/(1-0.0184)+Cohort[[i]]$Oct3
  
  #3 Year Old Spawners and River Harvest(monthly)
  Cohort[[i]]$Age3.9<-(Cohort[[i]]$Age3.10)/(1-0.0184)+Cohort[[i]]$Sept3+Cohort[[i]]$Age3Sp+Cohort[[i]]$Age3Hat+Cohort[[i]]$InRiver3
  #Second Year at Sea. 3 year olds are caught.(monthly) 
  Cohort[[i]]$Age3.8<-Cohort[[i]]$Age3.9/(1-0.0184)+Cohort[[i]]$Aug3
  Cohort[[i]]$Age3.7<-Cohort[[i]]$Age3.8/(1-0.0184)+Cohort[[i]]$Jul3
  Cohort[[i]]$Age3.6<-Cohort[[i]]$Age3.7/(1-0.0184)+Cohort[[i]]$Jun3
  Cohort[[i]]$Age3.5<-Cohort[[i]]$Age3.6/(1-0.0184)+Cohort[[i]]$May3
  Cohort[[i]]$Age3.4<-Cohort[[i]]$Age3.5/(1-0.0184)+Cohort[[i]]$Apr3
  Cohort[[i]]$Age3.3<-Cohort[[i]]$Age3.4/(1-0.0184)+Cohort[[i]]$Mar3
  Cohort[[i]]$Age3.2<-Cohort[[i]]$Age3.3/(1-0.0184)+Cohort[[i]]$Feb3
  Cohort[[i]]$Age3.1<-Cohort[[i]]$Age3.2/(1-0.0184)+Cohort[[i]]$Jan3 
  Cohort[[i]]$Age2.12<-Cohort[[i]]$Age3.1/(1-0.0184)+Cohort[[i]]$Dec2 
  Cohort[[i]]$Age2.11<-Cohort[[i]]$Age2.12/(1-0.0184)+Cohort[[i]]$Nov2
  Cohort[[i]]$Age2.10<-Cohort[[i]]$Age2.11/(1-0.0184)+Cohort[[i]]$Oct2
  #2 Year Old Spawners and River Harvest (monthly)
  Cohort[[i]]$Age2.9<-(Cohort[[i]]$Age2.10)/(1-0.0184)+Cohort[[i]]$Sept2+Cohort[[i]]$Age2Sp+Cohort[[i]]$Age2Hat+Cohort[[i]]$InRiver2
  Cohort[[i]]$Age2.8<-Cohort[[i]]$Age2.9/(1-0.0561)+Cohort[[i]]$Aug2
  Cohort[[i]]$Age2.7<-Cohort[[i]]$Age2.8/(1-0.0561)+Cohort[[i]]$Jul2
  Cohort[[i]]$Age2.6<-Cohort[[i]]$Age2.7/(1-0.0561)+Cohort[[i]]$Jun2
  Cohort[[i]]$Age2.5<-Cohort[[i]]$Age2.6/(1-0.0561)+Cohort[[i]]$May2
  Cohort[[i]]$Age2.4<-Cohort[[i]]$Age2.5/(1-0.0561)+Cohort[[i]]$Apr2 
  Cohort[[i]]$Age1.10<-Cohort[[i]]$Age2.4/(1-0.2929)
  Cohort[[i]]$Age1.9<-Cohort[[i]]$Age1.10/(1-0.0561)
  #Outmigration Survival
  Cohort[[i]]$Out_Survival<-Cohort[[i]]$Age1.9/Cohort[[i]]$Individuals_Released
  #Estimating Maturation
  Cohort[[i]]$Mat5<-(Cohort[[i]]$Age5Sp+Cohort[[i]]$Age5Hat+Cohort[[i]]$InRiver5)/(Cohort[[i]]$Age5Sp+Cohort[[i]]$Age5Hat+Cohort[[i]]$Age5.10/(1-0.0184)+Cohort[[i]]$InRiver5)
  Cohort[[i]]$Mat4<-(Cohort[[i]]$Age4Sp+Cohort[[i]]$Age4Hat+Cohort[[i]]$InRiver4)/(Cohort[[i]]$Age4Sp+Cohort[[i]]$Age4Hat+Cohort[[i]]$Age4.10/(1-0.0184)+Cohort[[i]]$InRiver4)
  Cohort[[i]]$Mat3<-(Cohort[[i]]$Age3Sp+Cohort[[i]]$Age3Hat+Cohort[[i]]$InRiver3)/(Cohort[[i]]$Age3Sp+Cohort[[i]]$Age3Hat+Cohort[[i]]$Age3.10/(1-0.0184)+Cohort[[i]]$InRiver3)
  Cohort[[i]]$Mat2<-(Cohort[[i]]$Age2Sp+Cohort[[i]]$Age2Hat+Cohort[[i]]$InRiver2)/(Cohort[[i]]$Age2Sp+Cohort[[i]]$Age2Hat+Cohort[[i]]$Age2.10/(1-0.0184)+Cohort[[i]]$InRiver2)
  #Estimating Impact Rate
  Cohort[[i]]$Imp5<-rowSums(Cohort[[i]][,45:54])/(Cohort[[i]]$Age4.9-Cohort[[i]]$Age4Sp-Cohort[[i]]$Age4Hat-Cohort[[i]]$InRiver4)
  Cohort[[i]]$Imp4<-rowSums(Cohort[[i]][,34:44])/(Cohort[[i]]$Age3.9-Cohort[[i]]$Age3Sp-Cohort[[i]]$Age3Hat-Cohort[[i]]$InRiver3)
  Cohort[[i]]$Imp3<-rowSums(Cohort[[i]][,22:33])/(Cohort[[i]]$Age2.9-Cohort[[i]]$Age2Sp-Cohort[[i]]$Age2Hat-Cohort[[i]]$InRiver2)
  Cohort[[i]]$Imp2<-rowSums(Cohort[[i]][,17:21])/(Cohort[[i]]$Age1.9-Cohort[[i]]$Age1Sp-Cohort[[i]]$Age1Hat)
  #Monthly impact rates
  Cohort[[i]]$Imp2_4<-Cohort[[i]]$Apr2/Cohort[[i]]$Age2.4
  Cohort[[i]]$Imp2_5<-Cohort[[i]]$May2/Cohort[[i]]$Age2.5
  Cohort[[i]]$Imp2_6<-Cohort[[i]]$Jun2/Cohort[[i]]$Age2.6
  Cohort[[i]]$Imp2_7<-Cohort[[i]]$Jul2/Cohort[[i]]$Age2.7
  Cohort[[i]]$Imp2_8<-Cohort[[i]]$Aug2/Cohort[[i]]$Age2.8
  Cohort[[i]]$Imp2_9<-Cohort[[i]]$Sept2/Cohort[[i]]$Age2.9
  Cohort[[i]]$Imp2_10<-Cohort[[i]]$Oct2/Cohort[[i]]$Age2.10
  Cohort[[i]]$Imp2_11<-Cohort[[i]]$Nov2/Cohort[[i]]$Age2.11
  Cohort[[i]]$Imp2_12<-Cohort[[i]]$Dec2/Cohort[[i]]$Age2.12

  Cohort[[i]]$Imp3_1<-Cohort[[i]]$Jan3/Cohort[[i]]$Age3.1
  Cohort[[i]]$Imp3_2<-Cohort[[i]]$Feb3/Cohort[[i]]$Age3.2
  Cohort[[i]]$Imp3_3<-Cohort[[i]]$Mar3/Cohort[[i]]$Age3.3
  Cohort[[i]]$Imp3_4<-Cohort[[i]]$Apr3/Cohort[[i]]$Age3.4
  Cohort[[i]]$Imp3_5<-Cohort[[i]]$May3/Cohort[[i]]$Age3.5
  Cohort[[i]]$Imp3_6<-Cohort[[i]]$Jun3/Cohort[[i]]$Age3.6
  Cohort[[i]]$Imp3_7<-Cohort[[i]]$Jul3/Cohort[[i]]$Age3.7
  Cohort[[i]]$Imp3_8<-Cohort[[i]]$Aug3/Cohort[[i]]$Age3.8
  Cohort[[i]]$Imp3_9<-Cohort[[i]]$Sept3/Cohort[[i]]$Age3.9
  Cohort[[i]]$Imp3_10<-Cohort[[i]]$Oct3/Cohort[[i]]$Age3.10
  Cohort[[i]]$Imp3_11<-Cohort[[i]]$Nov3/Cohort[[i]]$Age3.11
  Cohort[[i]]$Imp3_12<-Cohort[[i]]$Dec3/Cohort[[i]]$Age3.12

  Cohort[[i]]$Imp4_2<-Cohort[[i]]$Feb4/Cohort[[i]]$Age4.2
  Cohort[[i]]$Imp4_3<-Cohort[[i]]$Mar4/Cohort[[i]]$Age4.3
  Cohort[[i]]$Imp4_4<-Cohort[[i]]$Apr4/Cohort[[i]]$Age4.4
  Cohort[[i]]$Imp4_5<-Cohort[[i]]$May4/Cohort[[i]]$Age4.5
  Cohort[[i]]$Imp4_6<-Cohort[[i]]$Jun4/Cohort[[i]]$Age4.6
  Cohort[[i]]$Imp4_7<-Cohort[[i]]$Jul4/Cohort[[i]]$Age4.7
  Cohort[[i]]$Imp4_8<-Cohort[[i]]$Aug4/Cohort[[i]]$Age4.8
  Cohort[[i]]$Imp4_9<-Cohort[[i]]$Sept4/Cohort[[i]]$Age4.9
  Cohort[[i]]$Imp4_10<-Cohort[[i]]$Oct4/Cohort[[i]]$Age4.10
  Cohort[[i]]$Imp4_11<-Cohort[[i]]$Nov4/Cohort[[i]]$Age4.11

  Cohort[[i]]$Imp5_2<-Cohort[[i]]$Feb5/Cohort[[i]]$Age5.2
  Cohort[[i]]$Imp5_3<-Cohort[[i]]$Mar5/Cohort[[i]]$Age5.3
  Cohort[[i]]$Imp5_4<-Cohort[[i]]$Apr5/Cohort[[i]]$Age5.4
  Cohort[[i]]$Imp5_5<-Cohort[[i]]$May5/Cohort[[i]]$Age5.5
  Cohort[[i]]$Imp5_6<-Cohort[[i]]$Jun5/Cohort[[i]]$Age5.6
  Cohort[[i]]$Imp5_7<-Cohort[[i]]$Jul5/Cohort[[i]]$Age5.7
  Cohort[[i]]$Imp5_8<-Cohort[[i]]$Aug5/Cohort[[i]]$Age5.8
  Cohort[[i]]$Imp5_9<-Cohort[[i]]$Sept5/Cohort[[i]]$Age5.9
  Cohort[[i]]$Imp5_10<-Cohort[[i]]$Oct5/Cohort[[i]]$Age5.10
  #River impact rates
  Cohort[[i]]$ImpRiv5<-Cohort[[i]]$InRiver5/(Cohort[[i]]$Age5Sp+Cohort[[i]]$Age5Hat+Cohort[[i]]$InRiver5)
  Cohort[[i]]$ImpRiv4<-Cohort[[i]]$InRiver4/(Cohort[[i]]$Age4Sp+Cohort[[i]]$Age4Hat+Cohort[[i]]$InRiver4)
  Cohort[[i]]$ImpRiv3<-Cohort[[i]]$InRiver3/(Cohort[[i]]$Age3Sp+Cohort[[i]]$Age3Hat+Cohort[[i]]$InRiver3)
  Cohort[[i]]$ImpRiv2<-Cohort[[i]]$InRiver2/(Cohort[[i]]$Age2Sp+Cohort[[i]]$Age2Hat+Cohort[[i]]$InRiver2)
#Monthly harvest rates (for calculating natural origin harvest, not done when
  #reconstructing specific hatchery cohorts)
  Cohort[[i]]$Har2_4<-Fishing_Harvest[[i]]$Apr2/Cohort[[i]]$Age2.4
  Cohort[[i]]$Har2_5<-Fishing_Harvest[[i]]$May2/Cohort[[i]]$Age2.5
  Cohort[[i]]$Har2_6<-Fishing_Harvest[[i]]$Jun2/Cohort[[i]]$Age2.6
  Cohort[[i]]$Har2_7<-Fishing_Harvest[[i]]$Jul2/Cohort[[i]]$Age2.7
  Cohort[[i]]$Har2_8<-Fishing_Harvest[[i]]$Aug2/Cohort[[i]]$Age2.8
  Cohort[[i]]$Har2_9<-Fishing_Harvest[[i]]$Sept2/Cohort[[i]]$Age2.9
  Cohort[[i]]$Har2_10<-Fishing_Harvest[[i]]$Oct2/Cohort[[i]]$Age2.10
  Cohort[[i]]$Har2_11<-Fishing_Harvest[[i]]$Nov2/Cohort[[i]]$Age2.11
  Cohort[[i]]$Har2_12<-Fishing_Harvest[[i]]$Dec2/Cohort[[i]]$Age2.12
  
  Cohort[[i]]$Har3_1<-Fishing_Harvest[[i]]$Jan3/Cohort[[i]]$Age3.1
  Cohort[[i]]$Har3_2<-Fishing_Harvest[[i]]$Feb3/Cohort[[i]]$Age3.2
  Cohort[[i]]$Har3_3<-Fishing_Harvest[[i]]$Mar3/Cohort[[i]]$Age3.3
  Cohort[[i]]$Har3_4<-Fishing_Harvest[[i]]$Apr3/Cohort[[i]]$Age3.4
  Cohort[[i]]$Har3_5<-Fishing_Harvest[[i]]$May3/Cohort[[i]]$Age3.5
  Cohort[[i]]$Har3_6<-Fishing_Harvest[[i]]$Jun3/Cohort[[i]]$Age3.6
  Cohort[[i]]$Har3_7<-Fishing_Harvest[[i]]$Jul3/Cohort[[i]]$Age3.7
  Cohort[[i]]$Har3_8<-Fishing_Harvest[[i]]$Aug3/Cohort[[i]]$Age3.8
  Cohort[[i]]$Har3_9<-Fishing_Harvest[[i]]$Sept3/Cohort[[i]]$Age3.9
  Cohort[[i]]$Har3_10<-Fishing_Harvest[[i]]$Oct3/Cohort[[i]]$Age3.10
  Cohort[[i]]$Har3_11<-Fishing_Harvest[[i]]$Nov3/Cohort[[i]]$Age3.11
  Cohort[[i]]$Har3_12<-Fishing_Harvest[[i]]$Dec3/Cohort[[i]]$Age3.12
  
  Cohort[[i]]$Har4_2<-Fishing_Harvest[[i]]$Feb4/Cohort[[i]]$Age4.2
  Cohort[[i]]$Har4_3<-Fishing_Harvest[[i]]$Mar4/Cohort[[i]]$Age4.3
  Cohort[[i]]$Har4_4<-Fishing_Harvest[[i]]$Apr4/Cohort[[i]]$Age4.4
  Cohort[[i]]$Har4_5<-Fishing_Harvest[[i]]$May4/Cohort[[i]]$Age4.5
  Cohort[[i]]$Har4_6<-Fishing_Harvest[[i]]$Jun4/Cohort[[i]]$Age4.6
  Cohort[[i]]$Har4_7<-Fishing_Harvest[[i]]$Jul4/Cohort[[i]]$Age4.7
  Cohort[[i]]$Har4_8<-Fishing_Harvest[[i]]$Aug4/Cohort[[i]]$Age4.8
  Cohort[[i]]$Har4_9<-Fishing_Harvest[[i]]$Sept4/Cohort[[i]]$Age4.9
  Cohort[[i]]$Har4_10<-Fishing_Harvest[[i]]$Oct4/Cohort[[i]]$Age4.10
  Cohort[[i]]$Har4_11<-Fishing_Harvest[[i]]$Nov4/Cohort[[i]]$Age4.11
  
  Cohort[[i]]$Har5_2<-Fishing_Harvest[[i]]$Feb5/Cohort[[i]]$Age5.2
  Cohort[[i]]$Har5_3<-Fishing_Harvest[[i]]$Mar5/Cohort[[i]]$Age5.3
  Cohort[[i]]$Har5_4<-Fishing_Harvest[[i]]$Apr5/Cohort[[i]]$Age5.4
  Cohort[[i]]$Har5_5<-Fishing_Harvest[[i]]$May5/Cohort[[i]]$Age5.5
  Cohort[[i]]$Har5_6<-Fishing_Harvest[[i]]$Jun5/Cohort[[i]]$Age5.6
  Cohort[[i]]$Har5_7<-Fishing_Harvest[[i]]$Jul5/Cohort[[i]]$Age5.7
  Cohort[[i]]$Har5_8<-Fishing_Harvest[[i]]$Aug5/Cohort[[i]]$Age5.8
  Cohort[[i]]$Har5_9<-Fishing_Harvest[[i]]$Sept5/Cohort[[i]]$Age5.9
  Cohort[[i]]$Har5_10<-Fishing_Harvest[[i]]$Oct5/Cohort[[i]]$Age5.10
#Spawner reduction rates
  Cohort[[i]]$Age2NF<-Cohort[[i]]$Age2.4*(1-0.0561)^5*Cohort[[i]]$Mat2
  Cohort[[i]]$Age3NF<-(Cohort[[i]]$Age2.4*(1-0.0561)^5*(1-Cohort[[i]]$Mat2))*(1-0.0184)^12*Cohort[[i]]$Mat3
  Cohort[[i]]$Age4NF<-(Cohort[[i]]$Age2.4*(1-0.0561)^5*(1-Cohort[[i]]$Mat2))*(1-0.0184)^12*(1-Cohort[[i]]$Mat3)*(1-0.0184)^12*Cohort[[i]]$Mat4
  Cohort[[i]]$Age5NF<-(Cohort[[i]]$Age2.4*(1-0.0561)^5*(1-Cohort[[i]]$Mat2))*(1-0.0184)^12*(1-Cohort[[i]]$Mat3)*(1-0.0184)^12*(1-Cohort[[i]]$Mat4)*(1-0.0184)^12
#Immediate abundance w/o fishing in current year
  Cohort[[i]]$Age3Hyp<-Cohort[[i]]$Age3.4*(1-0.0184)^5*Cohort[[i]]$Mat3
  Cohort[[i]]$Age4Hyp<-Cohort[[i]]$Age4.4*(1-0.0184)^5*Cohort[[i]]$Mat4
  Cohort[[i]]$Age5Hyp<-Cohort[[i]]$Age5.4*(1-0.0184)^5
  Cohort[[i]][is.na(Cohort[[i]])]<-0 #data frame
}
Maturation_Uncertainty_Bootstrap<-array(NA, c(length(Cohort[[1]]$brood_year),3,1000))
for(i in 1:1000){
  Maturation_Uncertainty_Bootstrap[,1,i]<-Cohort[[i]]$Mat2
  Maturation_Uncertainty_Bootstrap[,2,i]<-Cohort[[i]]$Mat3
  Maturation_Uncertainty_Bootstrap[,3,i]<-Cohort[[i]]$Mat4
}
Maturation_Uncertainty<-matrix(nrow = length(Cohort[[1]]$brood_year), ncol=12)
for(i in 1:length(Cohort[[1]]$brood_year)){
  for(j in 1:3){
    Maturation_Uncertainty[i,c(1+(j-1)*3,2+(j-1)*3,3+(j-1)*3)]<-quantile(Maturation_Uncertainty_Bootstrap[i,j,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) #Mat 2
    Maturation_Uncertainty[i,2]<-mean(Maturation_Uncertainty_Bootstrap[i,1,])
    Maturation_Uncertainty[i,5]<-mean(Maturation_Uncertainty_Bootstrap[i,2,])
    Maturation_Uncertainty[i,8]<-mean(Maturation_Uncertainty_Bootstrap[i,3,])
    Maturation_Uncertainty[i,9+j]<-sd(Maturation_Uncertainty_Bootstrap[i,j,]) #to get SD for weighted Mann Kendall test
  }
}
brood_year<-Cohort[[1]]$brood_year
Maturation_Uncertainty<-as.data.frame(cbind(brood_year,Maturation_Uncertainty))
names(Maturation_Uncertainty)<-c("brood_year","Mat2Lower","Mat2Mean",  "Mat2Upper", "Mat3Lower", "Mat3Mean","Mat3Upper", "Mat4Lower", "Mat4Mean","Mat4Upper","SDMat2","SDMat3","SDMat4")
write.csv(Maturation_Uncertainty,"Maturation Uncertainty CWT.csv", row.names = FALSE)

#Fishery Impact Rates (Only calculated for all hatcheries combined)
Impact_Uncertainty_Bootstrap<-array(NA, c(length(Cohort[[1]]$brood_year),4,1000))
for(i in 1:1000){
  Impact_Uncertainty_Bootstrap[,1,i]<-Cohort[[i]]$Imp2
  Impact_Uncertainty_Bootstrap[,2,i]<-Cohort[[i]]$Imp3
  Impact_Uncertainty_Bootstrap[,3,i]<-Cohort[[i]]$Imp4
  Impact_Uncertainty_Bootstrap[,4,i]<-Cohort[[i]]$Imp5
}
Impact_Uncertainty<-matrix(nrow = nrow(Cohort[[1]])*4, ncol=4)
#5,50,95 quantile
for(i in 1:nrow(Cohort[[1]])){
  for(j in 1:4){
    Impact_Uncertainty[i+(j-1)*nrow(Cohort[[1]]),c(1:3)]<-quantile(Impact_Uncertainty_Bootstrap[i,j,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) #Mat 2
    Impact_Uncertainty[i+(j-1)*nrow(Cohort[[1]]),2]<-mean(Impact_Uncertainty_Bootstrap[i,j,c(1:1000)], na.rm = TRUE) 
    Impact_Uncertainty[i+(j-1)*nrow(Cohort[[1]]),4]<-j+1
    }
}
Impact_Uncertainty<-as.data.frame(cbind(rep(Cohort[[1]]$brood_year,4),Impact_Uncertainty))
names(Impact_Uncertainty)<-c("brood_year","ImpLower","ImpMean",  "ImpUpper",  "Age")
# write.csv(Impact_Uncertainty,"Impact Uncertainty CWT.csv", row.names = FALSE)

#Summarizing abundances without fishing for calculating
#a)Abundance without cumulative fishing (Unfished)
#b)Abundance without fishing in the current year (no fishing)
#c)Spawning reduction rate without fishing in the current year
WOfishing_Bootstrap<-array(NA, c(nrow(Cohort[[1]])-2-2,6,1000))
for(i in 1:1000){
  for(j in 3:(nrow(Cohort[[1]])-2)){
  WOfishing_Bootstrap[j-2,1,i]<-Cohort[[1]]$brood_year[j]+4 #run year from brood year
  #never fished 3+
  WOfishing_Bootstrap[j-2,2,i]<-Cohort[[i]]$Age3NF[1+j]+
    Cohort[[i]]$Age4NF[j]+
    Cohort[[i]]$Age5NF[j-1]
  #no fishing 3+
  WOfishing_Bootstrap[j-2,3,i]<-Cohort[[i]]$Age3Hyp[1+j]+
    Cohort[[i]]$Age4Hyp[j]+
    Cohort[[i]]$Age5Hyp[j-1]
  #SRR
  WOfishing_Bootstrap[j-2,4,i]<-1-((Cohort[[i]]$Age2Sp[2+j]+Cohort[[i]]$Age2Hat[2+j]+ #actual runs
                                       Cohort[[i]]$Age3Sp[1+j]+Cohort[[i]]$Age3Hat[1+j]+
                                       Cohort[[i]]$Age4Sp[j]+Cohort[[i]]$Age4Hat[j]+
                                       Cohort[[i]]$Age5Sp[j-1]+Cohort[[i]]$Age5Hat[j-1])/
                                  (Cohort[[i]]$Age2NF[2+j]+ #hypothetical runs
                                    Cohort[[i]]$Age3NF[1+j]+
                                      Cohort[[i]]$Age4NF[j]+
                                    Cohort[[i]]$Age5NF[j-1]))
  #SRR.y
  WOfishing_Bootstrap[j-2,5,i]<-1-((Cohort[[i]]$Age2Sp[2+j]+Cohort[[i]]$Age2Hat[2+j]+ #actual runs
                                    Cohort[[i]]$Age3Sp[1+j]+Cohort[[i]]$Age3Hat[1+j]+
                                    Cohort[[i]]$Age4Sp[j]+Cohort[[i]]$Age4Hat[j])/
                                   (Cohort[[i]]$Age2NF[2+j]+ #hypothetical runs
                                      Cohort[[i]]$Age3Hyp[1+j]+
                                      Cohort[[i]]$Age4Hyp[j]))
  #September abundance
  WOfishing_Bootstrap[j-2,6,i]<-(Cohort[[i]]$Age2.10[1+j])/(1-0.0184)+Cohort[[i]]$Sept2[1+j]+
                                      (Cohort[[i]]$Age3.10[j])/(1-0.0184)+Cohort[[i]]$Sept3[j]+
                                      (Cohort[[i]]$Age4.10[j-1])/(1-0.0184)+Cohort[[i]]$Sept4[j-1]
}
}
#5,50,95 quantile for never fished abundance 3+
Unfished3<-matrix(nrow = nrow(WOfishing_Bootstrap[,,1]), ncol=4)
for(i in 1:nrow(WOfishing_Bootstrap[,,1])){
  Unfished3[i,1]<-WOfishing_Bootstrap[i,1,1]
  Unfished3[i,c(2,4)]<-quantile(WOfishing_Bootstrap[i,2,c(1:1000)], probs=c(.025,.975), na.rm = TRUE)
  Unfished3[i,3]<-mean(WOfishing_Bootstrap[i,2,c(1:1000)]) 
}
colnames(Unfished3)<-c("run_year","Unfished3Lower","Unfished3Mean",  "Unfished3Upper")
write.csv(Unfished3, "Unfished3 CWT.csv",row.names = FALSE)
#5,50,95 quantile for no fishing abundance 3+
NoFishing3<-matrix(nrow = nrow(WOfishing_Bootstrap[,,1]), ncol=4)
for(i in 1:nrow(WOfishing_Bootstrap[,,1])){
  NoFishing3[i,1]<-WOfishing_Bootstrap[i,1,1]
  NoFishing3[i,c(2,4)]<-quantile(WOfishing_Bootstrap[i,3,c(1:1000)], probs=c(.025,.975), na.rm = TRUE)
  NoFishing3[i,3]<-mean(WOfishing_Bootstrap[i,3,c(1:1000)]) 
}
colnames(NoFishing3)<-c("run_year","Nofishing3Lower","Nofishing3Mean",  "Nofishing3Upper")
write.csv(NoFishing3, "Nofishing3 CWT.csv",row.names = FALSE)
#5,50,95 quantile for SRR
SRR_Uncertainty<-matrix(nrow = nrow(WOfishing_Bootstrap[,,1]), ncol=4)
for(i in 1:nrow(WOfishing_Bootstrap[,,1])){
    SRR_Uncertainty[i,1]<-WOfishing_Bootstrap[i,1,1]
    SRR_Uncertainty[i,c(2,4)]<-quantile(WOfishing_Bootstrap[i,4,c(1:1000)], probs=c(.025,.975), na.rm = TRUE) 
    SRR_Uncertainty[i,3]<-mean(WOfishing_Bootstrap[i,4,c(1:1000)]) #Mat 2
    }
colnames(SRR_Uncertainty)<-c("run_year","SRRLower","SRRMean",  "SRRUpper")
write.csv(SRR_Uncertainty, "SRR CWT.csv",row.names = FALSE)
#5,50,95 quantile for SRR.y
SRRy_Uncertainty<-matrix(nrow = nrow(WOfishing_Bootstrap[,,1]), ncol=4)
for(i in 1:nrow(WOfishing_Bootstrap[,,1])){
  SRRy_Uncertainty[i,1]<-WOfishing_Bootstrap[i,1,1]
  SRRy_Uncertainty[i,c(2,4)]<-quantile(WOfishing_Bootstrap[i,5,c(1:1000)], probs=c(.025,.975), na.rm = TRUE) 
  SRRy_Uncertainty[i,3]<-mean(WOfishing_Bootstrap[i,5,c(1:1000)]) #Mat 2
}
colnames(SRRy_Uncertainty)<-c("run_year","SRRyLower","SRRyMean",  "SRRyUpper")
write.csv(SRRy_Uncertainty, "SRRy CWT.csv",row.names = FALSE)
#5,50,95 quantile for Sept 1 abundance
Sept1_Uncertainty<-matrix(nrow = nrow(WOfishing_Bootstrap[,,1]), ncol=4)
for(i in 1:nrow(WOfishing_Bootstrap[,,1])){
  Sept1_Uncertainty[i,1]<-WOfishing_Bootstrap[i,1,1]
  Sept1_Uncertainty[i,c(2,4)]<-quantile(WOfishing_Bootstrap[i,6,c(1:1000)], probs=c(.025,.975), na.rm = TRUE) 
  Sept1_Uncertainty[i,3]<-mean(WOfishing_Bootstrap[i,6,c(1:1000)]) #Mat 2
}
colnames(Sept1_Uncertainty)<-c("run_year","Sept1Lower","Sept1Mean",  "Sept1Upper")
write.csv(Sept1_Uncertainty, "Sept1 CWT.csv",row.names = FALSE)

Outmigration_Uncertainty_Bootstrap<-array(NA, c(length(Cohort[[1]]$brood_year),1,1000))
for(i in 1:1000){
  Outmigration_Uncertainty_Bootstrap[,1,i]<-Cohort[[i]]$Out_Survival
}
Outmigration_Uncertainty<-matrix(nrow = length(Cohort[[1]]$brood_year), ncol=3)
for(i in 1:length(Cohort[[1]]$brood_year)){
    Outmigration_Uncertainty[i,]<-quantile(Outmigration_Uncertainty_Bootstrap[i,,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE)
    Outmigration_Uncertainty[i,2]<-mean(Outmigration_Uncertainty_Bootstrap[i,1,])
}
brood_year<-Cohort[[1]]$brood_year
Outmigration_Uncertainty<-as.data.frame(cbind(brood_year,Outmigration_Uncertainty))
names(Outmigration_Uncertainty)<-c("brood_year","OutSLower","OutSMean",  "OutSUpper")
write.csv(Outmigration_Uncertainty,"Outmigration Survival.csv", row.names = FALSE)



#monthly impact rates
test<-Cohort[[1]]
for(i in 1:1000){
  Cohort[[i]]<-Cohort[[i]][,c(1,167:206)] 
}
# saveRDS(Cohort, "CWT monthly harvest rates.Rds")

#monthly harvest rates
test<-Cohort[[1]]
for(i in 1:1000){
  Cohort[[i]]<-Cohort[[i]][,c(1,123:166)]
}
# saveRDS(Cohort, "CWT monthly impact rates.Rds")