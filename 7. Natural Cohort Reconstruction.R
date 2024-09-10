#####################################
### Natural Cohort Reconstruction ###
#####################################
library(dplyr)
library(tidyr)
setwd("C:/Users/echen/Box/Sacramento River Fall Chinook/Methodology Review")
Hatchery<-readRDS("Natural SpawnersAdjusted FRH.Rds")
Natural<-readRDS("Natural SpawnersAdjusted Sac.Rds")
Spawners<-Natural
#Only needed when there is natural escapement to hatchery
# Spawners<-list()
# for(i in 1:1000){
# Spawners[[i]]<-Natural[[i]] %>%
#   rbind(Hatchery[[i]]) %>%
#   group_by(brood_year) %>%
#   summarise(Age2Sp = sum(Age2Sp), Age3Sp = sum(Age3Sp), Age4Sp = sum(Age4Sp))
# }
Impact<-readRDS("CWT monthly impact rates.Rds")
Harvest<-readRDS("CWT monthly harvest rates.Rds")
#Parent Escapement
Total_Escapement<-read.csv("River Escapement.csv")
Total_Escapement<-Total_Escapement %>%
  # filter(River == "AMERICAN RIVER")%>%
  filter(River == "SAC R AB RBDD") %>%
  # filter(River == "YUBA RIVER") %>%
  # filter(River == "MILL CREEK"|River == "COTTONWOOD"|River == "DEER CREEK"|River == "COW CREEK"|River == "BUTTE CREEK") %>%
  # filter(River == "BATTLE CREEK") %>%
  # filter(River == "FEATHER RIVER") %>%
  # filter(River == "CLEAR CREEK") %>%
  group_by(run_year) %>%
  summarise(Total_Escapement = sum(Count))
colnames(Total_Escapement)<-c("brood_year", "Parent_escapement")
Cohort<-list()
for(i in 1:1000){
Cohort[[i]]<-Spawners[[i]] %>%
  left_join(Total_Escapement) %>%
  left_join(Impact[[i]])%>%
  left_join(Harvest[[i]]) %>%
  filter(brood_year >= 2006 & brood_year <= 2018) #2011 for Yuba, 2008 for OTHER, 2006 for everything else
#Start of September Age-4, fish get fished, natural-mortality, remaining spawn at end of September
Cohort[[i]]$Age4.9<-(((Cohort[[i]]$Age4Sp)/(1-Cohort[[i]]$ImpRiv4))/(1-0.0184))/(1-Cohort[[i]]$Imp4_9)
Cohort[[i]]$Age4.8<-(Cohort[[i]]$Age4.9/(1-0.0184))/(1-Cohort[[i]]$Imp4_8)
Cohort[[i]]$Age4.7<-(Cohort[[i]]$Age4.8/(1-0.0184))/(1-Cohort[[i]]$Imp4_7)
Cohort[[i]]$Age4.6<-(Cohort[[i]]$Age4.7/(1-0.0184))/(1-Cohort[[i]]$Imp4_6)
Cohort[[i]]$Age4.5<-(Cohort[[i]]$Age4.6/(1-0.0184))/(1-Cohort[[i]]$Imp4_5)
Cohort[[i]]$Age4.4<-(Cohort[[i]]$Age4.5/(1-0.0184))/(1-Cohort[[i]]$Imp4_4)
Cohort[[i]]$Age4.3<-(Cohort[[i]]$Age4.4/(1-0.0184))/(1-Cohort[[i]]$Imp4_3)
Cohort[[i]]$Age4.2<-(Cohort[[i]]$Age4.3/(1-0.0184))/(1-Cohort[[i]]$Imp4_2)
Cohort[[i]]$Age4.1<-Cohort[[i]]$Age4.2/(1-0.0184)

Cohort[[i]]$Age3.12<-(Cohort[[i]]$Age4.1/(1-0.0184))/(1-Cohort[[i]]$Imp3_12)
Cohort[[i]]$Age3.11<-(Cohort[[i]]$Age3.12/(1-0.0184))/(1-Cohort[[i]]$Imp3_11)
Cohort[[i]]$Age3.10<-(Cohort[[i]]$Age3.11/(1-0.0184))/(1-Cohort[[i]]$Imp3_10)
Cohort[[i]]$Age3.9<-((((Cohort[[i]]$Age3Sp)/(1-Cohort[[i]]$ImpRiv3))+Cohort[[i]]$Age3.10)/(1-0.0184))/(1-Cohort[[i]]$Imp3_9)
Cohort[[i]]$Age3.8<-(Cohort[[i]]$Age3.9/(1-0.0184))/(1-Cohort[[i]]$Imp3_8)
Cohort[[i]]$Age3.7<-(Cohort[[i]]$Age3.8/(1-0.0184))/(1-Cohort[[i]]$Imp3_7)
Cohort[[i]]$Age3.6<-(Cohort[[i]]$Age3.7/(1-0.0184))/(1-Cohort[[i]]$Imp3_6)
Cohort[[i]]$Age3.5<-(Cohort[[i]]$Age3.6/(1-0.0184))/(1-Cohort[[i]]$Imp3_5)
Cohort[[i]]$Age3.4<-(Cohort[[i]]$Age3.5/(1-0.0184))/(1-Cohort[[i]]$Imp3_4)
Cohort[[i]]$Age3.3<-(Cohort[[i]]$Age3.4/(1-0.0184))/(1-Cohort[[i]]$Imp3_3)
Cohort[[i]]$Age3.2<-(Cohort[[i]]$Age3.3/(1-0.0184))/(1-Cohort[[i]]$Imp3_2)
Cohort[[i]]$Age3.1<-(Cohort[[i]]$Age3.2/(1-0.0184))/(1-Cohort[[i]]$Imp3_1)

Cohort[[i]]$Age2.12<-(Cohort[[i]]$Age3.1/(1-0.0184))/(1-Cohort[[i]]$Imp2_12)
Cohort[[i]]$Age2.11<-(Cohort[[i]]$Age2.12/(1-0.0184))/(1-Cohort[[i]]$Imp2_11)
Cohort[[i]]$Age2.10<-(Cohort[[i]]$Age2.11/(1-0.0184))/(1-Cohort[[i]]$Imp2_10)
Cohort[[i]]$Age2.9<-((((Cohort[[i]]$Age2Sp)/(1-Cohort[[i]]$ImpRiv2))+Cohort[[i]]$Age2.10)/(1-0.0184))/(1-Cohort[[i]]$Imp2_9)
Cohort[[i]]$Age2.8<-(Cohort[[i]]$Age2.9/(1-0.0561))/(1-Cohort[[i]]$Imp2_8)
Cohort[[i]]$Age2.7<-(Cohort[[i]]$Age2.8/(1-0.0561))/(1-Cohort[[i]]$Imp2_7)
Cohort[[i]]$Age2.6<-(Cohort[[i]]$Age2.7/(1-0.0561))/(1-Cohort[[i]]$Imp2_6)
Cohort[[i]]$Age2.5<-(Cohort[[i]]$Age2.6/(1-0.0561))/(1-Cohort[[i]]$Imp2_5)
Cohort[[i]]$Age2.4<-(Cohort[[i]]$Age2.5/(1-0.0561))/(1-Cohort[[i]]$Imp2_4)
Cohort[[i]]$Age2.3<-Cohort[[i]]$Age2.4/(1-0.0561)
Cohort[[i]]$Age2.2<-Cohort[[i]]$Age2.3/(1-0.0561)
Cohort[[i]]$Age2.1<-Cohort[[i]]$Age2.2/(1-0.0561)
Cohort[[i]]$Age1.12<-Cohort[[i]]$Age2.1/(1-0.0561)
Cohort[[i]]$Age1.11<-Cohort[[i]]$Age1.12/(1-0.0561)
Cohort[[i]]$Age1.10<-Cohort[[i]]$Age1.11/(1-0.0561)
Cohort[[i]]$Age1.9<-Cohort[[i]]$Age1.10/(1-0.0561)

#Estimating Maturation
Cohort[[i]]$Mat4<-1
Cohort[[i]]$Mat3<-((Cohort[[i]]$Age3Sp)/(1-Cohort[[i]]$ImpRiv3))/((Cohort[[i]]$Age3Sp)/(1-Cohort[[i]]$ImpRiv3)+Cohort[[i]]$Age3.10)
Cohort[[i]]$Mat2<-((Cohort[[i]]$Age2Sp)/(1-Cohort[[i]]$ImpRiv2))/((Cohort[[i]]$Age2Sp)/(1-Cohort[[i]]$ImpRiv2)+Cohort[[i]]$Age2.10)
Cohort[[i]]$Mat3[is.na(Cohort[[i]]$Mat3)]<-1 
#Spawner to age-2 rate
Cohort[[i]]$Production<-Cohort[[i]]$Age1.9/Cohort[[i]]$Parent_escapement
#Spawner reduction rates
Cohort[[i]]$Age2NF<-Cohort[[i]]$Age2.4*(1-0.0561)^5*Cohort[[i]]$Mat2
Cohort[[i]]$Age3NF<-(Cohort[[i]]$Age2.4*(1-0.0561)^5*(1-Cohort[[i]]$Mat2))*(1-0.0184)^12*Cohort[[i]]$Mat3
Cohort[[i]]$Age4NF<-(Cohort[[i]]$Age2.4*(1-0.0561)^5*(1-Cohort[[i]]$Mat2))*(1-0.0184)^12*(1-Cohort[[i]]$Mat3)*(1-0.0184)^12*Cohort[[i]]$Mat4
#Immediate abundance w/o fishing
Cohort[[i]]$Age3Hyp<-Cohort[[i]]$Age3.4*(1-0.0184)^5*Cohort[[i]]$Mat3
Cohort[[i]]$Age4Hyp<-Cohort[[i]]$Age4.4*(1-0.0184)^5*Cohort[[i]]$Mat4
#Impacted Individuals
Cohort[[i]]$Imp2<-Cohort[[i]]$Imp2_4*Cohort[[i]]$Age2.4+Cohort[[i]]$Imp2_5*Cohort[[i]]$Age2.5+Cohort[[i]]$Imp2_6*Cohort[[i]]$Age2.6+Cohort[[i]]$Imp2_7*Cohort[[i]]$Age2.7+Cohort[[i]]$Imp2_8*Cohort[[i]]$Age2.8
Cohort[[i]]$Imp3<-Cohort[[i]]$Imp2_9*Cohort[[i]]$Age2.9+Cohort[[i]]$Imp2_10*Cohort[[i]]$Age2.10+Cohort[[i]]$Imp2_11*Cohort[[i]]$Age2.11+Cohort[[i]]$Imp2_12*Cohort[[i]]$Age2.12+
  Cohort[[i]]$Imp3_1*Cohort[[i]]$Age3.1+Cohort[[i]]$Imp3_2*Cohort[[i]]$Age3.2+Cohort[[i]]$Imp3_3*Cohort[[i]]$Age3.3+Cohort[[i]]$Imp3_4*Cohort[[i]]$Age3.4+Cohort[[i]]$Imp3_5*Cohort[[i]]$Age3.5+
  +Cohort[[i]]$Imp3_6*Cohort[[i]]$Age3.6+Cohort[[i]]$Imp3_7*Cohort[[i]]$Age3.7+Cohort[[i]]$Imp3_8*Cohort[[i]]$Age3.8
Cohort[[i]]$Imp4<-Cohort[[i]]$Imp3_9*Cohort[[i]]$Age3.9+Cohort[[i]]$Imp3_10*Cohort[[i]]$Age3.10+Cohort[[i]]$Imp3_11*Cohort[[i]]$Age3.11+Cohort[[i]]$Imp3_12*Cohort[[i]]$Age3.12+
    Cohort[[i]]$Imp4_2*Cohort[[i]]$Age4.2+Cohort[[i]]$Imp4_3*Cohort[[i]]$Age4.3+Cohort[[i]]$Imp4_4*Cohort[[i]]$Age4.4+Cohort[[i]]$Imp4_5*Cohort[[i]]$Age4.5+
  +Cohort[[i]]$Imp4_6*Cohort[[i]]$Age4.6+Cohort[[i]]$Imp4_7*Cohort[[i]]$Age4.7+Cohort[[i]]$Imp4_8*Cohort[[i]]$Age4.8
#Harvest Individuals
Cohort[[i]]$Har2<-Cohort[[i]]$Har2_4*Cohort[[i]]$Age2.4+Cohort[[i]]$Har2_5*Cohort[[i]]$Age2.5+Cohort[[i]]$Har2_6*Cohort[[i]]$Age2.6+Cohort[[i]]$Har2_7*Cohort[[i]]$Age2.7+Cohort[[i]]$Har2_8*Cohort[[i]]$Age2.8
Cohort[[i]]$Har3<-Cohort[[i]]$Har2_9*Cohort[[i]]$Age2.9+Cohort[[i]]$Har2_10*Cohort[[i]]$Age2.10+Cohort[[i]]$Har2_11*Cohort[[i]]$Age2.11+Cohort[[i]]$Har2_12*Cohort[[i]]$Age2.12+
  Cohort[[i]]$Har3_1*Cohort[[i]]$Age3.1+Cohort[[i]]$Har3_2*Cohort[[i]]$Age3.2+Cohort[[i]]$Har3_3*Cohort[[i]]$Age3.3+Cohort[[i]]$Har3_4*Cohort[[i]]$Age3.4+Cohort[[i]]$Har3_5*Cohort[[i]]$Age3.5+
  +Cohort[[i]]$Har3_6*Cohort[[i]]$Age3.6+Cohort[[i]]$Har3_7*Cohort[[i]]$Age3.7+Cohort[[i]]$Har3_8*Cohort[[i]]$Age3.8
Cohort[[i]]$Har4<-Cohort[[i]]$Har3_9*Cohort[[i]]$Age3.9+Cohort[[i]]$Har3_10*Cohort[[i]]$Age3.10+Cohort[[i]]$Har3_11*Cohort[[i]]$Age3.11+Cohort[[i]]$Har3_12*Cohort[[i]]$Age3.12+
  Cohort[[i]]$Har4_2*Cohort[[i]]$Age4.2+Cohort[[i]]$Har4_3*Cohort[[i]]$Age4.3+Cohort[[i]]$Har4_4*Cohort[[i]]$Age4.4+Cohort[[i]]$Har4_5*Cohort[[i]]$Age4.5+
  +Cohort[[i]]$Har4_6*Cohort[[i]]$Age4.6+Cohort[[i]]$Har4_7*Cohort[[i]]$Age4.7+Cohort[[i]]$Har4_8*Cohort[[i]]$Age4.8
}
#End
########################################
#Summarizing maturation results
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
    Maturation_Uncertainty[i,2]<-mean(Maturation_Uncertainty_Bootstrap[i,1,], na.rm = TRUE)
    Maturation_Uncertainty[i,5]<-mean(Maturation_Uncertainty_Bootstrap[i,2,], na.rm = TRUE)
    Maturation_Uncertainty[i,8]<-mean(Maturation_Uncertainty_Bootstrap[i,3,], na.rm = TRUE)
    Maturation_Uncertainty[i,9+j]<-sd(Maturation_Uncertainty_Bootstrap[i,j,]) #to get SD for weighted Mann Kendall test
  }
}
brood_year<-Cohort[[1]]$brood_year
Maturation_Uncertainty<-as.data.frame(cbind(brood_year,Maturation_Uncertainty))
names(Maturation_Uncertainty)<-c("brood_year","Mat2Lower","Mat2Mean",  "Mat2Upper", "Mat3Lower", "Mat3Mean","Mat3Upper", "Mat4Lower", "Mat4Mean","Mat4Upper","SDMat2","SDMat3","SDMat4")
write.csv(Maturation_Uncertainty,"Maturation_ ncertainty Natural Sac.csv", row.names = FALSE)
########################################
#Summarizing abundances without fishing for calculating
#Do not need to run for river specific reconstructions
#a)Abundance without cumulative fishing (Unfished)
#b)Abundance without fishing in the current year (no fishing)
#c)Spawning reduction rate without fishing in the current year
#spawner reduction
WOfishing_Bootstrap<-array(NA, c(nrow(Cohort[[1]])-1,6,1000))
for(i in 1:1000){
  for(j in 1:(nrow(Cohort[[1]])-1)){
    WOfishing_Bootstrap[j,1,i]<-Cohort[[1]]$brood_year[j]+4 #run year from brood year
    #never fished 3+
    WOfishing_Bootstrap[j,2,i]<-Cohort[[i]]$Age3NF[1+j]+
      Cohort[[i]]$Age4NF[j]
    #no fishing 3+
    WOfishing_Bootstrap[j,3,i]<-Cohort[[i]]$Age3Hyp[1+j]+
      Cohort[[i]]$Age4Hyp[j]
    #SRR
    WOfishing_Bootstrap[j,4,i]<-1-((Cohort[[i]]$Age2Sp[2+j]+ #actual runs
                                            Cohort[[i]]$Age3Sp[1+j]+
                                            Cohort[[i]]$Age4Sp[j])/
                                           (Cohort[[i]]$Age2NF[2+j]+ #hypothetical runs
                                              Cohort[[i]]$Age3NF[1+j]+
                                              Cohort[[i]]$Age4NF[j]))
    #SRR.y
    WOfishing_Bootstrap[j,5,i]<-1-((Cohort[[i]]$Age2Sp[2+j]+ #actual runs
                                      Cohort[[i]]$Age3Sp[1+j]+
                                      Cohort[[i]]$Age4Sp[j])/
                                     (Cohort[[i]]$Age2NF[2+j]+ #hypothetical runs
                                        Cohort[[i]]$Age3Hyp[1+j]+
                                        Cohort[[i]]$Age4Hyp[j]))
    #September abundance
    WOfishing_Bootstrap[j,6,i]<-((Cohort[[i]]$Age2.10[1+j])/(1-0.0184))/(1-Cohort[[i]]$Imp2_9[1+j])+
                                       ((Cohort[[i]]$Age3.10[j])/(1-0.0184))/(1-Cohort[[i]]$Imp3_9[j])
  }
}
WOfishing_Bootstrap[,,1]
Unfished3<-matrix(nrow = nrow(WOfishing_Bootstrap[,,1]), ncol=4)
#5,50,95 quantile for never fished 3+
for(i in 1:nrow(WOfishing_Bootstrap[,,1])){
  Unfished3[i,1]<-WOfishing_Bootstrap[i,1,1]
  Unfished3[i,c(2,4)]<-quantile(WOfishing_Bootstrap[i,2,c(1:1000)], probs=c(.025,.975), na.rm = TRUE) #Mat 2
  Unfished3[i,3]<-mean(WOfishing_Bootstrap[i,2,c(1:1000)]) #Mat 2
}
colnames(Unfished3)<-c("run_year","Unfished3Lower","Unfished3Mean",  "Unfished3Upper")
write.csv(Unfished3, "Unfished3 Natural.csv",row.names = FALSE)
#5,50,95 quantile for no fishing 3+
NoFishing3<-matrix(nrow = nrow(WOfishing_Bootstrap[,,1]), ncol=4)
for(i in 1:nrow(WOfishing_Bootstrap[,,1])){
  NoFishing3[i,1]<-WOfishing_Bootstrap[i,1,1]
  NoFishing3[i,c(2,4)]<-quantile(WOfishing_Bootstrap[i,3,c(1:1000)], probs=c(.025,.975), na.rm = TRUE) #Mat 2
  NoFishing3[i,3]<-mean(WOfishing_Bootstrap[i,3,c(1:1000)])
}
colnames(NoFishing3)<-c("run_year","Nofishing3Lower","Nofishing3Mean",  "Nofishing3Upper")
write.csv(NoFishing3, "Nofishing3 Natural.csv",row.names = FALSE)
#5,50,95 quantile for SRR
SRR<-matrix(nrow = nrow(WOfishing_Bootstrap[,,1]), ncol=4)
for(i in 1:nrow(WOfishing_Bootstrap[,,1]-1)){
  SRR[i,1]<-WOfishing_Bootstrap[i,1,1]
  SRR[i,c(2,4)]<-quantile(WOfishing_Bootstrap[i,4,c(1:1000)], probs=c(.025,.975), na.rm = TRUE) #Mat 2
  SRR[i,3]<-mean(WOfishing_Bootstrap[i,4,c(1:1000)]) #Mat 2
}

colnames(SRR)<-c("run_year","SRRLower","SRRMean",  "SRRUpper")
write.csv(SRR[1:8,], "SRR Natural.csv",row.names = FALSE)

#5,50,95 quantile for SRR.y
SRR.y<-matrix(nrow = nrow(WOfishing_Bootstrap[,,1]), ncol=4)
for(i in 1:nrow(WOfishing_Bootstrap[,,1]-1)){
  SRR.y[i,1]<-WOfishing_Bootstrap[i,1,1]
  SRR.y[i,c(2,4)]<-quantile(WOfishing_Bootstrap[i,5,c(1:1000)], probs=c(.025,.975), na.rm = TRUE) #Mat 2
  SRR.y[i,3]<-mean(WOfishing_Bootstrap[i,5,c(1:1000)]) #Mat 2
}
colnames(SRR.y)<-c("run_year","SRRyLower","SRRyMean",  "SRRyUpper")
write.csv(SRR.y[1:8,], "SRRy Natural.csv",row.names = FALSE)


#5,50,95 quantile for Sept 1 abundance
Sept1<-matrix(nrow = nrow(WOfishing_Bootstrap[,,1]), ncol=4)
for(i in 1:nrow(WOfishing_Bootstrap[,,1])){
  Sept1[i,1]<-WOfishing_Bootstrap[i,1,1]
  Sept1[i,c(2,4)]<-quantile(WOfishing_Bootstrap[i,6,c(1:1000)], probs=c(.025,.975), na.rm = TRUE)
  Sept1[i,3]<-mean(WOfishing_Bootstrap[i,6,c(1:1000)]) #Mat 2
}
colnames(Sept1)<-c("run_year","Sept1Lower","Sept1Mean",  "Sept1Upper")
write.csv(Sept1, "Sept1 Natural.csv",row.names = FALSE)
########################################
#Spawner to age-2
Prod_Uncertainty_Bootstrap<-array(NA, c(length(Cohort[[1]]$brood_year),3, 1000))
for(i in 1:1000){
  Prod_Uncertainty_Bootstrap[,1,i]<-Cohort[[i]]$Production
  Prod_Uncertainty_Bootstrap[,2,i]<-Cohort[[i]]$Age1.9
  Prod_Uncertainty_Bootstrap[,3,i]<-Cohort[[i]]$Parent_escapement
}
Prod_Uncertainty<-matrix(nrow = length(Cohort[[1]]$brood_year), ncol=9)
for(i in 1:length(Cohort[[1]]$brood_year)){
  for(j in 1:3){
    Prod_Uncertainty[i,c(1+(j-1)*3,2+(j-1)*3,3+(j-1)*3)]<-quantile(Prod_Uncertainty_Bootstrap[i,j,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) #Mat 2
    Prod_Uncertainty[i,2]<-mean(Prod_Uncertainty_Bootstrap[i,1,], na.rm = TRUE)
    Prod_Uncertainty[i,5]<-mean(Prod_Uncertainty_Bootstrap[i,2,], na.rm = TRUE)
    Prod_Uncertainty[i,8]<-mean(Prod_Uncertainty_Bootstrap[i,3,], na.rm = TRUE)
  }
}

brood_year<-Cohort[[1]]$brood_year
Productivity_Uncertainty<-as.data.frame(cbind(brood_year,Prod_Uncertainty))
names(Productivity_Uncertainty)<-c("brood_year","ProdLower","ProdMean",  "ProdUpper","Age2Lower","Age2Mean",  "Age2Upper","ParentLower","ParentMean",  "ParentUpper")
write.csv(Productivity_Uncertainty,"Productivity.csv", row.names = FALSE)
########################################
#Impact numbers
Impact<-list()
for(i in 1:1000){
Impact[[i]]<-Cohort[[i]] %>%
  select(brood_year, Imp2, Imp3, Imp4)
}
saveRDS(Impact, file = "Impact Natural Bootstrap.Rds")

Harvest<-list()
for(i in 1:1000){
  Harvest[[i]]<-Cohort[[i]] %>%
    select(brood_year, Har2, Har3, Har4)
}
saveRDS(Harvest, file = "Harvest Natural Bootstrap.Rds")
