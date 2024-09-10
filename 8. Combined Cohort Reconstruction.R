#####################################
###Combined Cohort Reconstruction ###
#####################################
#only needed for calculating SRR for both components
library(dplyr)
library(tidyr)
setwd("C:/Users/echen/Box/Sacramento River Fall Chinook/Methodology Review")
Hatchery<-readRDS("CWT Spawning Grounds.Rds") #Spawning ground escapement of hatchery fish
To_Hatchery<-read.csv("CWT Hatchery.csv") #Hatchery Escapement of hatchery fish
Natural<-readRDS("Natural SpawnersAdjusted.Rds") #Both hatchery and spawning ground escapement of natural origin fish
Spawners<-list()
#combining hatchery and natural-origin escapement
for(i in 1:1000){
Spawners[[i]]<-Hatchery[[i]] %>%
  rbind(Natural[[i]] %>% mutate(Age1Sp = 0)%>% mutate(Age5Sp = 0)) %>% #no age 1 and age 5 natural origin fish
  group_by(brood_year) %>%
  summarise(Age1Sp = sum(Age1Sp),Age2Sp = sum(Age2Sp), Age3Sp = sum(Age3Sp), Age4Sp = sum(Age4Sp), Age5Sp = sum(Age5Sp)) %>%
  left_join(To_Hatchery, by="brood_year") 
}
Impact<-readRDS("CWT monthly impact rates.Rds")
Cohort<-list()
for(i in 1:1000){
Cohort[[i]]<-Spawners[[i]] %>%
  left_join(Impact[[i]])%>%
  filter(brood_year >= 2006 & brood_year <= 2018)
#5 Year Old Fishing
Cohort[[i]]$Age5.10<-Cohort[[i]]$Imp5_10 
# 5 Year Old Spawners
Cohort[[i]]$Age5.9<-((Cohort[[i]]$Age5.10+(Cohort[[i]]$Age5Sp+Cohort[[i]]$Age5Hat)/(1-Cohort[[i]]$ImpRiv5))/(1-0.0184))/(1-Cohort[[i]]$Imp5_9) 
Cohort[[i]]$Age5.9[3]<-((Cohort[[i]]$Age5.10[3]+(Cohort[[i]]$Age5Sp[3]+Cohort[[i]]$Age5Hat[3])/(1-Cohort[[i]]$ImpRiv5[3]))/(1-0.0184))+10.2 #2008 impact rate is 100% so using N instead of rate. No Natural origin age 5 fish anyways
#Fourth Year at Sea. 5 year olds are caught
Cohort[[i]]$Age5.8<-(Cohort[[i]]$Age5.9/(1-0.0184))/(1-Cohort[[i]]$Imp5_8)
Cohort[[i]]$Age5.7<-(Cohort[[i]]$Age5.8/(1-0.0184))/(1-Cohort[[i]]$Imp5_7)
Cohort[[i]]$Age5.6<-(Cohort[[i]]$Age5.7/(1-0.0184))/(1-Cohort[[i]]$Imp5_6)
Cohort[[i]]$Age5.5<-(Cohort[[i]]$Age5.6/(1-0.0184))/(1-Cohort[[i]]$Imp5_5)
Cohort[[i]]$Age5.4<-(Cohort[[i]]$Age5.5/(1-0.0184))/(1-Cohort[[i]]$Imp5_4)
Cohort[[i]]$Age5.3<-(Cohort[[i]]$Age5.4/(1-0.0184))/(1-Cohort[[i]]$Imp5_3)
Cohort[[i]]$Age5.2<-(Cohort[[i]]$Age5.3/(1-0.0184))/(1-Cohort[[i]]$Imp5_2)
Cohort[[i]]$Age4.11<-(Cohort[[i]]$Age5.2/(1-0.0543))/(1-Cohort[[i]]$Imp4_11) #95% survival for 3 months
Cohort[[i]]$Age4.10<-(Cohort[[i]]$Age4.11/(1-0.0184))/(1-Cohort[[i]]$Imp4_10)

#Start of September Age-4, fish get fished, natural-mortality, remaining spawn at end of September
Cohort[[i]]$Age4.9<-((Cohort[[i]]$Age4.10+(Cohort[[i]]$Age4Sp+Cohort[[i]]$Age4Hat)/(1-Cohort[[i]]$ImpRiv4))/(1-0.0184))/(1-Cohort[[i]]$Imp4_9) #Remove InRiver5 for NFH
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
Cohort[[i]]$Age3.9<-((((Cohort[[i]]$Age3Sp+Cohort[[i]]$Age3Hat)/(1-Cohort[[i]]$ImpRiv3))+Cohort[[i]]$Age3.10)/(1-0.0184))/(1-Cohort[[i]]$Imp3_9)
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
Cohort[[i]]$Age2.9<-((((Cohort[[i]]$Age2Sp+Cohort[[i]]$Age2Hat)/(1-Cohort[[i]]$ImpRiv2))+Cohort[[i]]$Age2.10)/(1-0.0184))/(1-Cohort[[i]]$Imp2_9)
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
Cohort[[i]]$Mat4<-((Cohort[[i]]$Age4Sp+Cohort[[i]]$Age4Hat)/(1-Cohort[[i]]$ImpRiv4))/(((Cohort[[i]]$Age4Sp+Cohort[[i]]$Age4Hat)/(1-Cohort[[i]]$ImpRiv4))+Cohort[[i]]$Age4.10/(1-0.0184))
Cohort[[i]]$Mat3<-((Cohort[[i]]$Age3Sp+Cohort[[i]]$Age3Hat)/(1-Cohort[[i]]$ImpRiv3))/(((Cohort[[i]]$Age3Sp+Cohort[[i]]$Age3Hat)/(1-Cohort[[i]]$ImpRiv3))+Cohort[[i]]$Age3.10/(1-0.0184))
Cohort[[i]]$Mat2<-((Cohort[[i]]$Age2Sp+Cohort[[i]]$Age2Hat)/(1-Cohort[[i]]$ImpRiv2))/(((Cohort[[i]]$Age2Sp+Cohort[[i]]$Age2Hat)/(1-Cohort[[i]]$ImpRiv2))+Cohort[[i]]$Age2.10/(1-0.0184))
#Spawner reduction rates
Cohort[[i]]$Age2NF<-Cohort[[i]]$Age2.4*(1-0.0561)^5*Cohort[[i]]$Mat2
Cohort[[i]]$Age3NF<-(Cohort[[i]]$Age2.4*(1-0.0561)^5*(1-Cohort[[i]]$Mat2))*(1-0.0184)^12*Cohort[[i]]$Mat3
Cohort[[i]]$Age4NF<-(Cohort[[i]]$Age2.4*(1-0.0561)^5*(1-Cohort[[i]]$Mat2))*(1-0.0184)^12*(1-Cohort[[i]]$Mat3)*(1-0.0184)^12*Cohort[[i]]$Mat4
#Immediate abundance w/o fishing
Cohort[[i]]$Age3Hyp<-Cohort[[i]]$Age3.4*(1-0.0184)^5*Cohort[[i]]$Mat3
Cohort[[i]]$Age4Hyp<-Cohort[[i]]$Age4.4*(1-0.0184)^5*Cohort[[i]]$Mat4
}
#End
########################################
#Summarizing abundances without fishing for calculating
#a)Abundance without cumulative fishing (Unfished)
#b)Abundance without fishing in the current year (no fishing)
#c)Spawning reduction rate without fishing in the current year
#spawner reduction
WOfishing_Bootstrap<-array(NA, c(nrow(Cohort[[1]])-1,3,1000))
for(i in 1:1000){
  for(j in 1:(nrow(Cohort[[1]])-1)){
    WOfishing_Bootstrap[j,1,i]<-Cohort[[1]]$brood_year[j]+4 #run year from brood year
    #SRR
    WOfishing_Bootstrap[j,2,i]<-1-((Cohort[[i]]$Age2Sp[2+j]+Cohort[[i]]$Age2Hat[2+j]+ #actual runs
                                            Cohort[[i]]$Age3Sp[1+j]+Cohort[[i]]$Age3Hat[1+j]+
                                            Cohort[[i]]$Age4Sp[j]+Cohort[[i]]$Age4Hat[j])/
                                           (Cohort[[i]]$Age2NF[2+j]+ #hypothetical runs
                                              Cohort[[i]]$Age3NF[1+j]+
                                              Cohort[[i]]$Age4NF[j]))
    #SRR.y
    WOfishing_Bootstrap[j,3,i]<-1-((Cohort[[i]]$Age2Sp[2+j]+Cohort[[i]]$Age2Hat[2+j]+ #actual runs
                                      Cohort[[i]]$Age3Sp[1+j]+Cohort[[i]]$Age3Hat[1+j]+
                                      Cohort[[i]]$Age4Sp[j]+Cohort[[i]]$Age4Hat[j])/
                                     (Cohort[[i]]$Age2NF[2+j]+ #hypothetical runs
                                        Cohort[[i]]$Age3Hyp[1+j]+
                                        Cohort[[i]]$Age4Hyp[j]))
    }
}

#5,50,95 quantile for SRR
SRR<-matrix(nrow = nrow(WOfishing_Bootstrap[,,1]), ncol=4)
for(i in 1:nrow(WOfishing_Bootstrap[,,1]-1)){
  SRR[i,1]<-WOfishing_Bootstrap[i,1,1]
  SRR[i,c(2,4)]<-quantile(WOfishing_Bootstrap[i,2,c(1:1000)], probs=c(.025,.975), na.rm = TRUE) #Mat 2
  SRR[i,3]<-mean(WOfishing_Bootstrap[i,2,c(1:1000)]) #Mat 2
}
colnames(SRR)<-c("run_year","SRRLower","SRRMean",  "SRRUpper")
write.csv(SRR, "SRR.csv",row.names = FALSE)

#5,50,95 quantile for SRR.y
SRR.y<-matrix(nrow = nrow(WOfishing_Bootstrap[,,1]), ncol=4)
for(i in 1:nrow(WOfishing_Bootstrap[,,1]-1)){
  SRR.y[i,1]<-WOfishing_Bootstrap[i,1,1]
  SRR.y[i,c(2,4)]<-quantile(WOfishing_Bootstrap[i,3,c(1:1000)], probs=c(.025,.975), na.rm = TRUE) #Mat 2
  SRR.y[i,3]<-mean(WOfishing_Bootstrap[i,3,c(1:1000)]) #Mat 2
}
colnames(SRR.y)<-c("run_year","SRRyLower","SRRyMean",  "SRRyUpper")
write.csv(SRR.y, "SRRy.csv",row.names = FALSE)

