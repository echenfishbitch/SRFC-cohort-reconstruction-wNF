library(dplyr)
library(ggplot2)
library(tidyr)
setwd("C:/Users/echen/Box/Sacramento River Fall Chinook/Methodology Review/")
blank_bg<-theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
###################################################
## Compare SI to age 3+ abundance in September ####
### and Potential Escapement ######################
###################################################
SIdata<-read.csv("SIdata.csv")
SI<-SIdata %>%
  select(Year, PostSI)
colnames(SI)[1]<-"run_year"
UnfishedH<-read.csv("Unfished3 CWT.csv")
UnfishedN<-read.csv("Unfished3 Natural.csv")
Unfished<-rbind(UnfishedH, UnfishedN)
Unfished<-Unfished %>%
  group_by(run_year) %>%
  summarise(Unfished = sum(Unfished3Mean)/1000) %>%
  filter(run_year %in% 2012:2019)
NofishingH<-read.csv("Nofishing3 CWT.csv")
NofishingN<-read.csv("Nofishing3 Natural.csv")
Nofishing<-rbind(NofishingH, NofishingN)
Nofishing<-Nofishing %>%
  group_by(run_year) %>%
  summarise(Nofishing = sum(Nofishing3Mean)/1000) %>%
  filter(run_year %in% 2010:2019)
Sept1H<-read.csv("Sept1 CWT.csv")
Sept1N<-read.csv("Sept1 Natural.csv")
Sept1<-rbind(Sept1H, Sept1N)
Sept1<-Sept1 %>%
  group_by(run_year) %>%
  summarise(Sept1 = sum(Sept1Mean)/1000) %>%
  filter(run_year %in% 2010:2019)

Abundance<-Sept1 %>%
  left_join(Nofishing) %>%
  left_join(Unfished) %>%
  left_join(SI)%>%
  mutate(SIvsSept = (Sept1-PostSI)/PostSI) %>%
  mutate(SIvsPE = (Nofishing-PostSI)/PostSI) 
median(Abundance$SIvsPE)  
cor(Abundance$Nofishing, Abundance$PostSI)
cor(Abundance$Sept1, Abundance$PostSI)
Abundance$Unfished[1:2]<-"incomplete"
write.csv(Abundance, "Abundances.csv", row.names = FALSE)
###################################################
####### Compare exploitation rate to SRR ##########
###################################################
Exploit<-SIdata %>%
  select(Year, ExploitR) %>%
  mutate(ExploitR = ExploitR/100) %>%
  rename(run_year = Year)
SRR<-read.csv("SRR.csv")
SRR.y<-read.csv("SRRy.csv")

Exploit<-SRR.y %>%
  select(run_year, SRRyMean) %>%
  rename(SRRy = SRRyMean) %>%
  left_join(SRR %>% select(run_year, SRRMean) %>% filter(run_year %in% 2012:2019) %>% rename(SRR = SRRMean)) %>%
  left_join(Exploit) %>%
  filter(run_year < 2020)
Exploit$SRR[1:2]<-"incomplete"
write.csv(Exploit, "Exploitation.csv", row.names = FALSE)

###################################################
####### Compare ocean harvest and impact to #######
##########      PFMC Harvest         ##############
###################################################
setwd("C:/Users/echen/Box/Sacramento River Fall Chinook/Methodology Review/")
HatcheryImpact<-readRDS("Impact CWT Bootstrap.Rds")
NaturalImpact<-readRDS("Impact Natural Bootstrap.Rds")
Impact<-list()
for(i in 1:1000){ 
  #summarizing hatchery impact
  Impact[[i]]<-HatcheryImpact[[i]] %>%
    mutate(Imp2 = Apr2+May2+Jun2+Jul2+Aug2) %>%
    mutate(Imp3 = Sept2+Oct2+Nov2+Dec2+Jan3+Feb3+Mar3+Apr3+May3+Jun3+Jul3+Aug3) %>%
    mutate(Imp4 = Sept3+Oct3+Nov3+Dec3+Feb4+Mar4+Apr4+May4+Jun4+Jul4+Aug4) %>%
    mutate(Imp5 = Sept4+Oct4+Nov4+Feb5+Mar5+Apr5+May5+Jun5+Jul5+Aug5) %>%
    select(brood_year, Imp2, Imp3, Imp4, Imp5) %>%
    #adding natural impact
    rbind(NaturalImpact[[i]] %>% mutate(Imp5 = 0)) %>%
    group_by(brood_year) %>%
    summarise(Imp2 = sum(Imp2), Imp3 = sum(Imp3), Imp4 = sum(Imp4), Imp5 = sum(Imp5)) %>%
    rename(`2`= Imp2) %>%
    rename(`3`= Imp3) %>%
    rename(`4`= Imp4) %>%
    rename(`5`= Imp5) %>%
    pivot_longer(cols = c(`2`, `3`, `4`, `5`), names_to = "Age", values_to = "Impact") %>%
    mutate(run_year = as.numeric(Age)+brood_year) %>%
    pivot_wider(names_from = Age, values_from = Impact, names_sort=TRUE) %>%
    group_by(run_year) %>%
    #combining impact across ages
    summarize(Imp = sum(`3`, na.rm = TRUE)+sum(`4`, na.rm = TRUE)+sum(`5`, na.rm = TRUE)) %>%
    filter(run_year %in% 2010:2020)
}
#converting from list to frame
Impact_Bootstrap<-matrix(nrow = length(Impact[[1]]$run_year), ncol=1000)
for(i in 1:1000){
  Impact_Bootstrap[,i]<-Impact[[i]]$Imp
}
#calculating mean and 95% CrI
Impact_Uncertainty<-matrix(nrow = length(Impact[[1]]$run_year), ncol=3)
for(i in 1:length(Impact[[1]]$run_year)){
  Impact_Uncertainty[i,]<-quantile(Impact_Bootstrap[i,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) 
  Impact_Uncertainty[i,2]<-mean(Impact_Bootstrap[i,], na.rm = TRUE)
}
Impact_Uncertainty<-as.data.frame(cbind(Impact[[1]]$run_year,Impact_Uncertainty))
names(Impact_Uncertainty)<-c("Year","ImpLower","Impact",  "ImpUpper")

HatcheryHarvest<-readRDS("Harvest Bootstrap.Rds")
NaturalHarvest<-readRDS("Harvest Natural Bootstrap.Rds")
Harvest<-list()
for(i in 1:1000){
  Harvest[[i]]<-HatcheryHarvest[[i]] %>%
    #summarizing hatchery harvest
    mutate(Har2 = Apr2+May2+Jun2+Jul2+Aug2) %>%
    mutate(Har3 = Sept2+Oct2+Nov2+Dec2+Feb3+Mar3+Apr3+May3+Jun3+Jul3+Aug3) %>%
    mutate(Har4 = Sept3+Oct3+Nov3+Feb4+Mar4+Apr4+May4+Jun4+Jul4+Aug4) %>%
    mutate(Har5 = Sept4+Oct4+Nov4+Mar5+Apr5+May5+Jun5+Jul5+Aug5) %>%
    select(brood_year, Har2, Har3, Har4, Har5) %>%
    #adding natural harvest
    rbind(NaturalHarvest[[i]] %>% mutate(Har5 = 0)) %>%
    group_by(brood_year) %>%
    summarise(Har2 = sum(Har2), Har3 = sum(Har3), Har4 = sum(Har4), Har5 = sum(Har5)) %>%
    rename(`2`= Har2) %>%
    rename(`3`= Har3) %>%
    rename(`4`= Har4) %>%
    rename(`5`= Har5) %>%
    pivot_longer(cols = c(`2`, `3`, `4`, `5`), names_to = "Age", values_to = "Harvest") %>%
    mutate(run_year = as.numeric(Age)+brood_year) %>%
    #combining harvest across ages
    pivot_wider(names_from = Age, values_from = Harvest, names_sort=TRUE) %>%
    group_by(run_year) %>%
    summarize(Har = sum(`3`, na.rm = TRUE)+sum(`4`, na.rm = TRUE)+sum(`5`, na.rm = TRUE)) 
}
Harvest_Bootstrap<-matrix(nrow = length(Harvest[[1]]$run_year), ncol=1000)
for(i in 1:1000){
  Harvest_Bootstrap[,i]<-Harvest[[i]]$Har
}
Harvest_Uncertainty<-matrix(nrow = length(Harvest[[1]]$run_year), ncol=3)
for(i in 1:length(Harvest[[1]]$run_year)){
  Harvest_Uncertainty[i,]<-quantile(Harvest_Bootstrap[i,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) 
  Harvest_Uncertainty[i,2]<-mean(Harvest_Bootstrap[i,], na.rm = TRUE)
}
Harvest_Uncertainty<-as.data.frame(cbind(Harvest[[1]]$run_year,Harvest_Uncertainty))
names(Harvest_Uncertainty)<-c("Year","HarvLower","Harvest",  "HarvUpper")
Harvest<-Harvest_Uncertainty %>%
  mutate(Harvest = Harvest/1000) %>%
  select(Year,Harvest) %>%
  left_join(Impact_Uncertainty %>%
              mutate(Impact = Impact/1000) %>%
              select(Year,Impact)) %>%
  left_join(SIdata %>%
              select(Year, OceanHarvest)) %>%
  filter(Year %in% 2010:2020)%>%
  rename(run_year = Year) %>%
  mutate(HarvestvsSI = (Harvest-OceanHarvest)/OceanHarvest) %>%
  mutate(ImpvsSI = (Impact-OceanHarvest)/OceanHarvest)
mean(Harvest$ImpvsSI)
cor(Harvest$Impact,Harvest$OceanHarvest)
write.csv(Harvest, "Harvest.csv", row.names = FALSE)

