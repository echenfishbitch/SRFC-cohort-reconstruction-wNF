###############################################
#### Natural Age-Specific Escapement  #########
###############################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(beepr)
setwd("C:/Users/echen/Box/Sacramento River Fall Chinook/Methodology Review/")
################################################
####     Escapement to each river     ##########
###############     or hatchery      ###########
################################################
#Hatchery Escapement
Hatchery_Escapement<-read.csv("Hatchery Escapement.csv")
colnames(Hatchery_Escapement)[3]<-"River"
#River Escapement
River_Escapement<-read.csv("River Escapement.csv")
unique(River_Escapement$River)
Total_Escapement<-River_Escapement %>%
  rbind(Hatchery_Escapement)%>%
  filter(run_year <= 2020 & run_year >= 2010) %>%
#for Sacramento River
  filter(River == "SAC R BEL RBDD"|River == "SAC R AB RBDD") %>%
#for American River
  # filter(River == "AMERICAN RIVER") %>%
#for Nimbus Fish Hatchery
  # filter(River == "NIMBUS FISH HATCHERY") %>%
#for Feather River
  # filter(River == "FEATHER RIVER") %>%
#for Feather River Hatchery
  # filter(River == "FEATHER R HATCHERY") %>%
#for Battle Creek and CNFH
  # filter(River == "BATTLE CREEK"| River == "COLEMAN NFH") %>%
  #For Battle Creek, no spawning surveys to recover CWT to estimate Hatchery composition so
  #subtracting hatchery proportion out through ratio of No/Total for CNFH reported in Satterthwaite 2023 and Dean and Lindley 2023
  # mutate(Noprop = c(.11,.11,.09,.10,.11,.26,.17,.13,.13,.10,.13,1,1,1,1,1,1,1,1,1,1,1)) %>% 
  # mutate(Natural = Count*Noprop) %>%
  # select(run_year, Natural) %>%
  # rename(Count = Natural) %>%
#for Yuba River
  # filter(River == "YUBA RIVER") %>%
#for Clear Creek
  # filter(River == "CLEAR CREEK") %>%
#for Other
  # filter(River == "COTTONWOOD"|River == "COW CREEK"|River == "MILL CREEK"|River == "DEER CREEK"|River == "BUTTE CREEK") %>%
#
  group_by(run_year) %>%
  summarise(Total_Escapement = sum(Count))
################################################
## CWT Age Specific Escapement to each river ###
#####          for subtraction          ########
################################################
CWT<-readRDS("Escape to Spawning Grounds by river.Rds") #Hatchery escapement to spawning grounds
CWTH<-read.csv("Escape to Hatchery by hatchery.csv") #Hatchery escapement to hatcheries

#For Rivers  (except Battle Creek which has none)
for(i in 1:1000){
  CWT[[i]]<-CWT[[i]] %>%
    #For Clear Creek
    # filter(recovery_location_name == "CLEAR CREEK")
    #For Yuba River
    # filter(recovery_location_name == "YUBA RIVER")
  #For Sacramento River
  filter(recovery_location_name == "SAC R AB RBDD")
  # For American River
  # filter(recovery_location_name == "AMERICAN RIVER")
  #For Feather River
  # filter(recovery_location_name == "FEATHER RIVER")
}
#Only needed For calculating Escapement to minor tributaries
#These areas are made up of more than one recovery location
# for(i in 1:1000){
#   CWT[[i]]<-CWT[[i]] %>%
#   #For OTHER tributaries  
#     filter(recovery_location_name == "BUTTE CREEK"|recovery_location_name == "COTTONWOOD CREEK"|
#              recovery_location_name == "COW CREEK"|recovery_location_name == "DEER CREEK"|
#              recovery_location_name == "MILL CREEK") %>%
#     group_by(run_year) %>%
#     summarise(recovery_location_name = "OTHER", Age2Sp_CWT = sum(Age2Sp_CWT, na.rm = TRUE),Age3Sp_CWT = sum(Age3Sp_CWT, na.rm = TRUE), Age4Sp_CWT = sum(Age4Sp_CWT, na.rm = TRUE),Age5Sp_CWT = sum(Age5Sp_CWT, na.rm = TRUE),
#       Age2Sp_Hatchery = sum(Age2Sp_Hatchery, na.rm = TRUE),Age3Sp_Hatchery = sum(Age3Sp_Hatchery, na.rm = TRUE), Age4Sp_Hatchery = sum(Age4Sp_Hatchery, na.rm = TRUE),Age5Sp_Hatchery = sum(Age5Sp_Hatchery, na.rm = TRUE))
# }
##################################################
######    Unmarked hatchery-origin fish     ######  
######    Age Specific Escapement to each river ##  
##################################################
Escapement<-list() #general computations list (used to make Escapement_NCWT and Escapement_Unmarked)
Escapement_Unmarked<-list() #total escapement that is unmarked (needed for proportioning scale ages)
Escapement_NCWT<-list() #age-specific escapement of unmarked hatchery fish (needed for subtracting from unmarked age-specific escapement)

#For River sites
for(i in 1:1000){
Escapement[[i]]<-Total_Escapement %>%
  left_join(CWT[[i]] %>%
              mutate(CWT_Escapement = Age2Sp_CWT+Age3Sp_CWT+Age4Sp_CWT+Age5Sp_CWT) %>% #for calculating unmarked escapement
              #Calculate the number of hatchery fish unmarked and with no CWT
              #by subtracting age-specific hatchery fish with CWT from all age-specific hatchery fish
              mutate(Age2Sp_NCWT = Age2Sp_Hatchery-Age2Sp_CWT) %>%
              mutate(Age3Sp_NCWT = Age3Sp_Hatchery-Age3Sp_CWT) %>%
              mutate(Age4Sp_NCWT = Age4Sp_Hatchery-Age4Sp_CWT) %>%
              mutate(Age5Sp_NCWT = Age5Sp_Hatchery-Age5Sp_CWT)) %>%
              select(-recovery_location_name)
Escapement[[i]][is.na(Escapement[[i]])]<-0 #only needed for OTHER
Escapement[[i]]<-Escapement[[i]] %>%
  mutate(Unmarked_Escapement = Total_Escapement - CWT_Escapement)
Escapement_Unmarked[[i]]<-Escapement[[i]] %>% select(run_year, Unmarked_Escapement)
Escapement_NCWT[[i]]<-Escapement[[i]] %>% 
  select(run_year, Age2Sp_NCWT,Age3Sp_NCWT,Age4Sp_NCWT, Age5Sp_NCWT) 
Escapement_NCWT[[i]][is.na(Escapement_NCWT[[i]])] <- 0
}  
#For Nimbus Fish Hatchery and Feather River Hatchery and CNFH/Battle Creek
# Escapement<-Total_Escapement %>%
#   left_join(CWTH %>%
#               # filter(recovery_location_name == "NIMBUS FISH HATCHERY") %>%
#               filter(recovery_location_name == "FEATHER R HATCHERY") %>%
#               # filter(recovery_location_name == "COLEMAN NFH") %>%
#               mutate(CWT_Escapement = Age2Sp_CWT+Age3Sp_CWT+Age4Sp_CWT+Age5Sp_CWT) %>% #for calculating unmarked escapement
#               #Calculate the number of hatchery fish unmarked and with no CWT
#               #by subtracting age-specific hatchery fish with CWT from all age-specific hatchery fish
#               mutate(Age2Sp_NCWT = Age2Sp_Hatchery-Age2Sp_CWT) %>%
#               mutate(Age3Sp_NCWT = Age3Sp_Hatchery-Age3Sp_CWT) %>%
#               mutate(Age4Sp_NCWT = Age4Sp_Hatchery-Age4Sp_CWT) %>%
#               mutate(Age5Sp_NCWT = Age5Sp_Hatchery-Age5Sp_CWT)) %>%
#               mutate(Unmarked_Escapement = Total_Escapement - CWT_Escapement)
# Escapement_Unmarked<-Escapement %>% select(run_year, Unmarked_Escapement)
# Escapement_NCWT<-Escapement %>% 
#   select(run_year, Age2Sp_NCWT,Age3Sp_NCWT,Age4Sp_NCWT, Age5Sp_NCWT) 
##################################################
#############         Unmarked        ############  
######    Age Specific Escapement to each river ##  
##################################################
scaledata<-read.csv("scale ages.csv")
table(scaledata$River)
scaledata<-scaledata %>%
#for Sacramento River
  filter(River == "SAC R AB RBDD")
#for American River
  # filter(River == "AMERICAN RIVER")
#for Nimbus
  # filter(River == "NFH")
#for Feather River
  # filter(River == "FEATHER RIVER")
#for Feather River Hatchery
  # filter(River == "FRH")
#for Battle Creek and CNFH
  # filter(River == "CNFH")
#for Yuba River
  # filter(River == "YUBA RIVER")
#for Clear Creek
  # filter(River == "CLEAR CREEK")
#for Other
  # filter(River == "COTTONWOOD"|River == "COW CREEK"|River == "MILL CREEK"|River == "DEER CREEK"|River == "BUTTE CREEK")
#validation matrix
validated<-scaledata %>%
  filter(CWT.Age != 0) %>%
  filter(Reader.Age != 0)%>%
  filter(CWT.Age !=5) %>%
  filter(Reader.Age !=5) %>%
  filter(CWT.Age !=1) %>%
  filter(Reader.Age !=1) 
#
valid.mat<-as.matrix(table(validated$Reader.Age,validated$CWT.Age)) #Header is Known Age, Side is Read Age
a=3#length(read.ages)
n=a #"length" categories are really scale-read ages
q=array(NA,c(n,a))
for (i in 1:n)
{
  for (j in 1:a)
  {
    q[i,j]=as.numeric(valid.mat[i,j])
  }
}
for (j in 1:a) q[,j]=q[,j]/colSums(q)[j]

#read ages for unmarked fish
scaledata<-scaledata %>%
  filter(Supplement == "N") %>%
  filter(is.na(CWT.Code)) %>%
  filter(Reader.Age !=1) %>%
  filter(Reader.Age !=0) %>%
  filter(Reader.Age !=5) 
read_year<-split(scaledata, f=scaledata$run_year) #splitting the data by year for age-comp each run year  
scaleages<-list() #resampled scale ages
adjustedages<-list() #adjusted scale ages matrices from resampling
Escapement_Natural<-list() #age-specific escapement of natural-origin fish
yearhead<-as.character(unique(scaledata$run_year))
 for(z in 1:1000){
  scaleages[[z]]<-matrix(NA, nrow = a, ncol = length(unique(scaledata$run_year))+1) #one column for each year, plus one header column
  scaleages[[z]][,1]<-2:4 #header column
  adjustedages[[z]]<-matrix(NA, nrow = length(unique(scaledata$run_year)), ncol= a+1) #one column for each year, plus one header column
  adjustedages[[z]][,1]<-as.integer(yearhead)
  for(y in 1:length(unique(scaledata$run_year))){ #for each run year
    #Resampling
    resample<-sample(read_year[[y]]$Reader.Age, length(read_year[[y]]$Reader.Age), replace = TRUE)
    #Calculating raw age proportions
    scaleages[[z]][,y+1]<-c(length(resample[which(resample ==2)])/length(resample),
                          length(resample[which(resample ==3)])/length(resample),
                          length(resample[which(resample ==4)])/length(resample))
  # For if you're doing age adjustments
    p.hat=array(1/a,a)
    #IALK algorithm
    max.iterations=500
    p.chain=array(NA,c(max.iterations,a))
    l.hat=array(NA,n) #placeholder, l.hat updated within loop
    Pr.hat=array(NA,c(n,a)) #placeholder, Pr.hat updated within loop
    iteration=1
    bailout=0

    while ((iteration+bailout)<(max.iterations+1))
    {#iteration while loop
      p.chain[iteration,]=p.hat
      for (i in 1:n)
      {#loop over n
        l.hat[i]=sum(p.hat*q[i,])
      }#loop over n
      p.hat.new=p.hat
      p.hat.old=p.hat
      for (j in 1:a)
      {#loop over a
        cum.sum=0
        for (i in 1:n)
        {#loop over n
          cum.sum=cum.sum+scaleages[[z]][i,y+1]*p.hat[j]*q[i,j]/l.hat[i]
        }#loop over n
        p.hat.new[j]=cum.sum
      }#loop over a
      p.hat=p.hat.new
      iteration=iteration+1
      if (max(abs(p.hat.new-p.hat.old),na.rm=TRUE)<0.00005) bailout=max.iterations
    }#iteration while loop
    adjustedages[[z]][y,2:4]<-p.hat
  }
  #reorganizing age compositions by run year
  adjustedages[[z]]<-as.data.frame(adjustedages[[z]])
  colnames(adjustedages[[z]])<-c("run_year","Age2Sp_prop","Age3Sp_prop","Age4Sp_prop")
  ##################################################
  #############    Natural-origin       ############  
  ######    Age Specific Escapement to each river ##  
  ##################################################
  Escapement_Natural[[z]]<-adjustedages[[z]] %>%
    #proportioning unmarked escapement by scale ages
    # left_join(Escapement_Unmarked[[z]]) %>% #not a list for hatcheries
    left_join(Escapement_Unmarked) %>% #not a list for hatcheries
    mutate(Age2Sp_unmarked = Age2Sp_prop*Unmarked_Escapement) %>%
    mutate(Age3Sp_unmarked = Age3Sp_prop*Unmarked_Escapement) %>%
    mutate(Age4Sp_unmarked = Age4Sp_prop*Unmarked_Escapement) %>%
    #subtracting unmarked hatchery fish to get natural origin fish
    # left_join(Escapement_NCWT[[z]])%>% #not a list for hatcheries
    left_join(Escapement_NCWT)%>% #not a list for hatcheries
    mutate(`2` = Age2Sp_unmarked-Age2Sp_NCWT) %>%
    mutate(`3` = Age3Sp_unmarked-Age3Sp_NCWT) %>%
    mutate(`4` = Age4Sp_unmarked-Age4Sp_NCWT) %>%
    #in the event of negative values make them 0
    mutate(`2` = ifelse(`2` < 0, 0, `2`)) %>%
    mutate(`3` = ifelse(`3` < 0, 0, `3`)) %>%
    mutate(`4` = ifelse(`4` < 0, 0, `4`)) %>%
    select(run_year, `2`, `3`, `4`) %>%
    pivot_longer(cols = c(`2`, `3`, `4`), names_to = "Age", values_to = "Spawners") %>%
    mutate(brood_year = run_year - as.numeric(Age)) %>%
    select(brood_year, Age, Spawners) %>%
    pivot_wider(names_from = Age, values_from = Spawners, names_sort=TRUE) %>%
    group_by(brood_year) %>%
    summarize(Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE), Age4Sp = sum(`4`, na.rm = TRUE))
 }
saveRDS(Escapement_Natural, file = "Natural SpawnersAdjusted FRH.Rds")
test<-Escapement_Natural[[1]]
################
# Once all files have been created
# Merging all natural production together
SpawnersA<-readRDS("Natural SpawnersAdjusted Feather.Rds")
SpawnersB<-readRDS("Natural SpawnersAdjusted Sac.Rds")
SpawnersC<-readRDS("Natural SpawnersAdjusted American.Rds")
SpawnersD<-readRDS("Natural SpawnersAdjusted Clear.Rds")
SpawnersE<-readRDS("Natural SpawnersAdjusted Yuba.Rds")
SpawnersF<-readRDS("Natural SpawnersAdjusted Battle.Rds")
SpawnersG<-readRDS("Natural SpawnersAdjusted Other.Rds")
SpawnersH<-readRDS("Natural SpawnersAdjusted FRH.Rds")
SpawnersI<-readRDS("Natural SpawnersAdjusted NFH.Rds")

Spawners<-list()
for(i in 1:1000){
  Spawners[[i]]<-rbind(SpawnersA[[i]], rbind(SpawnersB[[i]]),rbind(SpawnersC[[i]], rbind(SpawnersD[[i]], rbind(SpawnersE[[i]], rbind(SpawnersF[[i]], rbind(SpawnersG[[i]], rbind(SpawnersH[[i]], SpawnersI[[i]])))))))
  Spawners[[i]]<-Spawners[[i]] %>%
    group_by(brood_year) %>%
    summarise(Age2Sp = sum(Age2Sp), Age3Sp = sum(Age3Sp), Age4Sp = sum(Age4Sp))
}
saveRDS(Spawners, file = "Natural SpawnersAdjusted.Rds")
