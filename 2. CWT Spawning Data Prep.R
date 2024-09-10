###############################################
############## CWT Reconstruction #############
#################CWT Data Prep#################
###############################################
library(dplyr)
library(tidyr)
setwd("C:/Users/echen/Box/Sacramento River Fall Chinook/Methodology Review")
#############################################
#####        Release data           #########
#############################################
CWT_Releases<-read.csv("CWTReleased.csv")
Releases<-CWT_Releases %>%
  group_by(hatchery_location_name, brood_year) %>%
  summarise(N = sum(Total_Released)) %>%
  filter(brood_year > 1997 & brood_year < 2017) %>%
  group_by(hatchery_location_name, brood_year) %>%
  summarise(N= sum(N)) %>%
  group_by(hatchery_location_name) %>%
  summarise(avg = mean(N))
#############################################
#####      Reading recovery data    #########
#############################################
CWT_Recoveries_NFH<-read.csv("CWTRecoveries NFH.csv")
CWT_Recoveries_CNFH<-read.csv("CWTRecoveries CNFH.csv")
CWT_Recoveries_FRH<-read.csv("CWTRecoveries FRH.csv")
CWT_Recoveries<-rbind(CWT_Recoveries_CNFH, rbind(CWT_Recoveries_FRH, CWT_Recoveries_NFH))
#merging Phi batch info with recovery data
CWT_Recoveries$tag_code<-as.character(CWT_Recoveries$tag_code)
CWT_Recoveries<-left_join(CWT_Recoveries, CWT_Releases)
#for ones where there is no batch specific Phi, use year and hatchery specific Phi
for(i in 1:nrow(CWT_Recoveries)){
  if(is.na(CWT_Recoveries$Phi[i])){
    CWT_Recoveries$Phi[i]<-CWT_Releases$Phi[which(CWT_Releases$hatchery_location_name == CWT_Recoveries$hatchery_location_name[i] & CWT_Releases$brood_year == CWT_Recoveries$brood_year[i])]
  }
}
CWT_Recoveries <- CWT_Recoveries %>%
  mutate(Value_Expanded = 1/Phi) 
#############################################
##########Escapement to Hatchery#############
#############################################
CWT_Hatchery<-CWT_Recoveries %>%
  filter(fishery ==50 & run_year > 1999) %>%
  # filter(hatchery_location_name == "NIMBUS FISH HATCHERY") %>% #For when obtaining hatchery-specific escapement
  mutate(estimated_number = ifelse(is.na(estimated_number), 1 , estimated_number)) %>%
  group_by(run_year, brood_year) %>%
  summarise(Escapement_to_Hatchery = sum(estimated_number/Phi)) %>%
  mutate(Age = run_year-brood_year)%>%
  pivot_wider(names_from = Age, values_from = Escapement_to_Hatchery, names_sort=TRUE) %>%
  group_by(brood_year) %>%
  summarize(Age1Hat = sum(`1`, na.rm = TRUE), Age2Hat = sum(`2`, na.rm = TRUE), Age3Hat = sum(`3`, na.rm = TRUE), Age4Hat = sum(`4`, na.rm = TRUE), Age5Hat = sum(`5`, na.rm = TRUE))
# write.csv(CWT_Hatchery, file = "CWT Hatchery.csv", row.names = FALSE)
#for natural-origin age-specific escapement
CWT_Escapement<-CWT_Recoveries %>%
  filter(fishery ==50 & run_year > 1999) %>%
  mutate(Age = run_year-brood_year)%>%
  mutate(estimated_number = ifelse(is.na(estimated_number), 1 , estimated_number)) %>%
  group_by(run_year, brood_year, Age, recovery_location_name) %>%
  summarise(CWT = sum(estimated_number)) %>%
  pivot_wider(names_from = Age, values_from = CWT, names_sort=TRUE) %>%
  group_by(run_year, recovery_location_name) %>%
  summarize(Age2Sp_CWT = sum(`2`, na.rm = TRUE), Age3Sp_CWT = sum(`3`, na.rm = TRUE)
            , Age4Sp_CWT = sum(`4`, na.rm = TRUE), Age5Sp_CWT = sum(`5`, na.rm = TRUE))%>% 
left_join(CWT_Recoveries %>%
            filter(fishery ==50 & run_year > 1999) %>%
            mutate(Age = run_year-brood_year) %>%
            group_by(run_year, brood_year, Age, recovery_location_name) %>%
            summarise(Hatchery = sum(estimated_number/Phi)) %>%
            pivot_wider(names_from = Age, values_from = Hatchery, names_sort=TRUE) %>%
            group_by(run_year, recovery_location_name) %>%
            summarize(Age2Sp_Hatchery = sum(`2`, na.rm = TRUE), Age3Sp_Hatchery = sum(`3`, na.rm = TRUE)
                      , Age4Sp_Hatchery = sum(`4`, na.rm = TRUE), Age5Sp_Hatchery = sum(`5`, na.rm = TRUE))) %>%
  filter(recovery_location_name == "COLEMAN NFH"|recovery_location_name == "FEATHER R HATCHERY"|recovery_location_name == "NIMBUS FISH HATCHERY") %>%
  filter(run_year %in% 2010:2020)
# write.csv(CWT_Escapement, "Escape to Hatchery by hatchery.csv", row.names = FALSE)
#############################################
#######  Escapement to Spawning Grounds #####
#############################################
#point estimates, no csv file created
CWT_SG<- CWT_Recoveries %>%
  filter(fishery == 54) %>% #from spawning ground surveys
  filter(hatchery_location_name == "COLEMAN NFH") %>% #For when obtaining hatchery-specific escapement
  mutate(estimated_number = ifelse(recovery_location_name == "COSUMNES RIVER" & run_year == 2012, 17.27, estimated_number)) %>%
  mutate(estimated_number = ifelse(recovery_location_name == "MILL CREEK" & run_year == 2012, 13.06, estimated_number)) %>%
  mutate(estimated_number = ifelse(recovery_location_name == "DEER CREEK" & run_year == 2012, 20.79, estimated_number)) %>%
  mutate(estimated_number = ifelse(recovery_location_name == "CALAVERAS RIVER" & run_year == 2011, 1.04, estimated_number)) %>%
  mutate(estimated_number = ifelse(recovery_location_name == "COTTONWOOD CREEK" & run_year == 2012, 3.41, estimated_number)) %>% #USFWS missing. Using CDFW value from same season and location
  mutate(estimated_number = ifelse(recovery_location_name == "COTTONWOOD CREEK" & run_year == 2013, 2.45, estimated_number)) %>% #USFWS missing. Using CDFW value from same season and location
  mutate(estimated_number = ifelse(recovery_location_name == "COTTONWOOD CREEK" & run_year == 2011, 1, estimated_number)) %>%#USFWS missing. Using CDFW value from same season and location
  mutate(estimated_number = ifelse(recovery_location_name == "SAC R AB RBDD" & run_year == 2013, 13.58, estimated_number)) %>%#USFWS missing. Using CDFW value from same season and location
  mutate(estimated_number = ifelse(recovery_location_name == "SAC R AB RBDD" & run_year == 2012, 12.08, estimated_number)) %>%#USFWS missing. Using CDFW value from same season and location
  mutate(estimated_number = ifelse(recovery_location_name == "SAC R AB RBDD" & run_year == 2011, 12.12, estimated_number)) %>%#USFWS missing. Using CDFW value from same season and location
  mutate(estimated_number = ifelse(sampling_agency == "CDFW" & run_year > 2010 & is.na(estimated_number), 1, estimated_number)) %>% #From Audrey: surveys 2011+ NAs should be 1
  mutate(estimated_number = ifelse(is.na(estimated_number), 1, estimated_number)) %>%
  mutate(Age = run_year - brood_year) 
CWT_Escapement<-CWT_SG %>%
  group_by(run_year, brood_year, Age) %>%
  summarise(Individuals= sum(estimated_number/Phi)) %>%
  pivot_wider(names_from = Age, values_from = Individuals, names_sort=TRUE) %>%
  group_by(brood_year) %>%
  #Nimbus does not have age-1 spawners
  summarize(Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
            , Age4Sp = sum(`4`, na.rm = TRUE), Age5Sp = sum(`5`, na.rm = TRUE)) #Zeros are from when sometimes there are NAs

#BOOTSTRAP for uncertainty
niterations = 1000
CWT_null<- CWT_SG %>%
  mutate(Individuals = NA) %>% #Empty space for resampled value
  mutate(Hatchery = NA) %>%
  mutate(CWTs = NA) #Empty space for resampled value
CWT_temp<-CWT_null #resampled individuals single data frame
Cohort_BY<-list() #Cohort BY will be a table of Spawners from each age by brood year
Cohort_RY<-list() #Cohort RY will be a table of Spawners from each age by brood year
for(j in 1:1){
  CWT_temp <-CWT_null %>% #at the start of every iteration, reset Individuals column
  group_by(brood_year, run_year, Age, estimated_number, Value_Expanded, recovery_location_name) %>%
    summarise(N=n()) %>%
    mutate(CWT = rnbinom(1,N,1/estimated_number)+N) %>%
    mutate(Individuals = CWT*Value_Expanded)
  Cohort_BY[[j]]<-CWT_temp %>%
    group_by(run_year, brood_year, Age) %>%
    summarise(Individuals= sum(Individuals)) %>% 
    pivot_wider(names_from = Age, values_from = Individuals, names_sort=TRUE) %>% #making each Age into its own column
    group_by(brood_year) %>% 
    #Nimbus and CNFH does not have age-1 spawners #Age1Sp = sum(`1`, na.rm = TRUE),
    summarize(Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
              , Age4Sp = sum(`4`, na.rm = TRUE), Age5Sp = sum(`5`, na.rm = TRUE)) #Zeros are from when sometimes there are NAs
  #######################
  #Don't need to run this chunk for specific hatcheries
  Cohort_RY[[j]]<-left_join(CWT_temp %>% #left joining this data by CWTs and Hatchery fish
    group_by(run_year, brood_year, Age, recovery_location_name) %>%
    summarise(CWT = sum(CWT), Individuals = sum(Individuals)) %>%
      pivot_wider(names_from = Age, values_from = CWT, names_sort=TRUE) %>%
      group_by(run_year, recovery_location_name) %>%
      summarize(Age2Sp_CWT = sum(`2`, na.rm = TRUE), Age3Sp_CWT = sum(`3`, na.rm = TRUE)
                , Age4Sp_CWT = sum(`4`, na.rm = TRUE), Age5Sp_CWT = sum(`5`, na.rm = TRUE)), #Zeros are from when sometimes there are NAs
    CWT_temp %>%
      group_by(run_year, brood_year, Age, recovery_location_name) %>%
      summarise(CWT = sum(CWT), Individuals = sum(Individuals))%>%
      pivot_wider(names_from = Age, values_from = Individuals, names_sort=TRUE) %>%
      group_by(run_year, recovery_location_name) %>%
      summarize(Age2Sp_Hatchery = sum(`2`, na.rm = TRUE), Age3Sp_Hatchery = sum(`3`, na.rm = TRUE)
                , Age4Sp_Hatchery = sum(`4`, na.rm = TRUE), Age5Sp_Hatchery = sum(`5`, na.rm = TRUE))) %>% #Zeros are from when sometimes there are NAs
      filter(run_year %in% 2010:2020)
     ########################
}
# saveRDS(Cohort_BY, file = "CWT Spawning Grounds CNFH.Rds")
saveRDS(Cohort_RY, file = "Escape to Spawning Grounds by river.Rds")