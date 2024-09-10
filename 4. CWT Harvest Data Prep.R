###############################################
######     CWT Fishery Data Prep       ########
###############################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
setwd("C:/Users/echen/Box/Sacramento River Fall Chinook/Cohort Reconstruction")
#############################################
#####        Release data           #########
#############################################
CWT_Releases<-read.csv("CWTReleased.csv")
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
  mutate(Value_Expanded = 1/Phi) %>%
  mutate(Month = month(ymd(recovery_date))) #Warning is for hatchery collections that don't have specific date
CWT_Recoveries$Month[which(CWT_Recoveries$recovery_date == 201907)]<-7 #Wonky recovery date
#############################################
#############  In-River Harvest #############
#############################################
#Point estimates, no file created
CWT_Recoveries_River<-CWT_Recoveries %>%
  # filter(hatchery_location_name == "COLEMAN NFH") %>%
  filter(fishery == 46) %>%
  mutate(Harvested = estimated_number/Phi) %>% #one river harvest has NA estimated number
  mutate(Age = run_year-brood_year) %>%
  group_by(run_year, brood_year, Age) %>%
  summarise(Harvested = sum(Harvested)) %>%
  pivot_wider(values_from = "Harvested", names_from = "Age")%>%
  select(run_year,brood_year, `2`,`3`,`4`,`5`) %>%
  group_by(brood_year) %>%
  summarise(InRiver2 = sum(`2`, na.rm = TRUE),InRiver3 = sum(`3`, na.rm = TRUE),InRiver4 = sum(`4`, na.rm = TRUE),InRiver5 = sum(`5`, na.rm = TRUE))# %>%
  # mutate(InRiver5 = 0) #For nimbus fish only

#BOOTSTRAP for uncertainty
niterations = 1000
CWT_River<- CWT_Recoveries %>%
  filter(hatchery_location_name == "FEATHER R HATCHERY") %>%
  filter(fishery == 46) %>%
  mutate(Age = run_year-brood_year) %>%
  mutate(Individuals = NA)#Empty space for resampled value
Cohort<-list() #Cohort will be a table of Spawners from each age, sampled 1000 times
for(j in 1:1000){
  Cohort[[j]] <-CWT_River %>%
  mutate(Individuals = (rnbinom(1,1,1/estimated_number)+1)/Phi) %>%
    group_by(run_year, brood_year, Age) %>%
    summarise(Individuals= sum(Individuals)) %>% 
    pivot_wider(names_from = Age, values_from = Individuals, names_sort=TRUE) %>% #making each Age into its own column
    group_by(brood_year) %>% #No inriver 5 for nimbus
    summarise(InRiver2 = sum(`2`, na.rm = TRUE),InRiver3 = sum(`3`, na.rm = TRUE),InRiver4 = sum(`4`, na.rm = TRUE),InRiver5 = sum(`5`, na.rm = TRUE)) #%>% #
    # mutate(InRiver5 = 0) #for nimbus fish only
}
# saveRDS(Cohort, file = "CWT River Harvest_FRH.Rds")
#############################################
############## Ocean Harvest ################
#############################################
SiteCodes<-read.csv("Site Codes.csv") #Sampling codes and their corresponding location
Size_Limits<-read.csv("Size limits.csv") 
Release_mort<-read.csv("Release Mortality Rates.csv")
SizeAge_year<-read.csv("Sizeatage year.csv") #year specific size at age when available    
SizeAge_general<-read.csv("Sizeatage general.csv")    #size at age using data from all years 
colnames(SizeAge_general)[c(3,4)]<-c("mu_gen", "sd_gen")
table(CWT_Recoveries$fishery)
CWT_Recoveries_Ocean<-CWT_Recoveries %>%  
  mutate(Age = run_year-brood_year) %>%
  filter(fishery == 10|fishery == 40|fishery == 15|fishery == 41|fishery == 42) %>%
  mutate(fishery = ifelse(fishery == 15|fishery == 41|fishery == 42, 40, fishery)) %>% #converting all sport types to 40 and treaty troll to 40 since regs are similar
  filter(run_year > 1995 & run_year < 2022) %>%
  mutate(Phi = ifelse(is.na(Phi), 1, Phi)) %>% #one sample in ocean recovery has no batch release info
  mutate(estimated_number = ifelse(estimated_number == 0, NA, estimated_number)) %>%
  left_join(SiteCodes) %>%
  mutate(Location = ifelse(is.na(Location) & sampling_agency == "MAKAH", "WA", Location))%>% 
  mutate(Location = ifelse(is.na(Location) & sampling_agency == "WDFW", "WA", Location))%>% 
  mutate(Location = ifelse(is.na(Location) & sampling_agency == "CDFO", "CAN", Location))%>% 
  mutate(Location = ifelse(is.na(Location) & sampling_agency == "ADFG", "AK", Location))%>% 
  left_join(Size_Limits) %>%
  left_join(Release_mort) %>%
#NF (North of Falcon) does not have size limit info from Mike so filling in
#WA recreational, AK, Canada (CAN), and WA commercial from Shelton et al. 2020
  mutate(limit = ifelse(Location == "WA" & fishery == 40 & Month > 6, 24, limit)) %>%
  mutate(limit = ifelse(Location == "WA" & fishery == 40 & Month < 7, 22, limit)) %>%
  mutate(limit = ifelse(Location == "WA" & fishery == 40 & run_year %in% 2016:2019, 24, limit)) %>%
  mutate(limit = ifelse(Location == "AK" & fishery == 10, 28, limit)) %>%
  mutate(limit = ifelse(Location == "CAN" & fishery == 10, 28, limit)) %>%
  mutate(limit = ifelse(Location == "WA" & fishery == 10 & run_year < 2021, 28, limit)) %>%
  mutate(limit = ifelse(Location == "WA" & fishery == 10 & run_year > 2020, 27, limit)) %>%
  mutate(limit = ifelse(Location == "CAN" & fishery == 40, 0, limit)) %>%
  mutate(limit = ifelse(Location == "AK" & fishery == 40, 0, limit)) %>%
  #NAs are primarily from commercial fishery and north of oregon
  mutate(Release.mort.rate = ifelse(is.na(Release.mort.rate), ifelse(fishery == 10, .26, .14) , Release.mort.rate))
summary(CWT_Recoveries_Ocean$estimated_number) #225 samples do not have estimated numbers or equals
#For missing estimated numbers, borrow from samples from the same time and area and fishery type

#If there are samples from the same month, year, recovery location, and fishery type, we borrow the mean estimated number
#If there are samples from the same month, year, sampling site, and fishery type, we borrow the mean estimated number
#If there are samples from the same month, year, Location, and fishery type, we borrow the mean estimated number
#If there are samples from the same year, Location, recovery location, and fishery type, we borrow the mean estimated number
#If there are samples from the same year, Location, and fishery type, we borrow the mean estimated number
#If there are samples from the same month, Location, and fishery type, we borrow the mean estimated number
#If there are samples from the Location, and fishery type, we borrow the mean estimated number

NAestimatednumber<-function(estimated_number, location_name, sampling_site, location, month, year, fishery){
  if(is.na(estimated_number)){
    if(length(CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$run_year == year
                                                                 & CWT_Recoveries_Ocean$Month == month &
                                                                 CWT_Recoveries_Ocean$fishery == fishery &
                                                                CWT_Recoveries_Ocean$recovery_location_name == location_name &
                                                                !is.na(CWT_Recoveries_Ocean$estimated_number))])> 1){
      mean(CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$run_year == year & 
                                                         CWT_Recoveries_Ocean$Month == month & 
                                                         CWT_Recoveries_Ocean$fishery == fishery &
                                                         CWT_Recoveries_Ocean$recovery_location_name == location_name)], na.rm = TRUE)
    }
  else if(length(CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$run_year == year
                                                              & CWT_Recoveries_Ocean$Month == month &
                                                              CWT_Recoveries_Ocean$fishery == fishery &
                                                              CWT_Recoveries_Ocean$sampling_site == sampling_site &
                                                              !is.na(CWT_Recoveries_Ocean$estimated_number))])> 1){
    mean(CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$run_year == year &
                                                       CWT_Recoveries_Ocean$Month == month &
                                                       CWT_Recoveries_Ocean$fishery == fishery &
                                                       CWT_Recoveries_Ocean$sampling_site == sampling_site)], na.rm = TRUE)
  }
  else if(length(CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$run_year == year
                                                             & CWT_Recoveries_Ocean$Month == month &
                                                             CWT_Recoveries_Ocean$fishery == fishery &
                                                             CWT_Recoveries_Ocean$Location == location &
                                                             !is.na(CWT_Recoveries_Ocean$estimated_number))])> 1){
    mean(CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$run_year == year &
                                                       CWT_Recoveries_Ocean$Month == month &
                                                       CWT_Recoveries_Ocean$fishery == fishery &
                                                       CWT_Recoveries_Ocean$Location == location)], na.rm = TRUE)
  }
    else if(length(CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$run_year == year &
                                                               CWT_Recoveries_Ocean$fishery == fishery &
                                                               CWT_Recoveries_Ocean$recovery_location_name == location_name &
                                                               !is.na(CWT_Recoveries_Ocean$estimated_number))])> 1){
      mean(CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$run_year == year &
                                                         CWT_Recoveries_Ocean$fishery == fishery &
                                                         CWT_Recoveries_Ocean$recovery_location_name == location_name)], na.rm = TRUE)
    }
    else if(length(CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$run_year == year &
                                                               CWT_Recoveries_Ocean$fishery == fishery &
                                                               CWT_Recoveries_Ocean$Location == location &
                                                               !is.na(CWT_Recoveries_Ocean$estimated_number))])> 1){
      mean(CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$run_year == year &
                                                         CWT_Recoveries_Ocean$fishery == fishery &
                                                         CWT_Recoveries_Ocean$Location == location)], na.rm = TRUE)
    }
    else if(length(CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$Month == month &
                                                               CWT_Recoveries_Ocean$fishery == fishery &
                                                               CWT_Recoveries_Ocean$Location == location &
                                                               !is.na(CWT_Recoveries_Ocean$estimated_number))])> 1){
      mean(CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$Month == month &
                                                         CWT_Recoveries_Ocean$fishery == fishery &
                                                         CWT_Recoveries_Ocean$Location == location)], na.rm = TRUE)
    }
else(estimated_number)
}
else(estimated_number)
}
CWT_Recoveries_Ocean$estimated_number<-mapply(NAestimatednumber, CWT_Recoveries_Ocean$estimated_number, CWT_Recoveries_Ocean$recovery_location_name,CWT_Recoveries_Ocean$sampling_site, CWT_Recoveries_Ocean$Location, CWT_Recoveries_Ocean$Month, CWT_Recoveries_Ocean$run_year, CWT_Recoveries_Ocean$fishery)
CWT_Recoveries_Ocean$estimated_number[which(CWT_Recoveries_Ocean$Location == "AK" & CWT_Recoveries_Ocean$fishery == 40)]<-1 #only one fish was recorded in AK recreational fisheries
CWT_Recoveries_Ocean<-CWT_Recoveries_Ocean %>% #adding Month of record
  left_join(SizeAge_year) %>%
  left_join(SizeAge_general) %>%
  mutate(mu = ifelse(is.na(mu),mu_gen,mu)) %>%
  mutate(sd = ifelse(is.na(sd),sd_gen,sd))
#pnorm function
Percent_Harvest<-function(Age, Length, Size_Limit, mu, s){
  if(Size_Limit == 0){
    1
  }
  else if (Age > 4){ #for rare age 5 fish, we assume they are always harvestable fish
    1
  }
  else if(!is.na(Length)){ #for fish below the size limit, we assume they are always harvestable 
  if((Length*1.0403+26.504)/25.4 < Size_Limit){
    1
  }
  else{
      1-pnorm(Size_Limit, mean = mu, sd = s)
    } }
  else{
  1-pnorm(Size_Limit, mean = mu, sd = s)
  }
}

CWT_Recoveries_Ocean$Percent_Harvestable<-as.numeric(as.character(mapply(Percent_Harvest, CWT_Recoveries_Ocean$Age, CWT_Recoveries_Ocean$length, CWT_Recoveries_Ocean$limit, CWT_Recoveries_Ocean$mu, CWT_Recoveries_Ocean$sd)))
CWT_Recoveries_Ocean<-CWT_Recoveries_Ocean %>%
  mutate(Percent_Harvestable = ifelse(is.na(Percent_Harvestable), 1, Percent_Harvestable)) %>%
  mutate(Harvested = estimated_number/Phi) %>%
  mutate(Catch=Harvested/Percent_Harvestable) %>% #Catch is C=H/p
  mutate(Release_Mort = (Catch-Harvested)*Release.mort.rate) %>% #Percent of those caught but not harvested that die after release (Standard 26% catch and release mortality-commerical fishery)
  mutate(Drop_Mort = (Catch)*.05) %>% #Drop off mortality 5% of Catch
  mutate(Impact = Harvested+Release_Mort+Drop_Mort) # I = H+S+D

Impact<-CWT_Recoveries_Ocean %>%
  filter(hatchery_location_name == "FEATHER R HATCHERY") %>%
  filter(brood_year > 1997 & brood_year < 2018)  %>%
  group_by(brood_year, run_year, Month, Age) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch),Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact)) %>%
  pivot_wider(names_from = c(Age,Month), values_from = Impact, names_sort=TRUE)
Impact<-Impact %>%
  group_by(brood_year) %>%
  #excluding 1 year old catches
  summarize(Apr2 = sum(`2_4`, na.rm = TRUE),May2 = sum(`2_5`, na.rm = TRUE),
            Jun2= sum(`2_6`, na.rm = TRUE), Jul2 = sum(`2_7`, na.rm = TRUE), Aug2 = sum(`2_8`, na.rm = TRUE),
            Sept2 = sum(`2_9`, na.rm = TRUE),Oct2 = sum(`2_10`, na.rm = TRUE), Nov2 = sum(`2_11`, na.rm = TRUE),
            Dec2 = sum(`2_12`, na.rm = TRUE), Jan3 = sum(`3_1`, na.rm = TRUE), Feb3 = sum(`3_2`, na.rm = TRUE),
            Mar3 = sum(`3_3`, na.rm = TRUE),Apr3 = sum(`3_4`, na.rm = TRUE),May3 = sum(`3_5`, na.rm = TRUE),
            Jun3= sum(`3_6`, na.rm = TRUE), Jul3 = sum(`3_7`, na.rm = TRUE), Aug3 = sum(`3_8`, na.rm = TRUE),
            Sept3 = sum(`3_9`, na.rm = TRUE), Oct3 = sum(`3_10`, na.rm = TRUE), Nov3 = sum(`3_11`, na.rm = TRUE),
            Dec3 = sum(`3_12`, na.rm = TRUE), Feb4 = sum(`4_2`, na.rm = TRUE),Mar4 = sum(`4_3`, na.rm = TRUE),
            Apr4 = sum(`4_4`, na.rm = TRUE), May4 = sum(`4_5`, na.rm = TRUE), Jun4= sum(`4_6`, na.rm = TRUE) ,
            Jul4 = sum(`4_7`, na.rm = TRUE), Aug4 = sum(`4_8`, na.rm = TRUE), Sept4 = sum(`4_9`, na.rm = TRUE),
            Oct4 = sum(`4_10`, na.rm = TRUE), Nov4 = sum(`4_11`, na.rm = TRUE), Feb5 = sum(`5_2`, na.rm = TRUE),
            Mar5 = sum(`5_3`, na.rm = TRUE),Apr5 = sum(`5_4`, na.rm = TRUE),May5 = sum(`5_5`, na.rm = TRUE),
            Jun5= sum(`5_6`, na.rm = TRUE), Jul5 = sum(`5_7`, na.rm = TRUE), Aug5 = sum(`5_8`, na.rm = TRUE),
            Sept5 = sum(`5_9`, na.rm = TRUE), Oct5 = sum(`5_10`, na.rm = TRUE), 
            Jun6= sum(`6_6`, na.rm = TRUE) , Jul6= sum(`6_7`, na.rm = TRUE),
            Aug6 = sum(`6_8`, na.rm = TRUE),  May7 = sum(`7_5`, na.rm = TRUE),
            Jun7= sum(`7_6`, na.rm = TRUE) , Jul7 = sum(`7_7`, na.rm = TRUE), Aug7 = sum(`7_8`, na.rm = TRUE))
#FRH
Impact<-Impact %>%
  group_by(brood_year) %>%
  summarize(Mar2 = sum(`2_3`, na.rm = TRUE), Apr2 = sum(`2_4`, na.rm = TRUE),May2 = sum(`2_5`, na.rm = TRUE),
            Jun2= sum(`2_6`, na.rm = TRUE), Jul2 = sum(`2_7`, na.rm = TRUE), Aug2 = sum(`2_8`, na.rm = TRUE),
            Sept2 = sum(`2_9`, na.rm = TRUE),Oct2 = sum(`2_10`, na.rm = TRUE), Nov2 = sum(`2_11`, na.rm = TRUE),
            Dec2 = sum(`2_12`, na.rm = TRUE), Jan3 = sum(`3_1`, na.rm = TRUE), Feb3 = sum(`3_2`, na.rm = TRUE),
            Mar3 = sum(`3_3`, na.rm = TRUE),Apr3 = sum(`3_4`, na.rm = TRUE),May3 = sum(`3_5`, na.rm = TRUE),
            Jun3= sum(`3_6`, na.rm = TRUE), Jul3 = sum(`3_7`, na.rm = TRUE), Aug3 = sum(`3_8`, na.rm = TRUE),
            Sept3 = sum(`3_9`, na.rm = TRUE), Oct3 = sum(`3_10`, na.rm = TRUE), Nov3 = sum(`3_11`, na.rm = TRUE),
            Dec3 = sum(`3_12`, na.rm = TRUE), Apr4 = sum(`4_1`, na.rm = TRUE), Feb4 = sum(`4_2`, na.rm = TRUE),Mar4 = sum(`4_3`, na.rm = TRUE),
            Apr4 = sum(`4_4`, na.rm = TRUE), May4 = sum(`4_5`, na.rm = TRUE), Jun4= sum(`4_6`, na.rm = TRUE) ,
            Jul4 = sum(`4_7`, na.rm = TRUE), Aug4 = sum(`4_8`, na.rm = TRUE), Sept4 = sum(`4_9`, na.rm = TRUE),
            Oct4 = sum(`4_10`, na.rm = TRUE), Nov4 = sum(`4_11`, na.rm = TRUE), Feb5 = sum(`5_2`, na.rm = TRUE),
            Mar5 = sum(`5_3`, na.rm = TRUE),Apr5 = sum(`5_4`, na.rm = TRUE),May5 = sum(`5_5`, na.rm = TRUE),
            Jun5= sum(`5_6`, na.rm = TRUE), Jul5 = sum(`5_7`, na.rm = TRUE), Aug5 = sum(`5_8`, na.rm = TRUE),
            Sept5 = sum(`5_9`, na.rm = TRUE), Oct5 = sum(`5_10`, na.rm = TRUE), Apr6= sum(`6_4`, na.rm = TRUE) ,
            May6= sum(`6_5`, na.rm = TRUE) ,Jun6= sum(`6_6`, na.rm = TRUE) , Jul6= sum(`6_7`, na.rm = TRUE),
            Aug6 = sum(`6_8`, na.rm = TRUE),  Aug6 = sum(`6_9`, na.rm = TRUE), May7 = sum(`7_5`, na.rm = TRUE),
            Jun7= sum(`7_6`, na.rm = TRUE) , Jul7 = sum(`7_7`, na.rm = TRUE), Aug7 = sum(`7_8`, na.rm = TRUE),
            Jun8 = sum(`8_6`, na.rm = TRUE))
#CNFH
Impact<-Impact %>%
  group_by(brood_year) %>%
  summarize(Apr2 = sum(`2_4`, na.rm = TRUE),May2 = sum(`2_5`, na.rm = TRUE),
            Jun2= sum(`2_6`, na.rm = TRUE), Jul2 = sum(`2_7`, na.rm = TRUE), Aug2 = sum(`2_8`, na.rm = TRUE),
            Sept2 = sum(`2_9`, na.rm = TRUE),Oct2 = sum(`2_10`, na.rm = TRUE), Nov2 = sum(`2_11`, na.rm = TRUE),
            Dec2 = 0, Jan3 = sum(`3_1`, na.rm = TRUE), Feb3 = sum(`3_2`, na.rm = TRUE),
            Mar3 = sum(`3_3`, na.rm = TRUE),Apr3 = sum(`3_4`, na.rm = TRUE),May3 = sum(`3_5`, na.rm = TRUE),
            Jun3= sum(`3_6`, na.rm = TRUE), Jul3 = sum(`3_7`, na.rm = TRUE), Aug3 = sum(`3_8`, na.rm = TRUE),
            Sept3 = sum(`3_9`, na.rm = TRUE), Oct3 = sum(`3_10`, na.rm = TRUE), Nov3 = sum(`3_11`, na.rm = TRUE),
            Dec3 = 0, Feb4 = 0, Mar4 = sum(`4_3`, na.rm = TRUE),
            Apr4 = sum(`4_4`, na.rm = TRUE), May4 = sum(`4_5`, na.rm = TRUE), Jun4= sum(`4_6`, na.rm = TRUE) ,
            Jul4 = sum(`4_7`, na.rm = TRUE), Aug4 = sum(`4_8`, na.rm = TRUE), Sept4 = sum(`4_9`, na.rm = TRUE),
            Oct4 = sum(`4_10`, na.rm = TRUE), Nov4 =0, Feb5 = 0,
            Mar5 = 0,Apr5 = sum(`5_4`, na.rm = TRUE),May5 = sum(`5_5`, na.rm = TRUE),
            Jun5= sum(`5_6`, na.rm = TRUE), Jul5 = sum(`5_7`, na.rm = TRUE), Aug5 = sum(`5_8`, na.rm = TRUE),
            Sept5 = sum(`5_9`, na.rm = TRUE), Oct5 = 0,
            Jun6= sum(`6_6`, na.rm = TRUE) , Jul6= sum(`6_7`, na.rm = TRUE),
            Aug6 = sum(`6_8`, na.rm = TRUE), May7 = 0,
            Jun7= sum(`7_6`, na.rm = TRUE) , Jul7 = sum(`7_7`, na.rm = TRUE), Aug7 = sum(`7_8`, na.rm = TRUE)) 
#NFH
Impact<-Impact %>%
  group_by(brood_year) %>%
  summarize(Apr2 = 0, May2 = sum(`2_5`, na.rm = TRUE),
            Jun2= sum(`2_6`, na.rm = TRUE), Jul2 = sum(`2_7`, na.rm = TRUE), Aug2 = sum(`2_8`, na.rm = TRUE),
            Sept2 = sum(`2_9`, na.rm = TRUE),Oct2 = sum(`2_10`, na.rm = TRUE), Nov2 = sum(`2_11`, na.rm = TRUE),
            Dec2 = 0, Jan3 = 0, Feb3 = sum(`3_2`, na.rm = TRUE),
            Mar3 = sum(`3_3`, na.rm = TRUE),Apr3 = sum(`3_4`, na.rm = TRUE),May3 = sum(`3_5`, na.rm = TRUE),
            Jun3= sum(`3_6`, na.rm = TRUE), Jul3 = sum(`3_7`, na.rm = TRUE), Aug3 = sum(`3_8`, na.rm = TRUE),
            Sept3 = sum(`3_9`, na.rm = TRUE), Oct3 = sum(`3_10`, na.rm = TRUE), Nov3 = sum(`3_11`, na.rm = TRUE),
            Dec3 = 0, Feb4 = sum(`4_2`, na.rm = TRUE),Mar4 = sum(`4_3`, na.rm = TRUE),
            Apr4 = sum(`4_4`, na.rm = TRUE), May4 = sum(`4_5`, na.rm = TRUE), Jun4= sum(`4_6`, na.rm = TRUE) ,
            Jul4 = sum(`4_7`, na.rm = TRUE), Aug4 = sum(`4_8`, na.rm = TRUE), Sept4 = sum(`4_9`, na.rm = TRUE),
            Oct4 = sum(`4_10`, na.rm = TRUE), Nov4 = sum(`4_11`, na.rm = TRUE), Feb5 = 0,
            Mar5 = sum(`5_3`, na.rm = TRUE),Apr5 = sum(`5_4`, na.rm = TRUE),May5 = sum(`5_5`, na.rm = TRUE),
            Jun5= sum(`5_6`, na.rm = TRUE), Jul5 = sum(`5_7`, na.rm = TRUE), Aug5 = sum(`5_8`, na.rm = TRUE),
            Sept5 = sum(`5_9`, na.rm = TRUE), Oct5 = sum(`5_10`, na.rm = TRUE), 
            Jun6= sum(`6_6`, na.rm = TRUE) , Jul6= sum(`6_7`, na.rm = TRUE),
            Aug6 = 0, May7 = sum(`7_5`, na.rm = TRUE),
            Jun7= 0 , Jul7 = 0, Aug7 = 0) 
###Bootstrapping.
Harvest.list<-list() #where we summarized Impact data by year, month, age
for(i in 1:1000){
  Harvest.list[[i]]<-CWT_Recoveries_Ocean %>%
    # filter(hatchery_location_name == "COLEMAN NFH") %>%
    group_by(brood_year, run_year, Age, Month, estimated_number, Value_Expanded, fishery, Location, Percent_Harvestable, Release.mort.rate) %>%
    summarise(N=n()) %>%
    mutate(Harvested_Sample = (rnbinom(1,N,1/estimated_number)+N)*Value_Expanded) %>%
    mutate(Catch_Sample = Harvested_Sample/Percent_Harvestable) %>%
    mutate(Drop_Sample = Catch_Sample*.05) %>%
    mutate(Release_Sample = (Catch_Sample - Harvested_Sample)*Release.mort.rate) %>%
    mutate(Impact_Sample = Harvested_Sample+Drop_Sample+Release_Sample)%>%
    group_by(run_year, brood_year, Age, Month) %>%
    summarise(Tags_Collected = sum(N), Harvested_Sample=sum(Harvested_Sample), Catch_Sample =sum(Catch_Sample), Release_Sample=sum(Release_Sample), Drop_Sample=sum(Drop_Sample), Impact_Sample=sum(Impact_Sample))
  }  

test<-Harvest.list[[1]] %>%
pivot_wider(names_from = c(Age,Month),values_from= Impact_Sample, names_sort = TRUE )%>%
  group_by(brood_year) 
colnames(test)
Impact_Bootstrap<-list()
for (i in 1:1000){
  Impact_Bootstrap[[i]]<-Harvest.list[[i]] %>%
    pivot_wider(names_from = c(Age,Month),values_from= Impact_Sample, names_sort = TRUE )%>%
    group_by(brood_year) %>%
  #need to exchange out with top for each hatchery
    summarize(Mar2 = sum(`2_3`, na.rm = TRUE), Apr2 = sum(`2_4`, na.rm = TRUE),May2 = sum(`2_5`, na.rm = TRUE),
              Jun2= sum(`2_6`, na.rm = TRUE), Jul2 = sum(`2_7`, na.rm = TRUE), Aug2 = sum(`2_8`, na.rm = TRUE),
              Sept2 = sum(`2_9`, na.rm = TRUE),Oct2 = sum(`2_10`, na.rm = TRUE), Nov2 = sum(`2_11`, na.rm = TRUE),
              Dec2 = sum(`2_12`, na.rm = TRUE), Jan3 = sum(`3_1`, na.rm = TRUE), Feb3 = sum(`3_2`, na.rm = TRUE),
              Mar3 = sum(`3_3`, na.rm = TRUE),Apr3 = sum(`3_4`, na.rm = TRUE),May3 = sum(`3_5`, na.rm = TRUE),
              Jun3= sum(`3_6`, na.rm = TRUE), Jul3 = sum(`3_7`, na.rm = TRUE), Aug3 = sum(`3_8`, na.rm = TRUE),
              Sept3 = sum(`3_9`, na.rm = TRUE), Oct3 = sum(`3_10`, na.rm = TRUE), Nov3 = sum(`3_11`, na.rm = TRUE),
              Dec3 = sum(`3_12`, na.rm = TRUE), Apr4 = sum(`4_1`, na.rm = TRUE), Feb4 = sum(`4_2`, na.rm = TRUE),Mar4 = sum(`4_3`, na.rm = TRUE),
              Apr4 = sum(`4_4`, na.rm = TRUE), May4 = sum(`4_5`, na.rm = TRUE), Jun4= sum(`4_6`, na.rm = TRUE) ,
              Jul4 = sum(`4_7`, na.rm = TRUE), Aug4 = sum(`4_8`, na.rm = TRUE), Sept4 = sum(`4_9`, na.rm = TRUE),
              Oct4 = sum(`4_10`, na.rm = TRUE), Nov4 = sum(`4_11`, na.rm = TRUE), Feb5 = sum(`5_2`, na.rm = TRUE),
              Mar5 = sum(`5_3`, na.rm = TRUE),Apr5 = sum(`5_4`, na.rm = TRUE),May5 = sum(`5_5`, na.rm = TRUE),
              Jun5= sum(`5_6`, na.rm = TRUE), Jul5 = sum(`5_7`, na.rm = TRUE), Aug5 = sum(`5_8`, na.rm = TRUE),
              Sept5 = sum(`5_9`, na.rm = TRUE), Oct5 = sum(`5_10`, na.rm = TRUE), Apr6= sum(`6_4`, na.rm = TRUE) ,
              May6= sum(`6_5`, na.rm = TRUE) ,Jun6= sum(`6_6`, na.rm = TRUE) , Jul6= sum(`6_7`, na.rm = TRUE),
              Aug6 = sum(`6_8`, na.rm = TRUE),  Aug6 = sum(`6_9`, na.rm = TRUE), May7 = sum(`7_5`, na.rm = TRUE),
              Jun7= sum(`7_6`, na.rm = TRUE) , Jul7 = sum(`7_7`, na.rm = TRUE), Aug7 = sum(`7_8`, na.rm = TRUE),
              Jun8 = sum(`8_6`, na.rm = TRUE))
}
test<- Impact_Bootstrap[[1]]

saveRDS(Impact_Bootstrap, file = "Impact CWT Bootstrap.Rds")
Harvest_Bootstrap<-list()
for (i in 1:1000){
  Harvest_Bootstrap[[i]]<-Harvest.list[[i]] %>%
    pivot_wider(names_from = c(Age,Month),values_from= Harvested_Sample, names_sort = TRUE )%>%
    group_by(brood_year) %>%
    #need to exchange out with top for each hatchery
    summarize(Mar2 = sum(`2_3`, na.rm = TRUE), Apr2 = sum(`2_4`, na.rm = TRUE),May2 = sum(`2_5`, na.rm = TRUE),
              Jun2= sum(`2_6`, na.rm = TRUE), Jul2 = sum(`2_7`, na.rm = TRUE), Aug2 = sum(`2_8`, na.rm = TRUE),
              Sept2 = sum(`2_9`, na.rm = TRUE),Oct2 = sum(`2_10`, na.rm = TRUE), Nov2 = sum(`2_11`, na.rm = TRUE),
              Dec2 = sum(`2_12`, na.rm = TRUE), Jan3 = sum(`3_1`, na.rm = TRUE), Feb3 = sum(`3_2`, na.rm = TRUE),
              Mar3 = sum(`3_3`, na.rm = TRUE),Apr3 = sum(`3_4`, na.rm = TRUE),May3 = sum(`3_5`, na.rm = TRUE),
              Jun3= sum(`3_6`, na.rm = TRUE), Jul3 = sum(`3_7`, na.rm = TRUE), Aug3 = sum(`3_8`, na.rm = TRUE),
              Sept3 = sum(`3_9`, na.rm = TRUE), Oct3 = sum(`3_10`, na.rm = TRUE), Nov3 = sum(`3_11`, na.rm = TRUE),
              Dec3 = sum(`3_12`, na.rm = TRUE), Apr4 = sum(`4_1`, na.rm = TRUE), Feb4 = sum(`4_2`, na.rm = TRUE),Mar4 = sum(`4_3`, na.rm = TRUE),
              Apr4 = sum(`4_4`, na.rm = TRUE), May4 = sum(`4_5`, na.rm = TRUE), Jun4= sum(`4_6`, na.rm = TRUE) ,
              Jul4 = sum(`4_7`, na.rm = TRUE), Aug4 = sum(`4_8`, na.rm = TRUE), Sept4 = sum(`4_9`, na.rm = TRUE),
              Oct4 = sum(`4_10`, na.rm = TRUE), Nov4 = sum(`4_11`, na.rm = TRUE), Feb5 = sum(`5_2`, na.rm = TRUE),
              Mar5 = sum(`5_3`, na.rm = TRUE),Apr5 = sum(`5_4`, na.rm = TRUE),May5 = sum(`5_5`, na.rm = TRUE),
              Jun5= sum(`5_6`, na.rm = TRUE), Jul5 = sum(`5_7`, na.rm = TRUE), Aug5 = sum(`5_8`, na.rm = TRUE),
              Sept5 = sum(`5_9`, na.rm = TRUE), Oct5 = sum(`5_10`, na.rm = TRUE), Apr6= sum(`6_4`, na.rm = TRUE) ,
              May6= sum(`6_5`, na.rm = TRUE) ,Jun6= sum(`6_6`, na.rm = TRUE) , Jul6= sum(`6_7`, na.rm = TRUE),
              Aug6 = sum(`6_8`, na.rm = TRUE),  Aug6 = sum(`6_9`, na.rm = TRUE), May7 = sum(`7_5`, na.rm = TRUE),
              Jun7= sum(`7_6`, na.rm = TRUE) , Jul7 = sum(`7_7`, na.rm = TRUE), Aug7 = sum(`7_8`, na.rm = TRUE),
              Jun8 = sum(`8_6`, na.rm = TRUE))
  }
saveRDS(Harvest_Bootstrap, file = "Harvest CWT Bootstrap.Rds")
