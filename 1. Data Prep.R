library(dplyr)
library(readxl)
library(stringr)
library(lubridate)
setwd("C:/Users/Emily/Box/Sacramento River Fall Chinook/Methodology Review/")
########################
#####Release data#######
########################
#Feather River Hatchery
CWT_Releases.FRH<-read.csv("CWTReleases FRH.csv") #FRH has 2002-2005 unmarked releases
#Fish with CWT and Ad Clip: cwt_1st_mark_count
#Fish with CWT and No clip: cwt_2nd_mark_count
#Fish with no CWT and Ad Clip: non_cwt_1st_mark_count
#Fish with no CWT and No Clip: non_cwt_2nd_mark_count
###########
FRH_agg_years<-2002:2005 #For FRH BY 2002-2005, we will do an annual phi instead of batch phi because some batches have no marks
CWT_Releases.FRH.agg<-CWT_Releases.FRH %>%
  filter(brood_year%in% FRH_agg_years) %>%
  group_by(brood_year) %>%
  summarise(tag_code = NA,  Total_Released = sum(Total_Released), Phi = sum(cwt_1st_mark_count)/sum(Total_Released)) %>%
  mutate(hatchery_location_name = "FEATHER R HATCHERY")
CWT_Releases.FRH<-CWT_Releases.FRH %>%
  filter(!brood_year %in% FRH_agg_years) 
#COLEMAN NFH
CWT_Releases.CNFH<-read.csv("CWTReleases CNFH.csv") #CNFH has 1974-1999, 2002-2006, 2012, 2021 unmarked releases (very minor in 2006 and 2012)
##########
CNFH_agg_years<-c(1974:1999, 2002:2005, 2021) #For CNFH BY 1996-1999, 2002-2005 and other years, we will do an annual phi instead of batch phi
CWT_Releases.CNFH.agg<-CWT_Releases.CNFH %>%
  filter(brood_year%in% CNFH_agg_years) %>%
  group_by(brood_year) %>%
  summarise(tag_code = NA,  Total_Released = sum(Total_Released), Phi = sum(cwt_1st_mark_count)/sum(Total_Released)) %>%
mutate(hatchery_location_name = "COLEMAN NFH")
CWT_Releases.CNFH<-CWT_Releases.CNFH %>%
  filter(!brood_year %in% CNFH_agg_years) 
#NIMBUS F HATCHERY
CWT_Releases.NFH<-read.csv("CWTReleases NFH.csv") #NFH 2002, 2004-2005 unmarked releases only. no marked releases
####################
CWT_Releases<-rbind(CWT_Releases.CNFH, rbind(CWT_Releases.FRH, CWT_Releases.NFH))
#Phi is the proportion of released fish that have CWT and Ad Clips
CWT_Releases<-CWT_Releases %>%
  filter(record_code == "T") %>%
  select(brood_year, hatchery_location_name, tag_code_or_release_id, Total_Released, Phi)
colnames(CWT_Releases)[3]<-"tag_code"
CWT_Releases<-rbind(CWT_Releases, rbind(CWT_Releases.CNFH.agg, CWT_Releases.FRH.agg))
CWT_Releases$tag_code[is.na(CWT_Releases$tag_code)]<-"none"
write.csv(CWT_Releases, "CWTReleased.csv", row.names = FALSE)
