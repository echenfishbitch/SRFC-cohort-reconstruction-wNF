################################
### Size at age using MLE ######
################################
setwd("C:/Users/Emily/Box/Sacramento River Fall Chinook/Methodology Review")
library(bbmle)
library(ggplot2)
library(dplyr)
library(lubridate)
CWT_Recoveries_NFH<-read.csv("CWTRecoveries NFH.csv")
CWT_Recoveries_CNFH<-read.csv("CWTRecoveries CNFH.csv")
CWT_Recoveries_FRH<-read.csv("CWTRecoveries FRH.csv")
CWT_Recoveries<-rbind(CWT_Recoveries_CNFH, rbind(CWT_Recoveries_FRH, CWT_Recoveries_NFH))
SiteCodes<-read.csv("Site Codes.csv")
Size_Limits<-read.csv("Size limits.csv")  

CWT_Recoveries<-CWT_Recoveries %>%
  filter(brood_year %in% 1998:2016)%>%
  mutate(Month = month(ymd(recovery_date))) #Warning is for hatchery collections that don't have specific date
CWT_Recoveries$Month[which(CWT_Recoveries$recovery_date == 201907)]<-7 #Wonky recovery date
CWT_Recoveries_Ocean<-CWT_Recoveries %>% #adding Month of record
  mutate(age = run_year-brood_year) %>%
  filter(fishery == 40) %>% #only using recreation fishery data where size limit is smaller
  mutate(tl_in = (1.0403*length+26.504)/25.4) %>% #converting fork length to total length
  left_join(SiteCodes) %>%
  left_join(Size_Limits) %>%
  filter(!is.na(limit)) %>% #removing observations without size limits or length data
  filter(!is.na(tl_in)) 
colnames(CWT_Recoveries_Ocean)[2]<-"recovery_year"
colnames(CWT_Recoveries_Ocean)[13]<-"recovery_month"

results<-as.data.frame(matrix(data=NA, nrow = 1000, ncol = 5)) #large empty data frame for results to go
colnames(results)<-c("run_year", "Month", "Age", "mu", "sd")
counter=1
all_data=CWT_Recoveries_Ocean
		
		if (length(all_data$recovery_year)>0){#usable dataset
		
		#for figuring out what range of years to extract data for
		# min_year=min(all_data$recovery_year) #comment out for non year specific size at age
		# max_year=max(all_data$recovery_year) #comment out for non year specific size at age
		# 
		# for (year_at in min_year:max_year) #comment out for non year specific size at age
		{#year loop
			this_year_data=all_data#[all_data$recovery_year==year_at,] #comment our filtering for non year specific
		  this_year_data = this_year_data
			if (length(this_year_data$age>0))
			{#usable year
			
			#figure out what range of months to extract data for
			min_month=min(this_year_data$recovery_month)
			max_month=max(this_year_data$recovery_month)
			for (month_at in min_month:max_month)
			{#month loop
				this_month_data=this_year_data[this_year_data$recovery_month==month_at,]
				if (length(this_month_data$age>0))
				
				{#usable month
				
				#figure out ages present
				min_age=min(this_month_data$age)
				max_age=max(this_month_data$age)

				for (age_at in min_age:max_age)
				{#age loop
					data_use=this_month_data[this_month_data$age==age_at,]
					
					tot_length=data_use$tl_in #x is total length in inches
					size_lim=data_use$limit #sl is minimum size limit in effect when/where fish caught
					
					#screen out sublegal fish
					legal=tot_length-size_lim

					tot_length=tot_length[legal>=0]
					size_lim=size_lim[legal>=0]
				
					#check to see if adequate sample size
					if (length(tot_length)>20)
					{#adequate sample size

						#Truncated Normal fitting
						source( "subs_norm.r" )
						z=list( x=tot_length, sl=size_lim)

						# ===============================================================
						# ML Estimation
		
		  				init <- list( mu=mean(tot_length), sd=sd(tot_length) )
		 				mle.out <- mle2( neg.log.lik, data=z, start=init )
		  				cat("\n")
		  				print( summary( mle.out ) )
		  				coef(mle.out)
		  				results[counter,]<-c(year_at, month_at, age_at, coef(mle.out))
		  			counter<-counter+1
					}#adequate sample size
				}#age loop
				}#usable month
			}#month loop
			}#usable year
		}#year loop
	}#usable dataset
 results<-results %>%
  filter(mu > 0) %>%
# write.csv(results, "Sizeatage year.csv", row.names = FALSE)
   
   
#For Non year specific, general size at age
  select(!run_year) #for non year specific
allsizes<-as.data.frame(cbind(rep(1:12, 7), rep(1:7, each = 12)))  #All sizes for all months for all possible ages (fish age 1 - 7)
colnames(allsizes)<-c("Month", "Age") 
allsizes<-allsizes %>%
  left_join(results)
#No size at age was estimated for very young and very old ages and
#   age for winter months when there was limited fishery data
#For very young and very old ages, we use the oldest and youngest age present
#For winter months, we approximated mu and sd using linear interpolations
allsizes[1:16,3]<-allsizes[17,3] #for young ages, use the youngest age available 
allsizes[1:16,4]<-allsizes[17,4] #for young ages, use the youngest age available 
allsizes[47:84,3]<-allsizes[46,3] #for old ages, use the oldest age available
allsizes[47:84,4]<-allsizes[46,4] #for old ages, use the oldest age available
W3_mu<-approx(c(allsizes[23,3],allsizes[27,3]), n = 5) #No size estimate for fish from Dec - Feb of Age 3
W3_sd<-approx(c(allsizes[23,4],allsizes[27,4]), n = 5) #No size estimate for fish from Dec - Feb of Age 3
allsizes[23:27,3]<-W3_mu[[2]]
allsizes[23:27,4]<-W3_sd[[2]]
W4_mu<-approx(c(allsizes[34,3],allsizes[40,3]), n = 7) #No size estimate for fish from Nov - Mar of Age 4
W4_sd<-approx(c(allsizes[34,4],allsizes[40,4]), n = 7) #No size estimate for fish from Nov - Mar of Age 4
allsizes[34:40,3]<-W4_mu[[2]]
allsizes[34:40,4]<-W4_sd[[2]]
write.csv(allsizes, "Sizeatage general.csv", row.names = FALSE)
