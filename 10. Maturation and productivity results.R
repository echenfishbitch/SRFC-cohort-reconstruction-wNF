#################################
### Plotting Maturation results##
#################################
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)
library(wdm)
setwd("C:/Users/echen/Box/Sacramento River Fall Chinook/Methodology Review")

#OVerall Hatchery and Overall Natural Maturation Rates
Hatchery<-read.csv("Maturation Uncertainty CWT.csv")
Hatchery$Population<-"Hatchery"
Hatchery<-Hatchery %>%
  filter(brood_year %in% 1998:2016)
Natural<-read.csv("Maturation Uncertainty Natural.csv")
Natural$Population<-"Natural"
Natural<-Natural %>%
  filter(brood_year %in% 2008:2016)
Maturation<-rbind(Hatchery, Natural)
colnames(Maturation)[14]<-"Origin"
#Hatchery-specific maturation rates
FRH<-read.csv("Maturation Uncertainty CWT FRH.csv")
FRH$Population<-"Feather River Hatchery"
CNFH<-read.csv("Maturation Uncertainty CWT CNFH.csv")
CNFH$Population<-"Coleman National Fish Hatchery"
NFH<-read.csv("Maturation Uncertainty CWT NFH.csv")
NFH$Population<-"Nimbus Fish Hatchery"
Hatchery_Maturation<-rbind(FRH, rbind(CNFH, NFH))
Hatchery_Maturation <- Hatchery_Maturation %>%
  filter(brood_year %in% 1998:2016)
#River-specific maturation rates
Sac<-read.csv("Maturation Uncertainty Natural Sac.csv")
Sac$Population<-"Upper Sacramento River            "
Fea<-read.csv("Maturation Uncertainty Natural Feather.csv")
Fea$Population<-"Feather River"
Ame<-read.csv("Maturation Uncertainty Natural American.csv")
Ame$Population<-"American River"
Clear<-read.csv("Maturation Uncertainty Natural Clear.csv")
Clear$Population<-"Clear Creek"
Yuba<-read.csv("Maturation Uncertainty Natural Yuba.csv")
Yuba$Population<-"Yuba River"
Yuba<-Yuba %>%
  filter(brood_year > 2011)
Battle<-read.csv("Maturation Uncertainty Natural Battle.csv")
Battle$Population<-"Battle Creek"
Other<-read.csv("Maturation Uncertainty Natural Other.csv")
Other$Population<-"Other tributaries"
Other<-Other %>%
  filter(brood_year >= 2008)
Natural_Maturation<-rbind(Ame, rbind(Clear, rbind(Sac,rbind(Fea, rbind(Yuba, rbind(Battle, Other))))))
Natural_Maturation <- Natural_Maturation %>%
  filter(brood_year %in% 2008:2016)
Natural_Maturation$Population<- ordered(Natural_Maturation$Population, levels = c("Upper Sacramento River            ", "Feather River", "American River",
                                                                                  "Yuba River", "Battle Creek", "Clear Creek", "Other tributaries"))


blank_bg<-theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))

age2<-ggplot(Maturation, aes(x=brood_year, y = Mat2Mean, color = Origin))+
  geom_errorbar(aes(ymin=Mat2Lower, ymax=Mat2Upper))+
  geom_point(size = 3)+
  ylab("age-2 maturation rate")+ 
  xlab("brood year")+
  ylim(0,1)+
  scale_x_continuous("Brood Year", labels = as.character(1998:2016), breaks = 1998:2016, limits = c(1997.5, 2016.5)) +
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  blank_bg

age3<-ggplot(Maturation, aes(x=brood_year, y = Mat3Mean, color = Origin))+
  geom_errorbar(aes(ymin=Mat3Lower, ymax=Mat3Upper))+
  geom_point(size = 3)+
  ylab("age-3 maturation rate")+ 
  xlab("brood year")+
  ylim(0,1)+
  scale_x_continuous("Brood Year", labels = as.character(1998:2016), breaks = 1998:2016, limits = c(1997.5, 2016.5)) +
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  blank_bg
plot_grid(age2,age3, nrow=2)
a<-ggplot(Hatchery_Maturation, aes(x=brood_year, y = Mat2Mean, color = Population))+
  geom_errorbar(aes(ymin=Mat2Lower, ymax=Mat2Upper))+
  geom_point(size = 3)+
  ylab("age-2 maturation rate")+ 
  xlab("brood year")+
  ylim(0,1)+
  scale_x_continuous("Brood Year", labels = as.character(1998:2016), breaks = 1998:2016, limits = c(1997.5, 2016.5)) +
  scale_color_manual(values=c("#E69F00", "#e3c277", "#8e6200"))+
  blank_bg
b<-ggplot(Natural_Maturation, aes(x=brood_year, y = Mat2Mean, color = Population))+
  geom_errorbar(aes(ymin=Mat2Lower, ymax=Mat2Upper))+
  geom_point(size = 3)+
  ylab("age-2 maturation rate")+ 
  # xlab("run year")+ 
  ylim(0,1)+
  scale_x_continuous("Brood Year", labels = as.character(1998:2016), breaks = 1998:2016, limits = c(1997.5, 2016.5)) +
  scale_color_manual(values=c("#969ca3","#3e7b93" ,"#171691","lightskyblue", "blue", "cadetblue1",  "#d3dbe1"))+
  blank_bg
plot_grid(a,b, nrow=2)
ggsave('FigA1.png', plot=last_plot(),  width=10, height = 6)
c<-ggplot(Hatchery_Maturation, aes(x=brood_year, y = Mat3Mean, color = Population))+
  geom_errorbar(aes(ymin=Mat3Lower, ymax=Mat3Upper))+
  geom_point(size = 3)+
  ylab("age-3 maturation rate")+ 
  xlab("brood year")+
  ylim(0,1)+
  scale_x_continuous("Brood Year", labels = as.character(1998:2016), breaks = 1998:2016, limits = c(1997.5, 2016.5)) +
  scale_color_manual(values=c("#E69F00", "#e3c277", "#8e6200"))+
  blank_bg
d<-ggplot(Natural_Maturation, aes(x=brood_year, y = Mat3Mean, color = Population))+
  geom_errorbar(aes(ymin=Mat3Lower, ymax=Mat3Upper))+
  geom_point(size = 3)+
  ylab("age-3 maturation rate")+ 
  # xlab("run year")+ 
  ylim(0,1)+
  scale_x_continuous("Brood Year", labels = as.character(1998:2016), breaks = 1998:2016, limits = c(1997.5, 2016.5)) +
  scale_color_manual(values=c("#969ca3","#3e7b93" ,"#171691","lightskyblue", "blue", "cadetblue1",  "#d3dbe1"))+
  blank_bg
plot_grid(c,d, nrow=2)
#################################################
### Reporting and testing maturation results ####
#################################################
#Hatchery fish
Hatchery<-read.csv("Maturation Uncertainty CWT.csv")
Hatchery<-Hatchery %>%
  filter(brood_year > 1997 & brood_year < 2017)
summary(Hatchery$Mat2Mean) #average age-2 maturation rate
sd(Hatchery$Mat2Mean) #SD age-2 maturation rate
summary(Hatchery$Mat3Mean) #average age-3 maturation rate
sd(Hatchery$Mat3Mean) #SD age-3 maturation rate
indep_test(Hatchery$brood_year, Hatchery$Mat2Mean, method=c("kendall"), weights = 1/(Hatchery$SDMat2)^2)#trend in age-2 with weights
indep_test(Hatchery$brood_year, Hatchery$Mat3Mean, method=c("kendall"), weights = 1/(Hatchery$SDMat3)^2)#trend in age-3 with weights
Hatchery<-Hatchery %>% #same years as natural-origin
  filter(brood_year > 2007 & brood_year < 2017)
summary(Hatchery$Mat2Mean) #average age-2 maturation rate
sd(Hatchery$Mat2Mean) #average age-2 maturation rate
summary(Hatchery$Mat3Mean) #average age-3 maturation rate
sd(Hatchery$Mat3Mean) #sd age-3 maturation rate
#Natural
Natural<-read.csv("Maturation Uncertainty Natural.csv")
NaturalAge2<-Natural %>%
  filter(brood_year > 2007 & brood_year < 2017)
summary(NaturalAge2$Mat2Mean) #average age-2 maturation rate
sd(NaturalAge2$Mat2Mean) #SD age-2 maturation rate
summary(NaturalAge2$Mat3Mean) #average age-2 maturation rate
sd(NaturalAge2$Mat3Mean) #SD age-2 maturation rate
indep_test(NaturalAge2$brood_year, NaturalAge2$Mat2Mean, method=c("kendall"), weights = 1/(NaturalAge2$SDMat2)^2)
indep_test(NaturalAge2$brood_year, NaturalAge2$Mat3Mean, method=c("kendall"), weights = 1/(NaturalAge2$SDMat3)^2)
#Relationship between error and age-2 maturation
SI<-read.csv("SIdata.csv")
SI$PostSI<-SI$PostSI*1000
SI$error<-0
SI$rho<-NA
SI$B0<-NA
SI$B1<-NA
SI$PreSI<-NA
for(i in 2001:2022){ #Year is the forecast year
  subset<-SI %>%
    filter(Year < i) #previous years data
  Jacks<-log(subset$Jacks[1:(nrow(subset)-1)])
  SIPost<-log(subset$PostSI[2:nrow(subset)])
  OG_model<-arima(SIPost, xreg=Jacks, order=c(1,0,0))
  summary<-summary(OG_model)
  SI$B0[i-1982]<-coef(OG_model)[2]
  SI$B1[i-1982]<-coef(OG_model)[3]
  SI$rho[i-1982]<-coef(OG_model)[1]
  SI$PreSI[i-1982]<-exp(coef(OG_model)[2]+
                          coef(OG_model)[3]*log(SI$Jacks[which(SI$Year == i-1)])+
                          coef(OG_model)[1]*SI$error[i-1983])
  SI$error[i-1982]<-log(SI$PostSI[which(SI$Year == i)])-log(SI$PreSI[which(SI$Year == i)])
}
#Error
SI<-SI %>%
  filter(Year %in% 2001:2022)
forecastmodelerror<-as.data.frame(cbind(SI$PostSI-exp(SI$B0+SI$B1*log(SI$Jacks)),log(SI$PostSI)-(SI$B0+SI$B1*log(SI$Jacks)), 2001:2022))
colnames(forecastmodelerror)<-c("error", "logerror", "run_year")
Hatchery<-read.csv("Maturation Uncertainty CWT.csv")
Hatchery<-Hatchery %>%
  filter(brood_year > 1997 & brood_year < 2017)
Hatchery$run_year<-Hatchery$brood_year+3
error_corr<-forecastmodelerror %>%
  left_join(Hatchery) %>%
  filter(run_year %in% 2001:2019)
plot(error_corr$logerror~error_corr$Mat2Mean, ylab = "error", xlab = "age-2 maturation rate")
text(error_corr$Mat2Mean,error_corr$logerror, labels=error_corr$brood_year+3)
cor(error_corr$Mat2Mean,error_corr$logerror)
cor(error_corr$Mat2Mean,error_corr$error)
#################################
### Plotting Production results##
#################################
setwd("C:/Users/echen/Box/Sacramento River Fall Chinook/Methodology Review")
Natural<-read.csv("Productivity.csv")
Natural<-Natural %>%
  filter(brood_year %in% 2008:2016)
mean(Natural$ProdMean)
median(Natural$ProdMean)
sd(Natural$ProdMean)
Hatchery<-read.csv("OutmigrationSurvival.csv")
Hatchery<-Hatchery %>%
  filter(brood_year %in% 1998:2016)
mean(Hatchery$OutSMean)
median(Hatchery$OutSMean)
sd(Hatchery$OutSMean)

S1<-Hatchery %>%
  left_join(Natural) 
cor(S1$OutSMean[11:19], S1$ProdMean[11:19])
coeff <- 400
ggplot(S1, aes(x=brood_year)) +
  geom_line(aes(y=OutSMean), colour = "#56B4E9") +
  geom_line(aes(y=ProdMean/coeff),colour = "#E69F00") + 
  scale_y_continuous(name = "Survival", sec.axis = sec_axis(~.*coeff, name="Productivity"))+
  blank_bg +
  scale_x_continuous("Brood Year", labels = as.character(S1$brood_year), breaks = S1$brood_year)
