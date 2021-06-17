#This script is for running GLMs on Silvertip abundance at Cocos 
#created by Geoffrey J. Osgood
#


library(forecast)
library(ggplot2)
library(mgcv)
library(glmmTMB)
library(glmmADMB)

setwd("~/Documents/git_repos/Cocos_El_Nino/")
setwd("c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019
cocos$Silvertips[cocos$Silvertips<0]<-1 #keep all records for pres/abs analysis

cocos$Silvertips<-as.numeric(cocos$Silvertips>0)

#Turn fixed and random effects into factors if necessary
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)
cocos$ID <- as.factor(cocos$ID)

cocos<-subset(cocos, !is.na(Visibility))
nrow(cocos)
#Average counts done on same dive

#pick the dive guide with the most dives
ag_diveguides<-function (x) {

	trial2<-subset(cocos, DiverCode%in%x)

	df.trial<-data.frame(table(trial2$DiverCode))

	return(as.character(df.trial[which.max(df.trial$Freq),1]))

	}


#make a function to give the mean or NA per dive
mean_func<-function(x) {

	if(sum(x!=(-1))==0) {-1} else {

	if(sum(!is.na(x))>0) 

	{round(mean(x[x>=0], na.rm=TRUE),0)}
}

}


cocos<-cocos[!is.na(cocos$Silvertips),]

Current_ag<-aggregate(as.numeric(as.character(cocos$CurrentCode))~ID+Date+SiteCode+Month+Day+Year+StudyJulianDate+StudyYear,
 mean_func, data=cocos)

Temp_ag<-aggregate(SST~ID+Date+SiteCode+Month+Day+Year+StudyJulianDate+StudyYear+StudyMonth,
 mean, data=cocos)

Anomaly_ag<-aggregate(Anomaly~ID+Date+SiteCode+Month+Day+Year+StudyJulianDate+StudyYear+StudyMonth,
 mean, data=cocos)

Visibility_ag<-aggregate(Visibility~ID+Date+SiteCode+Month+Day+Year+StudyJulianDate+StudyYear+StudyMonth,
 mean, data=cocos)

ONI_ag<-aggregate(ONI~ID+Date+SiteCode+Month+Day+Year+StudyJulianDate+StudyYear+StudyMonth,
 mean, data=cocos)

dive_ag<-aggregate(DiverCode~ID+Date+SiteCode+Month+Day+Year+StudyJulianDate+StudyYear+StudyMonth, ag_diveguides, data=cocos)

Silvertips_ag<-aggregate(Silvertips~ID+Date+SiteCode+Month+Day+Year+StudyJulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

sineag<-aggregate(SIN_TIME~ID+Date+SiteCode+Month+Day+Year+StudyJulianDate+StudyYear+StudyMonth,
 mean, data=cocos)

cosag<-aggregate(COS_TIME~ID+Date+SiteCode+Month+Day+Year+StudyJulianDate+StudyYear+StudyMonth,
 mean, data=cocos)


cocos_analysis<-data.frame(Date=Silvertips_ag$Date, Month=Silvertips_ag$Month, Day=Silvertips_ag$Day, Year=
	Silvertips_ag$Year, StudyJulianDate=Silvertips_ag$StudyJulianDate, StudyYear=Silvertips_ag$StudyYear,
	StudyMonth=Silvertips_ag$StudyMonth, Visibility=Visibility_ag$Visibility,
	CurrentCode=factor(Current_ag$"as.numeric(as.character(cocos$CurrentCode))"),
	SST=Temp_ag$SST, Anomaly=Anomaly_ag$Anomaly, ONI=ONI_ag$ONI, DiverCode=dive_ag$DiverCode,
	SiteCode=Silvertips_ag$SiteCode,
	Silvertips=Silvertips_ag$Silvertips, SIN_TIME=sineag$SIN_TIME, COS_TIME=cosag$COS_TIME)


nrow(Silvertips_ag[Silvertips_ag$Silvertips>0,])/nrow(Silvertips_ag)
sd(subset(cocos, Silvertips>0)$Silvertips)


#Run the model
##SST
Silvertip.linear.sst.noint<-glmmTMB(Silvertips~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos_analysis, family="binomial")

save(Silvertip.linear.sst.noint, file="Models/Silvertip_linear_sst_noint.Rdata")

Silvertip.linear.sst.noint.id<-glmmTMB(Silvertips~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode)+(1|ID), 
	data=cocos, family="binomial")

save(Silvertip.linear.sst.noint.id, file="Models/Silvertip_linear_sst_noint_id.Rdata")

#Anomaly

Silvertip.linear.anom.noint.id<-glmmTMB(Silvertips~StudyYear+Anomaly+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
                                          (1|DiverCode)+(1|SiteCode)+(1|ID), 
                                        data=cocos, family="binomial")

save(Silvertip.linear.anom.noint.id, file="Models/Silvertip_linear_anom_noint_id.Rdata")


Silvertip.linear.anom.noint<-glmmTMB(Silvertips~StudyYear+Anomaly+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos_analysis, family="binomial")

save(Silvertip.linear.anom.noint, file="Models/Silvertip_linear_anom_noint.Rdata")


#Nonlinear
Silvertip.nonlinear.sst.noint.id<-glmmTMB(Silvertips~StudyYear+scale(SST)+poly(ONI,2, raw=TRUE)+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode)+(1|ID), 
	data=cocos, family="binomial")

save(Silvertip.nonlinear.sst.noint.id, file="Models/Silvertip_nonlinear_sst_noint_id.Rdata")

Silvertip.nonlinear.anom.noint.id<-glmmTMB(Silvertips~StudyYear+Anomaly+poly(ONI,2, raw=TRUE)+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode)+(1|ID), 
	data=cocos, family="binomial")

save(Silvertip.nonlinear.anom.noint.id, file="Models/Silvertip_nonlinear_anom_noint_id.Rdata")


#Interaction
Silvertip.linear.sst.int<-glmmTMB(Silvertips~StudyYear*scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode)+(1|ID), 
	data=cocos, family="binomial")

save(Silvertip.linear.sst.int, file="Models/Silvertip_linear_sst_int.Rdata")

######################
#load models##########
######################

load("Models/Silvertip_linear_sst_noint_id.Rdata")
load("Models/Silvertip_linear_anom_noint_id.Rdata")

########################################################################
####Assessing effect sizes##############################################
########################################################################
#Count increase with SST:
model<-Silvertip.linear.sst.noint.id

a<-summary(model)$coefficients$cond[1,1]
bsst<-summary(model)$coefficients$cond[3,1]
boni<-summary(model)$coefficients$cond[4,1]

mean_temp<-mean(cocos$SST)
sd_temp<-mean(cocos$SST)+sd(cocos$SST)

sst_mean<-round(((exp(a+bsst*mean_temp)-exp(a+bsst*sd_temp))/exp(a+bsst*mean_temp))*100, 1)
sst_25<-round(((exp(a+bsst*25)-exp(a+bsst*30))/exp(a+bsst*25))*100, 1)

sst_mean
sst_25

oni_weak<-round(((exp(a+boni*0)-exp(a+boni*1))/exp(a+boni*0))*100, 1)
oni_nina<-round(((exp(a+boni*(-1))-exp(a+boni*(0)))/exp(a+boni*(0)))*100, 1)
oni_strong<-round(((exp(a+boni*(-1.5))-exp(a+boni*(1.5)))/exp(a+boni*(-1.5)))*100, 1)

oni_weak
oni_nina
oni_strong