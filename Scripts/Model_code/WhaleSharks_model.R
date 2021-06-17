#This script is for running GLMs on WhaleSharks abundance at Cocos 
#created by Geoffrey J. Osgood
#

library(here)
library(forecast)
library(ggplot2)
library(mgcv)
library(glmmTMB)
library(glmmADMB)

setwd("~/Documents/git_repos/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019

cocos$WhaleSharks[cocos$WhaleSharks<0]<-1 #change -1 to 1 to indicate presence

cocos$WhaleSharks<-as.numeric(cocos$WhaleSharks>0) #change to pres/abs only


#create dive ID to indicate unique dives (ie multiple observations from multiple dive guides on the same dive)
cocos$ID<- as.numeric(factor(paste(cocos$Date, cocos$Time, cocos$Site)))
cocos$ID[is.na(cocos$Time)] <- paste("NA_", cocos$ChronoSort[is.na(cocos$Time)], sep="")

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


presab_func<-function(x) {

	if(sum(x)>0) {1} else {0}

}


cocos<-cocos[!is.na(cocos$WhaleSharks),]

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

WhaleSharks_ag<-aggregate(WhaleSharks~ID+Date+SiteCode+Month+Day+Year+StudyJulianDate+StudyYear+StudyMonth,
 presab_func, data=cocos)

sineag<-aggregate(SIN_TIME~ID+Date+SiteCode+Month+Day+Year+StudyJulianDate+StudyYear+StudyMonth,
 mean, data=cocos)

cosag<-aggregate(COS_TIME~ID+Date+SiteCode+Month+Day+Year+StudyJulianDate+StudyYear+StudyMonth,
 mean, data=cocos)


cocos_analysis<-data.frame(Date=WhaleSharks_ag$Date, Month=WhaleSharks_ag$Month, Day=WhaleSharks_ag$Day, Year=
	WhaleSharks_ag$Year, StudyJulianDate=WhaleSharks_ag$StudyJulianDate, StudyYear=WhaleSharks_ag$StudyYear,
	StudyMonth=WhaleSharks_ag$StudyMonth, Visibility=Visibility_ag$Visibility,
	CurrentCode=factor(Current_ag$"as.numeric(as.character(cocos$CurrentCode))"),
	SST=Temp_ag$SST, Anomaly=Anomaly_ag$Anomaly, ONI=ONI_ag$ONI, DiverCode=dive_ag$DiverCode,
	SiteCode=WhaleSharks_ag$SiteCode,
	WhaleSharks=WhaleSharks_ag$WhaleSharks, SIN_TIME=sineag$SIN_TIME, COS_TIME=cosag$COS_TIME)


#Run the model

#ID not converge

##SST and ONI
WhaleSharks.linear.sst.noint<-glmmTMB(WhaleSharks~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos_analysis, family="binomial")

save(WhaleSharks.linear.sst.noint, file="Models/WhaleSharks_linear_sst_noint.Rdata")


WhaleSharks.linear.sst.noint.id<-glmmTMB(WhaleSharks~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode)+(1|ID), 
	data=cocos, family="binomial")


glmmTMBControl(optCtrl=list(iter.max=3e3,eval.max=3e3))


save(WhaleSharks.linear.sst.noint.id, file="Models/WhaleSharks_linear_sst_noint_id.Rdata")

###Anomaly
WhaleSharks.linear.anom.noint.id<-glmmTMB(WhaleSharks~StudyYear+Anomaly+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode)+(1|ID), 
	data=cocos, family="binomial")

save(WhaleSharks.linear.anom.noint.id, file="Models/WhaleSharks_linear_anom_noint_id.Rdata")

WhaleSharks.linear.anom.noint<-glmmTMB(WhaleSharks~StudyYear+Anomaly+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos_analysis, family="binomial")

save(WhaleSharks.linear.anom.noint, file="Models/WhaleSharks_linear_anom_noint.Rdata")


###Nonlinear
WhaleSharks.nonlinear.sst.noint<-glmmTMB(WhaleSharks~poly(StudyYear,2, raw=TRUE)+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos_analysis, family="binomial")

save(WhaleSharks.nonlinear.sst.noint, file="Models/WhaleSharks_nonlinear_sst_noint.Rdata")

#Interaction with SST and year
WhaleSharks.linear.sst.int<-glmmTMB(WhaleSharks~StudyYear*scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos_analysis, family="binomial")

save(WhaleSharks.linear.sst.int, file="Models/WhaleSharks_linear_sst_int.Rdata")


