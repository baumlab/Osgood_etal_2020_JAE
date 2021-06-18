#This script is for running GLMs on Galapagos abundance at Cocos 
#created by Geoffrey J. Osgood
#


library(forecast)
library(ggplot2)
library(mgcv)
library(glmmTMB)
library(glmmADMB)

setwd("c:/gjosg/Documents/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019

cocos$Galapagos[cocos$Galapagos<0]<-1 #change -1 to 1 to indicate presence

cocos$Galapagos<-as.numeric(cocos$Galapagos>0) #change to pres/abs only

#fix visibility
cocos$Visibility[cocos$Visibility==442]<-43
cocos$Visibility[cocos$Visibility==1256]<-37

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

cocos<-cocos[!is.na(cocos$Galapagos),]

#Run the model
#SST
Galapagos.linear.sst.noint<-glmmTMB(Galapagos~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos, family="binomial")

Galapagos.linear.sst.noint.YRE<-glmmTMB(Galapagos~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
                             (1|factor(StudyYear))+ (1|DiverCode)+(1|SiteCode), 
                                    data=cocos, family="binomial")



save(Galapagos.linear.sst.noint, file="Galapagos_linear_sst_noint.Rdata")

summary(Galapagos.linear.sst.noint)

#Anomaly
Galapagos.linear.anom.noint<-glmmTMB(Galapagos~StudyYear+Anomaly+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos, family="nbinom2", ziformula=~1)

save(Galapagos.linear.anom.noint, file="Galapagos_linear_anom_noint.Rdata")

#Non-linear
Galapagos.nonlinear.sst.noint<-glmmTMB(Galapagos~poly(StudyYear,2, raw=TRUE)+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode)+(1|ID), 
	data=cocos, family="nbinom2", ziformula=~1)

save(Galapagos.nonlinear.sst.noint, file="Galapagos_nonlinear_sst_noint.Rdata")

Galapagos.linear.sst.int<-glmmTMB(Galapagos~StudyYear*scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode)+(1|ID), 
	data=cocos, family="nbinom2", ziformula=~1)

save(Galapagos.linear.sst.int, file="Galapagos_linear_sst_int.Rdata")


Galapagos.linear.sst.anom<-glmmTMB(Galapagos~StudyYear+Anomaly+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode)+(1|ID), 
	data=cocos, family="nbinom2", ziformula=~1)

save(Galapagos.linear.sst.anom, file="Galapagos_linear_sst_anom.Rdata")


summary(Galapagos.linear.sst.noint)
summary(Galapagos.linear.anom.noint)
summary(Galapagos.linear.sst.anom)
summary(Galapagos.linear.sst.int)
