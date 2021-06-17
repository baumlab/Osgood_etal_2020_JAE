#This script is for running GLMs on MarbledRay abundance at Cocos 
#created by Geoffrey J. Osgood
#

library(glmmTMB)
library(parameters)

setwd("c:/Users/gjosg/Documents/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Dec 2020

#cocos<-subset(cocos, MarbledRays>=0) #remove -1's which indicate presence only

cocos$MarbledRays[cocos$MarbledRays>100]<-100 # change counts bigger than 100 to 100 


#Turn fixed and random effects into factors if necessary
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)
cocos$ID <- as.factor(cocos$ID)

#Remove NA's
cocos<-cocos[complete.cases(cocos[,c(9,10,13,16,18,20,23,35,36)]),]

#summary stats
nrow(subset(cocos, MarbledRays>0))/nrow(cocos)
mean(subset(cocos, MarbledRays>0)$MarbledRays)
max(subset(cocos, MarbledRays>0)$MarbledRays)

#Run the model

#SST

MarbledRay.linear<-glmmTMB(MarbledRays~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos, family="nbinom2")

save(MarbledRay.linear, file="c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/MarbledRay_linear.Rdata")

MarbledRay.linear.YRE<-glmmTMB(MarbledRays~StudyJulianDate+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
                            (1|StudyYear) + (1|DiverCode)+(1|SiteCode), 
                           data=cocos, family="nbinom2", ziformula = ~1)

save(MarbledRay.linear.YRE, file="c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/MarbledRay_linear_yre.Rdata")

######################
#load models##########
######################

load("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/MarbledRay_linear.Rdata")
load("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/MarbledRay_linear_yre.Rdata")


########################################################################
####Assessing effect sizes##############################################
########################################################################
#Count increase with SST:
model<-MarbledRay.linear.YRE
set.seed(1)

#to get confidence intervals:
coefs<-parameters::simulate_model(model, iterations=10000, component="conditional")

#mean effects:
mean_temp<-mean(cocos$SST)
sd_temp<-mean(cocos$SST)+sd(cocos$SST)

twentyfive<-(25-mean_temp)/sd(cocos$SST)
thirty<- (30-mean_temp)/sd(cocos$SST)
#functions for getting percents:

percent_func<-function(a,bsst,year,sine,cosine,x,y,z)
 {((exp(a+bsst*x+year*mean(cocos$StudyYear)+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME)))-
 	exp(a+bsst*y+year*mean(cocos$StudyYear)+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))/exp(a+bsst*z+year*mean(cocos$StudyYear)+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))*100}

percent_func_oni<-function(a,boni,year,sine,cosine,x,y,z)
 {((exp(a+boni*x+year*mean(cocos$StudyYear)+sine*mean(cocos$SIN_TIME)+cosine*mean(cocos$COS_TIME))-
 	exp(a+boni*y+year*mean(cocos$StudyYear)+sine*mean(cocos$SIN_TIME)+cosine*mean(cocos$COS_TIME)))/exp(a+boni*z+year*mean(cocos$StudyYear)+sine*mean(cocos$SIN_TIME)+cosine*mean(cocos$COS_TIME)))*100}

#to get mean percents:

#SST one degree
percent_func(a=summary(model)$coef$cond[1,1], bsst=summary(model)$coef$cond[3,1], 
	year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
	cosine=summary(model)$coef$cond[12,1], x=0,y=1,z=0)

percent_sim_sst<-percent_func(a=coefs[,1], bsst=coefs[,3], 
	year=coefs[,2], sine=coefs[,11], cosine=coefs[,12], x=0,y=1,z=0)

qnorm(0.025, mean=mean(percent_sim_sst), sd=sd(percent_sim_sst))
qnorm(0.975, mean=mean(percent_sim_sst), sd=sd(percent_sim_sst))

#SST five degrees
percent_func(a=summary(model)$coef$cond[1,1], bsst=summary(model)$coef$cond[3,1], 
	year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
	cosine=summary(model)$coef$cond[12,1], x=twentyfive,y=thirty,z=twentyfive)

percent_sim_sst<-percent_func(a=coefs[,1], bsst=coefs[,3], 
	year=coefs[,2], sine=coefs[,11], cosine=coefs[,12], x=twentyfive,y=thirty,z=twentyfive)

qnorm(0.025, mean=mean(percent_sim_sst), sd=sd(percent_sim_sst))
qnorm(0.975, mean=mean(percent_sim_sst), sd=sd(percent_sim_sst))

#ONI
percent_func_oni(a=summary(model)$coef$cond[1,1], boni=summary(model)$coef$cond[4,1], 
	year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
	cosine=summary(model)$coef$cond[12,1], x=-1,y=0,z=0)

percent_sim_oni<-percent_func_oni(a=coefs[,1], boni=coefs[,4], 
	year=coefs[,2], sine=coefs[,11], cosine=coefs[,12], x=-1,y=0,z=0)

qnorm(0.025, mean=mean(percent_sim_oni), sd=sd(percent_sim_oni))
qnorm(0.975, mean=mean(percent_sim_oni), sd=sd(percent_sim_oni))

#Count changed with year:
percent_func_year<-function(a,year,sine,cosine,x,y,z)
 {((exp(a+year*x+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME)))-
 	exp(a+year*y+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))/exp(a+year*z+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))*100}

percent_func_year(a=summary(model)$coef$cond[1,1], year=summary(model)$coef$cond[2,1], 
sine=summary(model)$coef$cond[11,1], cosine=summary(model)$coef$cond[12,1],
	x=0,y=9796,z=0)

percent_sim_year<-percent_func_year(a=coefs[,1], year=coefs[,2], sine=coefs[,11], 
	cosine=coefs[,12], x=0,y=9796,z=0)

qnorm(0.025, mean=mean(percent_sim_year), sd=sd(percent_sim_year))
qnorm(0.975, mean=mean(percent_sim_year), sd=sd(percent_sim_year))

###########################################################################################################################
#predictions at mean SST, 1 above mean, 25, and thirty and weak, moderate, strong ONI######################################
###########################################################################################################################

#find the year with the most counts, take average julian date in that year
year_temp<-aggregate(MarbledRays~StudyYear, cocos, sum)

mode_year<-year_temp[which.max(year_temp$MarbledRays), "StudyYear"]
mode_date<-mean(subset(cocos, StudyYear==mode_year)$StudyJulianDate)

#make function for the predictions
predictFun <- function(x, mode_date, model, type="SST") {
  if (type=="SST") {
    exp(summary(model)$coef$cond[1,1]+summary(model)$coef$cond[3,1]*x+summary(model)$coef$cond[2,1]*mode_date)
  } else {
    if (type=="ONI") {
      exp(summary(model)$coef$cond[1,1]+summary(model)$coef$cond[4,1]*x+summary(model)$coef$cond[2,1]*mode_date)
    } 
  }
}

predictFun(0, mode_date, model, "SST")
predictFun(1, mode_date, model, "SST")
predictFun(twentyfive, mode_date, model, "SST")
predictFun(thirty, mode_date, model, "SST")

########################################

predictFun(0.5, mode_date, model, "ONI")
predictFun(1, mode_date, model, "ONI")
predictFun(1.5, mode_date, model, "ONI")
predictFun(-0.5, mode_date, model, "ONI")
predictFun(-1, mode_date, model, "ONI")
predictFun(-1.5, mode_date, model, "ONI")
