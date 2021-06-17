#This script is for running GLMs on MobulaRay abundance at Cocos 
#created by Geoffrey J. Osgood
#


library(mgcv)
library(glmmTMB)

setwd("c:/Users/gjosg/Documents/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019

cocos$MobulaRays[cocos$MobulaRays<0]<-1 #change -1 to 1 to indicate presence
cocos$MantaRays[cocos$MantaRays<0]<-1 #change -1 to 1 to indicate presence
cocos$MobulaRays<-(cocos$MantaRays+cocos$MobulaRays) #combine all Mobula rays

#summary stats
mean(subset(cocos, MobulaRays>0)$MobulaRays)
max(subset(cocos, MobulaRays>0)$MobulaRays)


cocos$MobulaRays<-as.numeric(cocos$MobulaRays>0) #change to pres/abs only


#cocos<-subset(cocos, MobulaRays>=0) #remove -1's which indicate presence only
#cocos<-subset(cocos, MantaRays>=0) #remove -1's which indicate presence only


#create dive ID to indicate unique dives (ie multiple observations from multiple dive guides on the same dive)
cocos$ID<- as.numeric(factor(paste(cocos$Date, cocos$Time, cocos$Site)))
cocos$ID[is.na(cocos$Time)] <- paste("NA_", cocos$ChronoSort[is.na(cocos$Time)], sep="")

#Turn fixed and random effects into factors if necessary
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)
cocos$ID <- as.factor(cocos$ID)

cocos<-subset(cocos, !is.na(Visibility))

cocos<-cocos[!is.na(cocos$MobulaRays),]


#Run the model
#SST

MobulaRay.linear<-glmmTMB(MobulaRays~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos, family="binomial")

MobulaRay.linear.YRE<-glmmTMB(MobulaRays~StudyJulianDate+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
                          (1|StudyYear) + (1|DiverCode)+(1|SiteCode), 
                          data=cocos, family="binomial")

summary(MobulaRay.linear.YRE)

save(MobulaRay.linear.YRE, file="c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/MobulaRay_linear_yre.Rdata")


######################
#load models##########
######################

load("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/MobulaRay_linear.Rdata")
load("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/MobulaRay_linear_yre.Rdata")


########################################################################
####Assessing effect sizes##############################################
########################################################################
#Count increase with SST:
invlogit<-function(x) {exp(x)/(1+exp(x))}

model<-MobulaRay.linear.YRE
set.seed(1)

mean_temp<-mean(cocos$SST)
sd_temp<-mean(cocos$SST)+sd(cocos$SST)

twentyfive<-(25-mean_temp)/sd(cocos$SST)
thirty<- (30-mean_temp)/sd(cocos$SST)

#functions for getting percents:

percent_func<-function(a,bsst,year,sine,cosine,x,y,z)
 {((invlogit(a+bsst*x+year*mean(cocos$StudyYear)+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME)))-
  invlogit(a+bsst*y+year*mean(cocos$StudyYear)+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))/invlogit(a+bsst*z+year*mean(cocos$StudyYear)+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))*100}

percent_func_oni<-function(a,boni,year,sine,cosine,x,y,z)
 {((invlogit(a+boni*x+year*mean(cocos$StudyYear)+sine*mean(cocos$SIN_TIME)+cosine*mean(cocos$COS_TIME))-
  invlogit(a+boni*y+year*mean(cocos$StudyYear)+sine*mean(cocos$SIN_TIME)+cosine*mean(cocos$COS_TIME)))/invlogit(a+boni*z+year*mean(cocos$StudyYear)+sine*mean(cocos$SIN_TIME)+cosine*mean(cocos$COS_TIME)))*100}

#to get confidence intervals:

coefs<-parameters::simulate_model(model, iterations=10000)

#5 degrees
percent_sim_sst<-percent_func(a=coefs[,1], bsst=coefs[,3], 
  year=coefs[,2], sine=coefs[,11], cosine=coefs[,12], x=twentyfive,y=thirty,z=twentyfive)

qnorm(0.025, mean=mean(percent_sim_sst), sd=sd(percent_sim_sst))
qnorm(0.975, mean=mean(percent_sim_sst), sd=sd(percent_sim_sst))

#1 degree
percent_sim_sst<-percent_func(a=coefs[,1], bsst=coefs[,3], 
  year=coefs[,2], sine=coefs[,11], cosine=coefs[,12], x=0,y=1,z=0)

qnorm(0.025, mean=mean(percent_sim_sst), sd=sd(percent_sim_sst))
qnorm(0.975, mean=mean(percent_sim_sst), sd=sd(percent_sim_sst))

percent_sim_oni<-percent_func_oni(a=coefs[,1], boni=coefs[,4], 
  year=coefs[,2], sine=coefs[,11], cosine=coefs[,12],x=-1,y=0,z=0)

qnorm(0.025, mean=mean(percent_sim_oni), sd=sd(percent_sim_oni))
qnorm(0.975, mean=mean(percent_sim_oni), sd=sd(percent_sim_oni))

#to get mean percents:
#one degree
percent_func(a=summary(model)$coef$cond[1,1], bsst=summary(model)$coef$cond[3,1], 
  year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
  cosine=summary(model)$coef$cond[12,1], x=0,y=1,z=0)

#five degrees
percent_func(a=summary(model)$coef$cond[1,1], bsst=summary(model)$coef$cond[3,1], 
  year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
  cosine=summary(model)$coef$cond[12,1], x=twentyfive,y=thirty,z=twentyfive)

percent_func_oni(a=summary(model)$coef$cond[1,1], boni=summary(model)$coef$cond[4,1], 
  year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
  cosine=summary(model)$coef$cond[12,1], x=-1,y=0,z=0)

#Count changed with year:

percent_func_year<-function(a,year,sine,cosine,x,y,z)
 {((invlogit(a+year*x+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME)))-
 	invlogit(a+year*y+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))/invlogit(a+year*z+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))*100}

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
year_temp<-aggregate(MobulaRays~StudyYear, cocos, sum)

mode_year<-year_temp[which.max(year_temp$MobulaRays), "StudyYear"]
mode_date<-mean(subset(cocos, StudyYear==mode_year)$StudyJulianDate)

#make function for the predictions
predictFun <- function(x, mode_date, model, type="SST") {
  if (type=="SST") {
    invlogit(summary(model)$coef$cond[1,1]+summary(model)$coef$cond[3,1]*x+summary(model)$coef$cond[2,1]*mode_date)
  } else {
    if (type=="ONI") {
      invlogit(summary(model)$coef$cond[1,1]+summary(model)$coef$cond[4,1]*x+summary(model)$coef$cond[2,1]*mode_date)
    } 
  }
}

predictFun(0, mode_date, model, "SST")
predictFun(1, mode_date, model, "SST")
predictFun(twentyfive, mode_date, model, "SST")
predictFun(thirty, mode_date, model, "SST")

###############################

predictFun(0.5, mode_date, model, "ONI")
predictFun(1, mode_date, model, "ONI")
predictFun(1.5, mode_date, model, "ONI")
predictFun(-0.5, mode_date, model, "ONI")
predictFun(-1, mode_date, model, "ONI")
predictFun(-1.5, mode_date, model, "ONI")
