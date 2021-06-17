#This script is for running GLMs on hammerhead abundance at Cocos 
#created by Geoffrey J. Osgood
#

library(glmmTMB)
library(parameters)
library(brms)

setwd("c:/Users/gjosg/Documents/Cocos_El_Nino/")

#  read in the data
cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) 

cocos<-subset(cocos, Hammerheads>=0) #remove -1's which indicate presence only

#Turn fixed and random effects into factors if necessary
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)
cocos$ID <- as.factor(cocos$ID)

#Remove NA's
cocos<-cocos[complete.cases(cocos[,c(9,10,13,16,18,20,21,35,36)]),]

#summary stats
nrow(subset(cocos, Hammerheads>0))/nrow(cocos)
mean(subset(cocos, Hammerheads>0)$Hammerheads)
max(subset(cocos, Hammerheads>0)$Hammerheads)


#Run the model
hammerhead.linear<-glmmTMB(Hammerheads~StudyYear+scale(SST)+ONI+
CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos, family="nbinom2")

save(hammerhead.linear, file="Models/hammerhead_linear.Rdata")


#Same model with year as random effect:
hammerhead.linear.YRE<-glmmTMB(Hammerheads~StudyJulianDate+scale(SST)+ONI+
CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode)+(1|StudyYear), 
	data=cocos, family="nbinom2", REML=TRUE)

save(hammerhead.linear.YRE, file="c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/hammerhead_linear_yre.Rdata")



######################
#load models##########
######################

load("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/hammerhead_linear_yre.Rdata")
load("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/hammerhead_linear.Rdata")

#assess significance
summary(hammerhead.linear.YRE)

########################################################################
####Assessing effect sizes##############################################
########################################################################
#Count increase with SST:
model<-hammerhead.linear.YRE
set.seed(1)

mean_temp<-mean(cocos$SST)
sd_temp<-mean(cocos$SST)+sd(cocos$SST)

twentyfive<-(25-mean_temp)/sd(cocos$SST)
thirty<- (30-mean_temp)/sd(cocos$SST)

#functions for getting percents:
percent_func<-function(a,bsst,x,y,z) {((exp(a+bsst*x)-exp(a+bsst*y))/exp(a+bsst*z))*100}

percent_func_oni<-function(a,boni,y,z) {((exp(a+boni*x)-exp(a+boni*y))/exp(a+boni*z))*100}


#to get confidence intervals:

coefs<-parameters::simulate_model(model, iterations=10000, component="conditional")

percent_sim_sst<-percent_func(a=coefs[,1], bsst=coefs[,3], 
	year=coefs[,2], sine=coefs[,11], cosine=coefs[,12], x=0,y=1,z=0)

qnorm(0.025, mean=mean(percent_sim_sst), sd=sd(percent_sim_sst))
qnorm(0.975, mean=mean(percent_sim_sst), sd=sd(percent_sim_sst))


#to get mean percents - five degrees:
percent_func(a=summary(model)$coef$cond[1,1], bsst=summary(model)$coef$cond[3,1], 
	x=twentyfive,y=thirty,z=twentyfive)

#to get mean percents - one degree:
percent_func(a=summary(model)$coef$cond[1,1], bsst=summary(model)$coef$cond[3,1], 
             year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
             cosine=summary(model)$coef$cond[12,1], x=0,y=1,z=0)

#ONI
percent_sim_oni<-percent_func_oni(a=coefs[,1], boni=coefs[,4], 
                                  year=coefs[,2], sine=coefs[,11], 
                                  cosine=coefs[,12],x=0,y=-1,z=0)

qnorm(0.025, mean=mean(percent_sim_oni), sd=sd(percent_sim_oni))
qnorm(0.975, mean=mean(percent_sim_oni), sd=sd(percent_sim_oni))

#weak ONI
percent_func_oni(a=summary(model)$coef$cond[1,1], boni=summary(model)$coef$cond[4,1], 
	year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
	cosine=summary(model)$coef$cond[12,1], x=0,y=0.5,z=0)

percent_func_oni(a=summary(model)$coef$cond[1,1], boni=summary(model)$coef$cond[4,1], 
                 year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
                 cosine=summary(model)$coef$cond[12,1], x=0,y=0.5,z=0)

#ONI - strong La Nina compared to strong El Nino
percent_func_oni(a=summary(model)$coef$cond[1,1], boni=summary(model)$coef$cond[4,1], 
                 year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
                 cosine=summary(model)$coef$cond[12,1], x=-1.5,y=1.5,z=1.5)


#Count changed with year:

percent_func_year<-function(a,year,sine,cosine,x,y,z)
 {((exp(a+year*x+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME)))-
 	exp(a+year*y+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))/exp(a+year*z+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))*100}

percent_func_year(a=summary(model)$coef$cond[1,1], 
year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1], 
cosine=summary(model)$coef$cond[12,1],
	x=0,y=9796,z=0)

percent_sim_year<-percent_func_year(a=coefs[,1], year=coefs[,2], 
	sine=coefs[,11], 
	cosine=coefs_year[,12], x=0,y=9796,z=0)


qnorm(0.025, mean=mean(percent_sim_year), sd=sd(percent_sim_year))
qnorm(0.975, mean=mean(percent_sim_year), sd=sd(percent_sim_year))

###########################################################################################################################
#predictions at mean SST, 1 above mean, 25, and thirty and weak, moderate, strong ONI######################################
###########################################################################################################################

#find the year with the most counts, take average julian date in that year
year_temp<-aggregate(Hammerheads~StudyYear, cocos, sum)

mode_year<-year_temp[which.max(year_temp$Hammerheads), "StudyYear"]
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

predictFun(0.5, mode_date, model, "ONI")
predictFun(1, mode_date, model, "ONI")
predictFun(1.5, mode_date, model, "ONI")
predictFun(-0.5, mode_date, model, "ONI")
predictFun(-1, mode_date, model, "ONI")
predictFun(-1.5, mode_date, model, "ONI")
