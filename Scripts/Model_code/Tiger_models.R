#This script is for running GLMs on TigerSharks abundance at Cocos 
#created by Geoffrey J. Osgood
#


library(parameters)
library(mgcv)
library(glmmTMB)

setwd("c:/Users/gjosg/Documents/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019
#summary stats
mean(subset(cocos, TigerSharks>0)$TigerSharks)
max(subset(cocos, TigerSharks>0)$TigerSharks)

cocos$TigerSharks[cocos$TigerSharks<0]<-1 #keep all records for pres/abs analysis

nrow(subset(cocos, TigerSharks>0))/nrow(cocos)

cocos$TigerSharks<-as.numeric(cocos$TigerSharks>0)

#Turn fixed and random effects into factors if necessary
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)
cocos$ID <- as.factor(cocos$ID)

cocos<-subset(cocos, !is.na(Visibility))

#Remove NA's
cocos<-cocos[complete.cases(cocos[,c(9,10,13,16,18,20,33,35,36)]),]


cocos_tiger<-subset(cocos, Year>2006)

cocos_tiger$StudyYear<-cocos_tiger$StudyYear-14

#Run the model
##SST

TigerSharks.cubic<-glmmTMB(TigerSharks~StudyYear+scale(SST)+poly(ONI, 3, raw=TRUE)+
	CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos, family="binomial")

save(TigerSharks.cubic.short, file="c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/TigerSharks_cubic_short.Rdata")

TigerSharks.cubic.YRE<-glmmTMB(TigerSharks~StudyJulianDate+scale(SST)+poly(ONI, 3, raw=TRUE)+
	CurrentCode+scale(Visibility)+(1|StudyYear)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos, family="binomial")

save(TigerSharks.cubic.YRE, file="c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/TigerSharks_cubic_yre.Rdata")

######################
#load models##########
######################

load("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/TigerSharks_cubic_short_yre.Rdata")
load("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/TigerSharks_cubic_short.Rdata")

#for measuring year effects, model fit on whole time series:
load("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/TigerSharks_cubic_yre.Rdata")



########################################################################
####Assessing effect sizes##############################################
########################################################################

#set seed for simulations below:
set.seed(1)
model<-TigerSharks.cubic.short.YRE

#Count increase with SST:

mean_temp<-mean(cocos$SST)
sd_temp<-mean(cocos$SST)+sd(cocos$SST)

twentyfive<-(25-mean_temp)/sd(cocos$SST)
thirty<- (30-mean_temp)/sd(cocos$SST)

#functions for getting percents:

invlogit<-function(x) {exp(x)/(1+exp(x))}

percent_func<-function(a,bsst,year,sine,cosine,x,y,z)
 {((invlogit(a+bsst*x+year*mean(cocos$StudyYear)+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME)))-
 	invlogit(a+bsst*y+year*mean(cocos$StudyYear)+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))/invlogit(a+bsst*z+year*mean(cocos$StudyYear)+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))*100}

percent_func_tiger<-function(a,boni1,boni2,boni3,year,sine,cosine,x,y,z)
 {((invlogit(a+boni1*x+boni2*(x^2)+boni3*(x^3)+year*mean(cocos$StudyYear)+sine*mean(cocos$SIN_TIME)+cosine*mean(cocos$COS_TIME))-
 	invlogit(a+boni1*y+boni2*(y^2)+boni3*(y^3)+year*mean(cocos$StudyYear)+sine*mean(cocos$SIN_TIME)+cosine*mean(cocos$COS_TIME)))/invlogit(a+boni1*z+boni2*(z^2)+boni3*(z^3)+year*mean(cocos$StudyYear)+sine*mean(cocos$SIN_TIME)+cosine*mean(cocos$COS_TIME)))*100}

percent_func_oni<-function(a,boni,year,sine,cosine,x,y,z)
{((invlogit(a+boni*x+year*mean(cocos$StudyYear)+sine*mean(cocos$SIN_TIME)+cosine*mean(cocos$COS_TIME))-
     invlogit(a+boni*y+year*mean(cocos$StudyYear)+sine*mean(cocos$SIN_TIME)+cosine*mean(cocos$COS_TIME)))/invlogit(a+boni*z+year*mean(cocos$StudyYear)+sine*mean(cocos$SIN_TIME)+cosine*mean(cocos$COS_TIME)))*100}


#to get confidence intervals:

coefs<-parameters::simulate_model(model, iterations=10000)

percent_sim_oni<-percent_func_tiger(a=coefs[,1], boni1=coefs[,4], boni2=coefs[,5], boni3=coefs[,6], 
	year=coefs[,2], sine=coefs[,13], cosine=coefs[,14], x=2,y=-2,z=-2)

qnorm(0.025, mean=mean(percent_sim_oni), sd=sd(percent_sim_oni))
qnorm(0.975, mean=mean(percent_sim_oni), sd=sd(percent_sim_oni))

percent_sim_sst<-percent_func(a=coefs[,1], bsst=coefs[,3], 
	year=coefs[,2], sine=coefs[,13], cosine=coefs[,14], x=thirty,y=twentyfive,z=twentyfive)

qnorm(0.025, mean=mean(percent_sim_sst), sd=sd(percent_sim_sst))
qnorm(0.975, mean=mean(percent_sim_sst), sd=sd(percent_sim_sst))

#to get mean percents:
percent_func(a=summary(model)$coef$cond[1,1], bsst=summary(model)$coef$cond[3,1], 
	year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[13,1],
	cosine=summary(model)$coef$cond[14,1], x=0,y=1,z=0)

percent_func_tiger(a=summary(model)$coef$cond[1,1], boni1=summary(model)$coef$cond[4,1], 
	boni2=summary(model)$coef$cond[5,1], boni3 = summary(model)$coef$cond[6,1], 
	year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[13,1],
	cosine=summary(model)$coef$cond[14,1], x=2,y=-2,z=-2)

percent_func(a=summary(model)$coef$cond[1,1], 
        bsst=summary(model)$coef$cond[3,1], 
        year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
             cosine=summary(model)$coef$cond[12,1], x=twentyfive,y=thirty,z=twentyfive)

#Count changed with year:

coefs_year<-simulate_model(model, iterations=10000)

percent_func_year<-function(a,year,sine,cosine,x,y,z)
 {((invlogit(a+year*x+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME)))-
 	invlogit(a+year*y+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))/invlogit(a+year*z+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))*100}

percent_func_year(a=summary(model)$coef$cond[1,1], year=summary(model)$coef$cond[2,1],
 sine=summary(model)$coef$cond[13,1], cosine=summary(model)$coef$cond[14,1],
	x=0,y=365,z=0)


percent_sim_year<-percent_func_year(a=coefs_year[,1], 
year=coefs_year[,2], sine=coefs_year[,13], cosine=coefs_year[,14], x=0,y=365,z=0)

qnorm(0.025, mean=mean(percent_sim_year), sd=sd(percent_sim_year))
qnorm(0.975, mean=mean(percent_sim_year), sd=sd(percent_sim_year))

###########################################################################################################################
#predictions at mean SST, 1 above mean, 25, and thirty and weak, moderate, strong ONI######################################
###########################################################################################################################

#find the year with the most counts, take average julian date in that year
year_temp<-aggregate(TigerSharks~StudyYear, cocos_tiger, sum)

mode_year<-year_temp[which.max(year_temp$TigerSharks), "StudyYear"]
mode_date<-mean(subset(cocos_tiger, StudyYear==mode_year)$StudyJulianDate)

#make function for the predictions
predictFun <- function(x, mode_date, model, type="SST") {
  if (type=="SST") {
    invlogit(summary(model)$coef$cond[1,1]+summary(model)$coef$cond[3,1]*x+summary(model)$coef$cond[2,1]*mode_date)
  } else {
    if (type=="ONI") {
      invlogit(summary(model)$coef$cond[1,1]+summary(model)$coef$cond[4,1]*x+summary(model)$coef$cond[5,1]*x^2+
            summary(model)$coef$cond[6,1]*x^3+summary(model)$coef$cond[2,1]*mode_date)
    } 
  }
}

predictFun(0, mode_date, model, "SST")
predictFun(1, mode_date, model, "SST")
predictFun(twentyfive, mode_date, model, "SST")
predictFun(thirty, mode_date, model, "SST")

#########

predictFun(0.5, mode_date, model, "ONI")
predictFun(1, mode_date, model, "ONI")
predictFun(1.5, mode_date, model, "ONI")
predictFun(-0.5, mode_date, model, "ONI")
predictFun(-1, mode_date, model, "ONI")
predictFun(-1.5, mode_date, model, "ONI")


