#This script is for running GLMs on Blacktip abundance at Cocos 
#created by Geoffrey J. Osgood
#


library(glmmTMB)
library(parameters)

setwd("c:/Users/gjosg/Documents/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019

#summary stats
mean(subset(cocos, Blacktips>0)$Blacktips)
max(subset(cocos, Blacktips>0)$Blacktips)

cocos$Blacktips[cocos$Blacktips<0]<-1 #keep all records for pres/abs analysis

nrow(subset(cocos,Blacktips>0))/nrow(cocos)

cocos$Blacktips<-as.numeric(cocos$Blacktip>0)

#Turn fixed and random effects into factors if necessary
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)
cocos$ID <- as.factor(cocos$ID)

cocos<-subset(cocos, !is.na(Visibility))

cocos<-cocos[!is.na(cocos$Blacktips),]


#Run the model
##SST
Blacktip.linear<-glmmTMB(Blacktips~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos, family="binomial")

save(Blacktip.linear, file="c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/Blacktip_linear.Rdata")

Blacktip.linear.YRE<-glmmTMB(Blacktips~StudyJulianDate+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
                           (1|StudyYear)+(1|DiverCode)+(1|SiteCode), 
                         data=cocos, family="binomial")

summary(Blacktip.linear.YRE)

save(Blacktip.linear.YRE, file="c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/Blacktip_linear_yre.Rdata")


Blacktip.linear.nooni<-glmmTMB(Blacktips~StudyYear+scale(SST)+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=cocos, family="binomial")

save(Blacktip.linear.nooni, file="Models/Blacktip_linear_nooni.Rdata")

Blacktip.linear.nosst<-glmmTMB(Blacktips~StudyYear+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
                           (1|DiverCode)+(1|SiteCode), 
                         data=cocos, family="binomial")

save(Blacktip.linear.nosst, file="Models/Blacktip_linear_nosst.Rdata")


Blacktip.linear.notemp<-glmmTMB(Blacktips~StudyYear+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
                           (1|DiverCode)+(1|SiteCode), 
                         data=cocos, family="binomial")

save(Blacktip.linear.notemp, file="Models/Blacktip_linear_notemp.Rdata")

######################
#load models##########
######################

load("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/Blacktip_linear.Rdata")
load("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Models/Blacktip_linear_yre.Rdata")


########################################################################
####Assessing effect sizes##############################################
########################################################################
#Count increase with SST:
set.seed(1)

invlogit<-function(x) {exp(x)/(1+exp(x))}

model<-Blacktip.linear.YRE

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
summary(model)

percent_sim_oni<-percent_func_oni(a=coefs[,1], boni=coefs[,4], 
	year=coefs[,2], sine=coefs[,11], cosine=coefs[,12],x=1.5,y=-1.5,z=-1.5)

qnorm(0.025, mean=mean(percent_sim_oni), sd=sd(percent_sim_oni))
qnorm(0.975, mean=mean(percent_sim_oni), sd=sd(percent_sim_oni))

#to get mean percents:
percent_func(a=summary(model)$coef$cond[1,1], bsst=summary(model)$coef$cond[3,1], 
	year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
	cosine=summary(model)$coef$cond[12,1], x=twentyfive,y=thirty,z=twentyfive)

percent_func_oni(a=summary(model)$coef$cond[1,1], boni=summary(model)$coef$cond[4,1], 
	year=summary(model)$coef$cond[2,1], sine=summary(model)$coef$cond[11,1],
	cosine=summary(model)$coef$cond[12,1], x=1.5,y=-1.5,z=-1.5)


#Count changed with year:
model<-Blacktip.linear.YRE
coefs_year<-parameters::simulate_model(model, iterations=10000)

percent_func_year<-function(a,year,sine,cosine,x,y,z)
 {((invlogit(a+year*x+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME)))-
 	invlogit(a+year*y+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))/invlogit(a+year*z+sine*(mean(cocos$SIN_TIME))+cosine*(mean(cocos$COS_TIME))))*100}

percent_func_year(a=summary(model)$coef$cond[1,1], year=summary(model)$coef$cond[2,1], 
sine=summary(model)$coef$cond[11,1], cosine=summary(model)$coef$cond[12,1],
	x=0,y=9796,z=0)

percent_sim_year<-percent_func_year(a=coefs_year[,1], year=coefs_year[,2], sine=coefs_year[,11], 
	cosine=coefs_year[,12], x=0,y=365,z=0)

qnorm(0.025, mean=mean(percent_sim_year), sd=sd(percent_sim_year))
qnorm(0.975, mean=mean(percent_sim_year), sd=sd(percent_sim_year))
max(cocos$StudyJulianDate)

###########################################################################################################################
#predictions at mean SST, 1 above mean, 25, and thirty and weak, moderate, strong ONI######################################
###########################################################################################################################
#find the year with the most counts, take average julian date in that year
year_temp<-aggregate(Blacktips~StudyYear, cocos, sum)

mode_year<-year_temp[which.max(year_temp$Blacktips), "StudyYear"]
mode_date<-mean(subset(cocos, StudyYear==mode_year)$StudyJulianDate)


###make function for the predictions#####

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

#####

predictFun(0.5, mode_date, model, "ONI")
predictFun(1, mode_date, model, "ONI")
predictFun(1.5, mode_date, model, "ONI")
predictFun(-0.5, mode_date, model, "ONI")
predictFun(-1, mode_date, model, "ONI")
predictFun(-1.5, mode_date, model, "ONI")

