#This script is for running the power analysis and making figure 4
#created by Geoffrey J. Osgood
#


library(forecast)
library(ggplot2)
library(mgcv)
library(glmmTMB)
library(glmmADMB)

setwd("c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/")
setwd("~/Documents/git_repos/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019

cocos<-subset(cocos, Hammerheads>=0) #remove -1's which indicate presence only

#Remove NAs in Visibility
cocos<-subset(cocos, !is.na(Visibility))

#Turn fixed and random effects into factors if necessary
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)
cocos$ID <- as.factor(cocos$ID)

cocos<-cocos[!is.na(cocos$Hammerheads),]




########################################################################
####Power analysis #####################################################
########################################################################

#five year time series
five_years<-1993:2015

effect_five_oni<-NULL
effect_five_no_oni<-NULL

pvalue_five_oni<-NULL
pvalue_five_no_oni<-NULL

for (i in 1:length(five_years)) {

	year_1<-1993+(i-1)
	year_end<-year_1+4

	temp<-subset(cocos, Year>=year_1 & Year<=year_end)
	m1<-glmmTMB(Hammerheads~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=temp, family="nbinom2")

	m2<-glmmTMB(Hammerheads~StudyYear+scale(SST)+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=temp, family="nbinom2")

	effect_five_oni[i]<-summary(m1)$coefficients$cond[2,1]
	effect_five_no_oni[i]<-summary(m2)$coefficients$cond[2,1]

	pvalue_five_oni[i]<-summary(m1)$coefficients$cond[2,4]
	pvalue_five_no_oni[i]<-summary(m2)$coefficients$cond[2,4]


}

#ten year time series
ten_years<-1993:2010

effect_ten_oni<-NULL
effect_ten_no_oni<-NULL

pvalue_ten_oni<-NULL
pvalue_ten_no_oni<-NULL


for (i in 1:length(ten_years)) {

	year_1<-1993+(i-1)
	year_end<-year_1+9

	temp<-subset(cocos, Year>=year_1 & Year<=year_end)
	m1<-glmmTMB(Hammerheads~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=temp, family="nbinom2")

	m2<-glmmTMB(Hammerheads~StudyYear+scale(SST)+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
	(1|DiverCode)+(1|SiteCode), 
	data=temp, family="nbinom2")

	effect_ten_oni[i]<-summary(m1)$coefficients$cond[2,1]
	effect_ten_no_oni[i]<-summary(m2)$coefficients$cond[2,1]

	pvalue_ten_oni[i]<-summary(m1)$coefficients$cond[2,4]
	pvalue_ten_no_oni[i]<-summary(m2)$coefficients$cond[2,4]


}

#fifteen year time series
fifteen_years<-1993:2005

effect_fifteen_oni<-NULL
effect_fifteen_no_oni<-NULL

pvalue_fifteen_oni<-NULL
pvalue_fifteen_no_oni<-NULL

for (i in 1:length(fifteen_years)) {
  
  year_1<-1993+(i-1)
  year_end<-year_1+14
  
  temp<-subset(cocos, Year>=year_1 & Year<=year_end)
  m1<-glmmTMB(Hammerheads~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
                (1|DiverCode)+(1|SiteCode), 
              data=temp, family="nbinom2")
  
  m2<-glmmTMB(Hammerheads~StudyYear+scale(SST)+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
                (1|DiverCode)+(1|SiteCode), 
              data=temp, family="nbinom2")
  
  effect_fifteen_oni[i]<-summary(m1)$coefficients$cond[2,1]
  effect_fifteen_no_oni[i]<-summary(m2)$coefficients$cond[2,1]
  
  pvalue_fifteen_oni[i]<-summary(m1)$coefficients$cond[2,4]
  pvalue_fifteen_no_oni[i]<-summary(m2)$coefficients$cond[2,4]

  
}

#twenty year time series
twenty_years<-1993:2000

effect_twenty_oni<-NULL
effect_twenty_no_oni<-NULL

pvalue_twenty_oni<-NULL
pvalue_twenty_no_oni<-NULL


for (i in 1:length(twenty_years)) {
  
  year_1<-1993+(i-1)
  year_end<-year_1+19
  
  temp<-subset(cocos, Year>=year_1 & Year<=year_end)
  m1<-glmmTMB(Hammerheads~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
                (1|DiverCode)+(1|SiteCode), 
              data=temp, family="nbinom2")
  
  m2<-glmmTMB(Hammerheads~StudyYear+scale(SST)+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME+
                (1|DiverCode)+(1|SiteCode), 
              data=temp, family="nbinom2")
  
  effect_twenty_oni[i]<-summary(m1)$coefficients$cond[2,1]
  effect_twenty_no_oni[i]<-summary(m2)$coefficients$cond[2,1]

  pvalue_twenty_oni[i]<-summary(m1)$coefficients$cond[2,4]
  pvalue_twenty_no_oni[i]<-summary(m2)$coefficients$cond[2,4]

  
  
}

df_five<-data.frame(effect_size_oni=effect_five_oni, effect_size_no_oni=effect_five_no_oni, 
	pvalue_oni=pvalue_five_oni, pvalue_no_oni=pvalue_five_no_oni)

save(df_five, file="Models/five_years.Rdata")

df_ten<-data.frame(effect_size_oni=effect_ten_oni, effect_size_no_oni=effect_ten_no_oni, 
	pvalue_oni=pvalue_ten_oni, pvalue_no_oni=pvalue_ten_no_oni)

save(df_ten, file="Models/ten_years.Rdata")

df_fifteen<-data.frame(effect_size_oni=effect_fifteen_oni, effect_size_no_oni=effect_fifteen_no_oni, 
	pvalue_oni=pvalue_fifteen_oni, pvalue_no_oni=pvalue_fifteen_no_oni)

save(df_fifteen, file="Models/fifteen_years.Rdata")

df_twenty<-data.frame(effect_size_oni=effect_twenty_oni, effect_size_no_oni=effect_twenty_no_oni, 
	pvalue_oni=pvalue_twenty_oni, pvalue_no_oni=pvalue_twenty_no_oni)

save(df_twenty, file="Models/twenty_years.Rdata")



mean(effect_ten_no_oni)
mean(effect_ten_oni)

mean(effect_five_no_oni)
mean(effect_five_oni)

mean(effect_fifteen_no_oni)
mean(effect_fifteen_oni)

mean(effect_twenty_no_oni)
mean(effect_twenty_oni)




######################################
##Power analysis random observations##
######################################
#tenth of time series

set.seed(66)

effect_tenth_oni<-NULL
effect_tenth_no_oni<-NULL

pvalue_tenth_oni<-NULL
pvalue_tenth_no_oni<-NULL


for (i in 1:1000) {

	sample_dives<-sample(1:nrow(cocos), 0.1*nrow(cocos), replace=FALSE)

	temp<-cocos[sample_dives,]
	m1<-glmmTMB(Hammerheads~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME, 
	data=temp, family="nbinom2")

	m2<-glmmTMB(Hammerheads~StudyYear+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME, 
	data=temp, family="nbinom2")

	effect_tenth_oni[i]<-summary(m1)$coefficients$cond[2,1]
	effect_tenth_no_oni[i]<-summary(m2)$coefficients$cond[2,1]

	pvalue_tenth_oni[i]<-summary(m1)$coefficients$cond[2,4]
	pvalue_tenth_no_oni[i]<-summary(m2)$coefficients$cond[2,4]


}

#quarter of time series
mean(effect_tenth_no_oni)
effect_quarter_oni<-NULL
effect_quarter_no_oni<-NULL

pvalue_quarter_oni<-NULL
pvalue_quarter_no_oni<-NULL


for (i in 1:1000) {

	sample_dives<-sample(1:nrow(cocos), 0.25*nrow(cocos), replace=FALSE)

	temp<-cocos[sample_dives,]
	m1<-glmmTMB(Hammerheads~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME, 
	data=temp, family="nbinom2")

	m2<-glmmTMB(Hammerheads~StudyYear+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME, 
	data=temp, family="nbinom2")

	effect_quarter_oni[i]<-summary(m1)$coefficients$cond[2,1]
	effect_quarter_no_oni[i]<-summary(m2)$coefficients$cond[2,1]

	pvalue_quarter_oni[i]<-summary(m1)$coefficients$cond[2,4]
	pvalue_quarter_no_oni[i]<-summary(m2)$coefficients$cond[2,4]

}

#half of time series

effect_half_oni<-NULL
effect_half_no_oni<-NULL

pvalue_half_oni<-NULL
pvalue_half_no_oni<-NULL

for (i in 1:1000) {

	sample_dives<-sample(1:nrow(cocos), 0.5*nrow(cocos), replace=FALSE)

	temp<-cocos[sample_dives,]
	m1<-glmmTMB(Hammerheads~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME, 
	data=temp, family="nbinom2")

	m2<-glmmTMB(Hammerheads~StudyYear+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME, 
	data=temp, family="nbinom2")

	effect_half_oni[i]<-summary(m1)$coefficients$cond[2,1]
	effect_half_no_oni[i]<-summary(m2)$coefficients$cond[2,1]

	pvalue_half_oni[i]<-summary(m1)$coefficients$cond[2,4]
	pvalue_half_no_oni[i]<-summary(m2)$coefficients$cond[2,4]

}

#three quarters of time series

effect_threequarter_oni<-NULL
effect_threequarter_no_oni<-NULL

pvalue_threequarter_oni<-NULL
pvalue_threequarter_no_oni<-NULL

for (i in 1:1000) {

	sample_dives<-sample(1:nrow(cocos), 0.75*nrow(cocos), replace=FALSE)

	temp<-cocos[sample_dives,]
	m1<-glmmTMB(Hammerheads~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME, 
	data=temp, family="nbinom2")

	m2<-glmmTMB(Hammerheads~StudyYear+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME, 
	data=temp, family="nbinom2")

	effect_threequarter_oni[i]<-summary(m1)$coefficients$cond[2,1]
	effect_threequarter_no_oni[i]<-summary(m2)$coefficients$cond[2,1]

	pvalue_threequarter_oni[i]<-summary(m1)$coefficients$cond[2,4]
	pvalue_threequarter_no_oni[i]<-summary(m2)$coefficients$cond[2,4]


}


#ninety percent of time series

effect_ninety_oni<-NULL
effect_ninety_no_oni<-NULL

pvalue_ninety_oni<-NULL
pvalue_ninety_no_oni<-NULL

for (i in 1:1000) {

	sample_dives<-sample(1:nrow(cocos), 0.9*nrow(cocos), replace=FALSE)

	temp<-cocos[sample_dives,]
	m1<-glmmTMB(Hammerheads~StudyYear+scale(SST)+ONI+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME, 
	data=temp, family="nbinom2")

	m2<-glmmTMB(Hammerheads~StudyYear+CurrentCode+scale(Visibility)+SIN_TIME+COS_TIME, 
	data=temp, family="nbinom2")

	effect_ninety_oni[i]<-summary(m1)$coefficients$cond[2,1]
	effect_ninety_no_oni[i]<-summary(m2)$coefficients$cond[2,1]

	pvalue_ninety_oni[i]<-summary(m1)$coefficients$cond[2,4]
	pvalue_ninety_no_oni[i]<-summary(m2)$coefficients$cond[2,4]

}

effect_size_oni<-data.frame(tenth=effect_tenth_oni, quarter=effect_quarter_oni,
	half=effect_half_oni, threequarter=effect_threequarter_oni, ninety=effect_ninety_oni)

mean(effect_tenth_oni)
mean(effect_quarter_oni)
mean(effect_half_oni)
mean(effect_threequarter_oni)

mean(effect_tenth_no_oni)
mean(effect_quarter_no_oni)
mean(effect_half_no_oni)
mean(effect_threequarter_no_oni)


effect_size_no_oni<-data.frame(tenth=effect_tenth_no_oni, quarter=effect_quarter_no_oni,
	half=effect_half_no_oni, threequarter=effect_threequarter_no_oni, ninety=effect_ninety_no_oni)

pvalue_size_oni<-data.frame(tenth=pvalue_tenth_oni, quarter=pvalue_quarter_oni,
	half=pvalue_half_oni, threequarter=pvalue_threequarter_oni, ninety=pvalue_ninety_oni)

pvalue_size_no_oni<-data.frame(tenth=pvalue_tenth_no_oni, quarter=pvalue_quarter_no_oni,
	half=pvalue_half_no_oni, threequarter=pvalue_threequarter_no_oni, ninety=pvalue_ninety_no_oni)

save(effect_size_oni, file="Models/effect_size_oni.Rdata")
save(effect_size_no_oni, file="Models/effect_size_no_oni.Rdata")

save(pvalue_size_oni, file="Models/pvalue_size_oni.Rdata")
save(pvalue_size_no_oni, file="Models/pvalue_size_no_oni.Rdata")

load("Models/effect_size_oni.Rdata")
load("Models/effect_size_no_oni.Rdata")

load("Models/pvalue_size_oni.Rdata")
load("Models/pvalue_size_no_oni.Rdata")

load("Models/five_years.Rdata")
load("Models/ten_years.Rdata")
load("Models/fifteen_years.Rdata")
load("Models/twenty_years.Rdata")

#######################################
#####Figure
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

par(mar=c(5.5,5.5,2,2))

plot(df_five$effect_size_oni~five_years, type="l", ylim=c(-0.6,1.2), 
	las=2, , ylab="", xlab="", cex.lab=1.5, cex.axis=1.5)
points(df_five$effect_size_no_oni~five_years, type="l", col="red")
abline(h=-0.024648, lty=2)
abline(h=-0.047225, lty=2, col="red")
mtext("(a)", side=3, adj=0, cex=1.5, line=0.3)
mtext("Year coefficient", side=2, adj=0.5, line=4,cex=1.5)

plot(df_ten$effect_size_oni~ten_years, type="l", ylim=c(-0.25,0.05), las=2,
     ylab="", xlab="", cex.lab=1.5, cex.axis=1.5)
points(df_ten$effect_size_no_oni~ten_years, type="l", col="red")
abline(h=-0.024648, lty=2)
abline(h=-0.047225, lty=2, col="red")
mtext("(b)", side=3, adj=0, cex=1.5, line=0.3)

plot(df_fifteen$effect_size_oni~fifteen_years, type="l", ylim=c(-0.12,0), las=2, 
     ylab="", xlab="", cex.axis=1.5)
points(df_fifteen$effect_size_no_oni~fifteen_years, type="l", col="red")
abline(h=-0.024648, lty=2)
abline(h=-0.047225, lty=2, col="red")
mtext("(c)", side=3, adj=0, cex=1.5, line=0.3)
mtext("Starting year", side=1, adj=0.5, line=4,cex=1.5)
mtext("Year coefficient", side=2, adj=0.5, line=4,cex=1.5)

plot(df_twenty$effect_size_oni~twenty_years, type="l", ylim=c(-0.12,-0.02), las=2, 
     ylab="", xlab="", cex.lab=1.5, cex.axis=1.5)
points(df_twenty$effect_size_no_oni~twenty_years, type="l", col="red")
abline(h=-0.024648, lty=2)
abline(h=-0.047225, lty=2, col="red")
mtext("(d)", side=3, adj=0, cex=1.5, line=0.3)
mtext("Starting year", side=1, adj=0.5, line=4,cex=1.5)

