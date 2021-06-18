#This script is for running GLMs on hammerhead abundance at Cocos 
#created by Geoffrey J. Osgood
#

library(here)
library(forecast)
library(ggplot2)
library(mgcv)
library(glmmTMB)
library(glmmADMB)

setwd("c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/")
setwd("~/Documents/git_repos/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019

malpelo<-read.csv("Data/Malpelo.csv", header=TRUE, stringsAsFactors = FALSE) 

cocos<-subset(cocos, Hammerheads>=0) #remove -1's which indicate presence only
malpelo<-subset(malpelo, Hammerheads>=0) #remove -1's which indicate presence only

#fix visibility
cocos$Visibility[cocos$Visibility==442]<-43
cocos$Visibility[cocos$Visibility==1256]<-37

#Average counts done on same dive


malpelo_avg<-aggregate(Hammerheads~Year, malpelo, mean)[-12,]
cocos_avg<-aggregate(Hammerheads~Year, cocos, mean)

plot(Hammerheads~Year, data=cocos_avg, type="l", xaxt="n")
points(Hammerheads~Year, data=malpelo_avg, type="l", xaxt="n", col="blue")
axis(1, at=1993:2019, labels=1993:2019, las=2)

