#!/bin/env Rscript


#This code applies a series of filters to get the data set for the final Cocos Island analyses 

#Created by: Easton R. White
#Last edited: 4-Jun-2014 



#call in data and run a number of filters
main=read.csv('~/Documents/git_repos/Cocos_El_Nino/Data/Cleaned_Cocos_Malpelo_Data.csv',header=T,sep=',')

##################


cocos_Easton=subset(main,main$IslandCode==1)
malpelo=subset(main,main$IslandCode==2)



#$remove 1992 and 2014
cocos_Easton=subset(cocos_Easton,cocos_Easton$Year>1992)
cocos_Easton=subset(cocos_Easton,cocos_Easton$Year<2014)

nrow(cocos_Easton) #total number of dives between 1993 and 2013



#remove divermasters who made less than 100 dives, and misc, unknown divemasters
cocos_Easton=subset(cocos_Easton,cocos_Easton$DiverCode<38)
cocos_Easton=subset(cocos_Easton,cocos_Easton$DiverCode!=36)

nrow(cocos_Easton)


#remove miscellaneous dive sites
cocos_Easton=subset(cocos_Easton,cocos_Easton$SiteCode<119)
cocos_Easton=subset(cocos_Easton,cocos_Easton$SiteCode!=104)

nrow(cocos_Easton)




#############################################


#########
#remove high points in currentcode, change NAs to 0
cocos_Easton$CurrentCode[cocos_Easton$CurrentCode>3]=3
cocos_Easton$CurrentCode[is.na(cocos_Easton$CurrentCode)]=0

#convert measurements to metric system
cocos_Easton$SeaTempF=(cocos_Easton$SeaTempF - 32)*(5/9)
cocos_Easton$SeaTempF=round(cocos_Easton$SeaTempF)
cocos_Easton$VisibilityFt=cocos_Easton$VisibilityFt*0.3048
cocos_Easton$VisibilityFt=round(cocos_Easton$VisibilityFt)
names(cocos_Easton)[19:20]=c('SeaTempCelsius','VisibilityMeters')


#set factor variables to factors
cocos_Easton$SiteCode <- as.factor(cocos_Easton$SiteCode)
cocos_Easton$CurrentCode <- as.factor(cocos_Easton$CurrentCode)
cocos_Easton$DiverCode <- as.factor(cocos_Easton$DiverCode)


#define seasonality terms
cocos_Easton$SIN_TIME=sin(2*pi*cocos_Easton$StudyJulianDate/365.25)
cocos_Easton$COS_TIME=cos(2*pi*cocos_Easton$StudyJulianDate/365.25)

###################################################
#Convert "other species to presence-absence
cocos_Easton$Silky[cocos_Easton$Silky!=0]=1
cocos_Easton$Silvertip[cocos_Easton$Silvertip!=0]=1
cocos_Easton$Blacktip[cocos_Easton$Blacktip!=0]=1
cocos_Easton$Galapagos[cocos_Easton$Galapagos!=0]=1
cocos_Easton$Whale.Shark[cocos_Easton$Whale.Shark!=0]=1
cocos_Easton$Tiger[cocos_Easton$Tiger!=0]=1
names(cocos_Easton)
#remove unneccesary rows (not including marble rays)
marblerays=cocos_Easton[,c(2:12,17,19:23,27)]
cocos_Easton=cocos_Easton[,c(2:12,17,19:23,25,26,28:30,34:39)]

nrow(cocos_Easton)

#cocos_Easton=na.omit(cocos_Easton)
nrow(cocos_Easton)

marblerays=na.omit(marblerays)
marblerays=subset(marblerays,marblerays$Year<2013)
nrow(marblerays)


####
#Create Final Data sets
#write.table(cocos_Easton,'FINAL_Cocos_Dataset_11species.txt',quote=F,sep=',',row.names=F)
#write.table(marblerays,'FINAL_Cocos_Dataset_marblerays.txt',quote=F,sep=',',row.names=F)


#dude=cocos[,c(23:28)]
#dude[dude>0]=1
#trial=rowSums(dude)
#trial[trial>0]=1







