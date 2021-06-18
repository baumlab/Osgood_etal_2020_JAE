#This code applies a series of filters to get the data set for the final Cocos Island analyses, updating with 2014 

#Created by: Geoffrey J Osgood
#Last edited: 19-Sept-2019


rm(list=ls(all=T))

setwd("c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/Data")
setwd("~/Documents/git_repos/Cocos_El_Nino/Data")

library(chron)


#Make date column
Cocos2014<-read.csv("Compiled_Cocos_for_2014_fixed_Nov22_2016.csv", header=T, sep=",")
Cocos<-read.csv("Cleaned_Cocos_Malpelo_Data.csv", header=T, sep=",")

#$remove 1992 and 2014
cocos=subset(Cocos,Year>1992)
cocos=subset(cocos,Year<2014)

#remove divermasters who made less than 100 dives, and misc, unknown divemasters
cocos=subset(cocos,cocos$DiverCode<38)
cocos=subset(cocos,cocos$DiverCode!=36)

#remove miscellaneous dive sites
cocos=subset(cocos,SiteCode!=104)
cocos=subset(cocos,SiteCode!=119)

#########
#remove high points in currentcode, change NAs to 0
cocos$CurrentCode[cocos$CurrentCode>3]=3
cocos$CurrentCode[is.na(cocos$CurrentCode)]=0

#convert measurements to metric system
cocos$SeaTempF=(cocos$SeaTempF - 32)*(5/9)
cocos$SeaTempF=round(cocos$SeaTempF)
cocos$VisibilityFt=cocos$VisibilityFt*0.3048
cocos$VisibilityFt=round(cocos$VisibilityFt)
names(cocos)[19:20]=c('SeaTempCelsius','VisibilityMeters')

##Add seasonlity terms
cocos$SIN_TIME=sin(2*pi*cocos$StudyJulianDate/365.25)
cocos$COS_TIME=cos(2*pi*cocos$StudyJulianDate/365.25)

#Fix date format

cocos$Date<-as.Date(cocos$Date, format="%m/%d/%Y")

#remove unneccesary columns
cocos_select=cocos[,c(2:12,17,19:23,25,26,28:31,34:39, 68:69)]

#Combine dolphins into single column:
cocos_select$Dolphins<-cocos[,44]

DateTemp<-paste(Cocos2014$Year, Cocos2014$Month, Cocos2014$Day, sep="-")
Cocos2014$Date<-as.Date(DateTemp)
Cocos2014$Month<-as.numeric(Cocos2014$Month)
Cocos2014$JulianDate<-julian(y=Cocos2014$Year, x=Cocos2014$Month, d=Cocos2014$Day, origin=c(month=1, day=1, year=1900))+2415021
Cocos2014$StudyJulianDate<-Cocos2014$JulianDate-2448843

Cocos2014$SIN_TIME=sin(2*pi*Cocos2014$StudyJulianDate/365.25)
Cocos2014$COS_TIME=cos(2*pi*Cocos2014$StudyJulianDate/365.25)
Cocos2014$SeaTempCelsius<-(Cocos2014$SeaTemp-32)*(5/9)
Cocos2014$SeaTempCelsius<-round(Cocos2014$SeaTempCelsius)
Cocos2014$VisibilityMeters<-Cocos2014$Visibility*0.3048
Cocos2014$CurrentCode[Cocos2014$CurrentCode>3]=3
Cocos2014$CurrentCode[is.na(Cocos2014$CurrentCode)]=0
Cocos2014$StudyYear<- rep(23, nrow(Cocos2014)) 
Cocos2014$StudyMonth<-Cocos2014$Month+252
Cocos2014$IslandCode<-rep(1, nrow(Cocos2014)) 

Cocos2014$Silky[is.na(Cocos2014$Silky)]=0
Cocos2014$Blacktip[is.na(Cocos2014$Blacktip)]=0
Cocos2014$Silvertip[is.na(Cocos2014$Silvertip)]=0

Cocos2014<-Cocos2014[,-which(names(Cocos2014)%in%c("SeaTemp", "Dive_time", "SeaCond", "Visibility", "Weather",
	"CurrentDirect", "OtherSharks", "jacks", "SchoolMate", "Thermocline",
	 "Rating", "Comments", "X"))]

#remove divermasters who made less than 100 dives, and misc, unknown divemasters
Cocos2014=subset(Cocos2014,Cocos2014$DiverCode<38)
Cocos2014=subset(Cocos2014,Cocos2014$DiverCode!=36)

#remove miscellaneous dive sites
Cocos2014=subset(Cocos2014,Cocos2014$SiteCode<119)
Cocos2014=subset(Cocos2014,Cocos2014$SiteCode!=104)
Cocos2014<-Cocos2014[, c(names(cocos_select))]

Cocos_Updated_2014<-rbind(Cocos2014, cocos_select)
Cocos_Updated_2014=na.omit(Cocos_Updated_2014)

Cocos_Updated_2014$SiteCode <- as.factor(Cocos_Updated_2014$SiteCode)
Cocos_Updated_2014$CurrentCode <- as.factor(Cocos_Updated_2014$CurrentCode)
Cocos_Updated_2014$DiverCode <- as.factor(Cocos_Updated_2014$DiverCode)

#Updating El Nino Index

ElNino<-read.table("ElNinoIndexUpdated2014.txt", sep="\t", header=TRUE)

for (i in 1993:2014) {

	for (j in 1:12) {

		Cocos_Updated_2014[which(Cocos_Updated_2014$Year==i & Cocos_Updated_2014$Month==j), "ElNinoIndex"]<-subset(ElNino, YEAR==i)[,(j+1)]

}}

save(Cocos_Updated_2014, file="Cocos_Updated2014_Final.Rdata")
