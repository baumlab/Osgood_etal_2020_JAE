#Created by: Geoffrey J. Osgood
#Last edited: 21-Nov-2019

library(dplyr)
library(timeDate)

#######Add the pre-2014 data:
setwd("~/Documents/git_repos/Cocos_El_Nino/")
setwd("c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/")

pre2014<-read.csv(file="Data/Cleaned_Cocos_Malpelo_Data.csv", stringsAsFactors=FALSE)
post2014<-read.csv(file="Data/Cocos_Aug2019_CleanedJan2020.csv", stringsAsFactors=FALSE)

SiteID_index<-distinct(pre2014[,c("Site", "SiteCode")]) #Get an index identifying which Site has which site code
DiverID_index<-distinct(pre2014[,c("DiverName", "DiverCode")]) #Get an index identifying which Dive guide has which diver code

#Change site names to site codes in the SiteCode column in the post2014 data

post2014$SiteCode <- SiteID_index$SiteCode[match(post2014$Site, SiteID_index$Site)]
post2014$DiverCode <- DiverID_index$DiverCode[match(post2014$DiverName, DiverID_index$DiverName)]

#Give code of 200 for general Malpelo sites
post2014$SiteCode[post2014$Site=="Malpelo"]<-200

#give miscellaneous dive sites a code of 119
post2014$SiteCode[is.na(post2014$Site)]<-119

#Giving new divers with over 100 dives a new code
post2014$DiverCode[post2014$DiverName=="Arik"]<-42
post2014$DiverCode[post2014$DiverName=="Fernando"]<-43
post2014$DiverCode[post2014$DiverName=="Geovanny"]<-44
post2014$DiverCode[post2014$DiverName=="Hector"]<-45
post2014$DiverCode[post2014$DiverName=="Litus"]<-46
post2014$DiverCode[post2014$DiverName=="Merlin"]<-47
post2014$DiverCode[post2014$DiverName=="Roy"]<-48
pre2014$DiverCode[pre2014$DiverName=="Roy"]<-48 # Roy got more dives in new data, so give him his own code (not the generic one used for mischellaneous divers)
post2014$DiverCode[post2014$DiverName=="Sergio"]<-49
post2014$DiverCode[post2014$DiverName=="Stefan"]<-50
post2014$DiverCode[is.na(post2014$DiverCode)] <- 41 #miscellaneous divers codes not in pre2014 data - label as DiverCode = 41 to match with Easton's previous filtering

#Remove two cases where date not recorded
post2014<-subset(post2014, !is.na(Date))
pre2014<-subset(pre2014, !is.na(Date))
pre2014<-subset(pre2014, Date!="")

#Remove 1992 and 2014
pre2014<-subset(pre2014,Year>1992 & Year<2014)

#Add day, month, and year columns to new post 2014 data:
post2014$Date<-as.Date(post2014$Date)
post2014$Day<-as.numeric(format(post2014$Date, format="%d"))
post2014$Month<-as.numeric(format(post2014$Date, format="%m"))
post2014$Year<-as.numeric(format(post2014$Date, format="%Y"))
post2014$StudyYear<-post2014$Year-min(pre2014$Year)+1 #number of years since study start

#Make sure old data in right date format:
pre2014$Date<-as.Date(pre2014$Date, format="%m/%d/%Y")

#Add Julian Date and make sure consistent across both data sets
post2014$JulianDate<-as.numeric(julian(timeDate(post2014$Date), origin = timeDate("1970-01-01")))
pre2014$JulianDate<-as.numeric(julian(timeDate(pre2014$Date), origin = timeDate("1970-01-01")))

post2014$StudyJulianDate<-(as.numeric(post2014$JulianDate)-min(pre2014$JulianDate)+1) #number of days since first data point
pre2014$StudyJulianDate<-(pre2014$JulianDate-min(pre2014$JulianDate)+1) #number of days since first data point

#Make sure study year is correct in old data
pre2014$StudyYear<-pre2014$Year-min(pre2014$Year)+1 #number of years since study start


#Fix row names
rownames(pre2014)<-1:nrow(pre2014)

#Add Island codes to new data 
post2014$IslandCode<-1
post2014$IslandCode[post2014$Site=="Malpelo"]<-2

#Turn time into number (1,2,3,4) to match with TimeCode column of old data set
post2014$TimeCode<-as.numeric(factor(post2014$Time, levels=c("Morning", "Midday", 
	"Afternoon", "Night")))

#Fix missing time codes for pre2014 data

pre2014[c(9259,15270,15359), "TimeCode"] <- 3

pre2014[c(9430),"TimeCode"] <- 1
head(post2014)
#remove columns from pre2014 that I no longer need:
pre2014<-pre2014[,-c(9, 15, 16, 23, 24, 32, 33, 40:67)]

#remove columns from post2014 that I no longer need:
post2014<-post2014[, -c(1,4,12,26)]

#Add boat column to pre2014 (but fill with NAs as I can't tell from the data which boat for each row easily)
#This is just for checking multiple divers at same site at same time, which is mainly a problem for post2014 data
#Only 20 dates had possible issues in pre2014 - will look at manually
pre2014$Boat<-rep(NA, nrow(pre2014))


#Rename columns that do not match in both data sets
names(post2014)[names(post2014)=="Condition"] <- "SeaCondCode"
names(post2014)[names(post2014)=="Current_stength"] <- "CurrentCode"
names(pre2014)[names(pre2014)=="Hammerhead"] <- "Hammerheads"
names(pre2014)[names(pre2014)=="Tiger"] <- "TigerSharks"
names(pre2014)[names(pre2014)=="Silvertip"] <- "Silvertips"
names(pre2014)[names(pre2014)=="Blacktip"] <- "Blacktips"
names(pre2014)[names(pre2014)=="Whale.Shark"] <- "WhaleSharks"
names(pre2014)[names(pre2014)=="MarbleRays"] <- "MarbledRays"

#Convert visibility and temperature to metric in pre2014 data
pre2014$SeaTempF=(pre2014$SeaTempF - 32)*(5/9)
pre2014$SeaTempF=round(pre2014$SeaTempF)
pre2014$VisibilityFt=pre2014$VisibilityFt*0.3048
pre2014$VisibilityFt=round(pre2014$VisibilityFt)
names(pre2014)[names(pre2014)=="SeaTempF"]<-'Temperature'
names(pre2014)[names(pre2014)=="VisibilityFt"]<-'Visibility'

#Add ChronoSort column (sorting data by date)
post2014_ordered<-post2014[order(as.Date(post2014$Date)),]
post2014_ordered$ChronoSort<-1:nrow(post2014_ordered) #need to replace starting from beginning of data once all joined below

#Put all the data together
cocos<-rbind(pre2014, post2014_ordered)

#########
#Remove sea condition column
cocos<-cocos[,-which(names(cocos)=="SeaCondCode")]


#remove high points in currentcode, change NAs to 0
cocos$CurrentCode[cocos$CurrentCode>5]=5
cocos$CurrentCode[is.na(cocos$CurrentCode)]=0

##Add seasonlity terms
cocos$SIN_TIME=sin(2*pi*cocos$StudyJulianDate/365.25)
cocos$COS_TIME=cos(2*pi*cocos$StudyJulianDate/365.25)

#remove divermasters who made less than 100 dives, and misc, unknown divemasters
cocos=subset(cocos,cocos$DiverCode!=36)
cocos=subset(cocos,cocos$DiverCode!=39)
cocos=subset(cocos,cocos$DiverCode!=41)
cocos=subset(cocos,cocos$DiverCode!=40)

trial<-subset(cocos, Year==1993)
trial2<-subset(cocos, Year==1993)

#Add Cocos study month
cocos$StudyMonth<-cocos$Month+12*(cocos$StudyYear-1)

#Reset Cocos Study Julian Day
cocos$StudyJulianDate<-cocos$StudyJulianDate-min(cocos$StudyJulianDate)+1


#save file for Malpelo analyses
malpelo<-subset(cocos, IslandCode==2)
malpelo<-malpelo[order(as.Date(malpelo$Date)),]
malpelo$ChronoSort<-1:nrow(malpelo)

#remove miscellaneous dive sites 
cocos=subset(cocos,cocos$SiteCode<119)
cocos=subset(cocos,cocos$SiteCode!=104)

#Update ChronoSort column (sorting data by date)
cocos<-cocos[order(as.Date(cocos$Date)),]
cocos$ChronoSort<-1:nrow(cocos)

#Replace "M" in Silky column with 0
cocos$Silky[cocos$Silky=="M"]<-0

#create dive ID to indicate unique dives (ie multiple observations from multiple dive guides on the same dive)
cocos$ID<- as.numeric(factor(paste(cocos$Date, cocos$Time, cocos$Site, cocos$Boat)))
cocos$ID[is.na(cocos$Time)] <- paste("NA_", cocos$ChronoSort[is.na(cocos$Time)], sep="")

#Remove duplicate rows on same dive

#Remove all duplicated rows regardless of how many divers are on the dive
cocos<-cocos[!duplicated(cocos[,c(19:31,36)]),]

#determine which dives had more than 2 divers recording data
id_table<-data.frame(table(cocos$ID))
id_multi<-subset(id_table, Freq>1)
multi_prob<-subset(cocos, ID%in%unique(id_multi$Var1)) #identify dives at same site and time with more than 1 diver

#re-establish Chronsort to ensure it works
cocos$ChronoSort<-1:nrow(cocos)

#divide based on pre and post 2014 due to lack of boat information before 2014
old<-subset(cocos, Year<2014 & Date%in%unique(multi_prob$Date)) 
new<-subset(cocos, Year>=2014 & Date%in%unique(multi_prob$Date))

new$DateTime<-paste(new$Date, new$Time, new$Boat)
old$DateTime<-paste(old$Date, old$Time, old$Boat)

num_div<-function(x) {length(unique(x))} #function to identify how many divers on a boat on a date (if only 2, then it is safe to assume each in their own group)

#use the above function to identify dates with too many divers for just two dive groups per boat
ag_length_new<-aggregate(new$DiverCode, by=list(new$DateTime), num_div)
ag_length_old<-aggregate(old$DiverCode, by=list(old$DateTime), num_div)

too_many_new<-subset(ag_length_new, x>2) #identify dates with too many divers for just two groups
too_many_old<-subset(ag_length_old, x>2) #identify dates with too many divers for just two groups

cocos$DateTime<-paste(cocos$Date, cocos$Time, cocos$Boat)

#subset cocos to find dives on dates with too many divers to assume just two groups
cocos_new<-subset(cocos, ID%in%unique(as.character(id_multi$Var1)) & DateTime%in%unique(too_many_new$Group.1))
cocos_old<-subset(cocos, ID%in%unique(as.character(id_multi$Var1)) & DateTime%in%unique(too_many_old$Group.1))

same_diver<-data.frame(table(multi_prob$ID, multi_prob$DiverCode))

#for loop to determine which dives are done by the INEXPERIENCED divers (to remove them later)

chosen_dives_new<-NULL
for (i in 1:length(unique(cocos_new$ID))) {

	divers<-subset(cocos, ID==unique(cocos_new$ID)[i])$DiverCode
	trial<-subset(cocos, DiverCode%in%divers)

	df.trial<-data.frame(table(trial$DiverCode))

	chosen_dives_new[i]<-subset(cocos_new, 
		DiverCode==as.character(df.trial[which.max(df.trial$Freq),1]) & ID==unique(cocos_new$ID)[i])$ChronoSort

}

not_chosen_dives_new<-subset(cocos_new, !ChronoSort%in%chosen_dives_new)$ChronoSort # determine which dives are from inexperienced divers

chosen_dives_old<-NULL
for (i in 1:length(unique(cocos_old$ID))) {

	divers<-subset(cocos, ID==unique(cocos_old$ID)[i])$DiverCode
	trial<-subset(cocos, DiverCode%in%divers)

	df.trial<-data.frame(table(trial$DiverCode))

	chosen_dives_old[i]<-subset(cocos_old, 
		DiverCode==as.character(df.trial[which.max(df.trial$Freq),1]) & ID==unique(cocos_old$ID)[i])$ChronoSort[1]

}

not_chosen_dives_old<-subset(cocos_old, !ChronoSort%in%chosen_dives_old)$ChronoSort # determine which dives are from inexperienced divers

cocos_fixed<-subset(cocos, !ChronoSort%in%c(not_chosen_dives_old, not_chosen_dives_new)) #remove dives from inexperienced divers


write.csv(cocos_fixed,"~/Documents/git_repos/Cocos_El_Nino/Data/Cocos_Updated.csv")
write.csv(malpelo,"~/Documents/git_repos/Cocos_El_Nino/Data/Malpelo.csv")

write.csv(cocos_fixed,"c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/Data/Cocos_Updated.csv")
write.csv(malpelo,"c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/Data/Malpelo.csv")






