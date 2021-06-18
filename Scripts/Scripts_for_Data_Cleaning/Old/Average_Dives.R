#This code is for averaging counts taken on the same dive by different dive guides and assigning the number to the most experienced diver

#Created by Geoffrey J Osgood
#Updated on January 7, 2020
setwd("c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE)

names(cocos)




#pick the dive guide with the most dives
ag_diveguides<-function (x) {

	trial2<-subset(cocos, DiverCode%in%x)

	df.trial<-data.frame(table(trial2$DiverCode))

	return(as.character(df.trial[which.max(df.trial$Freq),1]))

	}

#function for current code

ag_diveguides<-function (x) {

	trial2<-subset(cocos, DiverCode%in%x)

	df.trial<-data.frame(table(trial2$DiverCode))

	as.character(df.trial[which.max(df.trial$Freq),1])

	}


#make a function to give the mean or NA per dive
mean_func<-function(x) {

	if(sum(x!=(-1))==0) {-1} else {

	if(sum(!is.na(x))>0) 

	{round(mean(x[x>=0], na.rm=TRUE),0)} else {mean(x, na.rm=FALSE)} 
}

}


Current_ag<-aggregate(as.numeric(as.character(cocos$CurrentCode))~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear,
 mean_func, data=cocos)

Temp_ag<-aggregate(SST~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

Anomaly_ag<-aggregate(Anomaly~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

ONI_ag<-aggregate(ONI~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

dive_ag<-aggregate(DiverCode~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth, ag_diveguides, data=cocos)

Hamm_ag<-aggregate(Hammerheads~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)


trial<-cocos[!cocos$ID%in%Hamm_ag$ID,]

aggregate(Hammerheads~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 is.na, data=trial)


EagleRays_ag<-aggregate(EagleRays~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

Whitetips_ag<-aggregate(Whitetips~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

MarbledRays_ag<-aggregate(MarbledRays~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

nrow(MarbledRays_ag)

MantaRays_ag<-aggregate(MantaRays~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

MobulaRays_ag<-aggregate(MobulaRays~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

Silky_ag<-aggregate(Silky~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

Silvertips_ag<-aggregate(Silvertips~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

Blacktips_ag<-aggregate(Blacktips~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

Galapagos_ag<-aggregate(Galapagos~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

WhaleSharks_ag<-aggregate(WhaleSharks~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

TigerSharks_ag<-aggregate(TigerSharks~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

sineag<-aggregate(SIN_TIME~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

cosag<-aggregate(COS_TIME~ID+Date+SiteCode+Month+Day+Year+JulianDate+StudyYear+StudyMonth,
 mean_func, data=cocos)

cocos_analysis<-data.frame(Date=Hamm_ag$Date, Month=Hamm_ag$Month, Day=Hamm_ag$Day, Year=
	Hamm_ag$Year, JulianDate=Hamm_ag$JulianDate, StudyYear=Hamm_ag$StudyYear,
	StudyMonth=Hamm_ag$StudyMonth,
	CurrentCode=Current_ag$"as.numeric(as.character(cocos$CurrentCode))",
	Temperature=Temp_ag$SST, Anomaly=Anomaly_ag$Anomaly, ONI=ONI_ag$ONI, DiverCode=dive_ag$DiverCode,
	Hammerheads=Hamm_ag$Hammerheads, Whitetips=Whitetips_ag$Whitetips, 
	EagleRays=EagleRays_ag$EagleRays, MarbledRays=MarbledRays_ag$MarbledRays, 
	MantaRays=MantaRays_ag$MantaRays, MobulaRays=MobulaRays_ag$MobulaRays,
	Silvertips=Silvertips_ag$Silvertips, Silky=Silky_ag$Sikly, Blacktips=Blacktips_ag$Blacktips,
	Galapagos=Galapagos_ag$Galapagos, WhaleSharks=WhaleSharks_ag$WhaleSharks, 
	TigerSharks=TigerSharks_ag$TigerSharks, SIN_TIME=sineag$SIN_TIME, COS_TIME=cosag$COS_TIME)