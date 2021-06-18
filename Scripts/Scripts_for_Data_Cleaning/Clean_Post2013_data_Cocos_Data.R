#Created by: Geoffrey J. Osgood
#Last edited: 21-Nov-2019

#Notes on data cleaning prior to running this code: original spreadsheets received from Undersea Hunter were checked by volunteers and double-checked by me 
#for sharks/rays mentioned in the comments but not put into columns in order to put those counts in the columns.
# -1 in a cell indicates presence only could be determined from the comments.
# -2 in a cell indicates that cells should be treated as NA (NOT zero) for a species (ie data for that cell was not collected properly)
# When diver guide names were absent, when possible, the name was inferred from the writing/recording patterns on the spreadsheet
# and those divers working for the company at the time. Only inferred when unambiguous.
# Time cells that were empty were filled with "Morning", "Midday", "Afternoon", or "Night" which such was obvious. 
#These spreadsheets are in the "Cleaned" folder. Originals are in the "Original" folder in the Data folder.


setwd("~/Documents/git_repos/Cocos_El_Nino/Data/Updated_Data_2019/Cleaned/") #set working directory
setwd("c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/Data/Updated_Data_2019/Cleaned/") #set working directory

rm(list=ls()) #remove all files when re-running

library(readxl)
library(zoo)
library(stringr)

col_names<-NULL

#function for converting to Celsius:
Fahr_to_Cel_Func<-function(x) {
if (x<50 | is.na(x)){
return(round(x,1))
} else {return(round(((x-32)*(5/9)),1))}

}


files<-list.files(pattern=".xlsx") #list all spreadsheet with the data to read in 

################################################################
#for loop to go through each spreadsheet and each tab within each to find, clean, and collate data
################################################################

for (i in files) {

	for (j in 2:length(excel_sheets(i))) {

trial<-read_excel(i, 
	sheet = j, col_names = TRUE, skip = 0) #read in data

skip_rows<-which(data.frame(trial)[,1]=="Date") #determine which row the data starts on to skip when I read in the data again:

Guide<-trial[1,2][[1]]
Director_or_guide<-trial[1,1][[1]]
Director_or_guide[Director_or_guide%in%c("Cruise Director:", "cruise director:", 
	"Cruise Director :")]<-"Director"
Director_or_guide[Director_or_guide%in%c("Dive Guide:", "DIVE GUIDE:", "DM", 
"Dive guide 1", "Dive Guide 2:", "Dive Guide 3:", "Dive master 2", "Dive Master",
"Dive Master:", "Dive Guide", "Divemaster:", "dive master", "DIVEMASTER:", "Divemaster")]<-"Guide"

Boat<-names(trial)[2]

temp<-read_excel(i, 
	sheet = j, col_names = TRUE, skip = skip_rows) #read in data, skipping past the extra rows at the top of each sheet

temp<-data.frame(temp)

if(is.null(col_names)) { #collect column names for checking for mistakes

col_names<-colnames(temp) } else {

	col_names<-c(col_names, colnames(temp))}


if(is.na(sum(as.numeric(temp$Date)))) {
data<-temp[-c(which(is.na(temp$Date))[1]:nrow(temp)),] #get only rows that have data
} else{data<-temp}

#collecting the environmental columns
if(is.na(sum(as.numeric(temp$Date)))) {
Date<-as.Date(as.numeric(data$Date), origin="1899-12-30") # fix date format
} else{Date<-temp$Date}

Site<-as.character(data$"Location.") 

#get dive time and categorize into morning, midday, or afternoon
Dive_time<-as.character(data$"Dive.time")

Dive_time[Dive_time%in%c("0.375", "9;00", "09;00","09*;00","9*:00","1899-12-31 09:00:00")] <-"9:00" 
Dive_time[Dive_time%in%c("8;00", "0.33333333333333331", "08'00", "1899-12-31 08:00:00",
	"8", "08 00", "08;00-","08;00", '8"00','08"00')]<-"8:00"
Dive_time[Dive_time%in%c("08;30","0.35416666666666669","8>30","8:30am","8;30",
	"1899-12-31 08:30:00")] <-"8:30"
Dive_time[Dive_time%in%c("0.34375")] <- "8:15"
Dive_time[Dive_time%in%c("0.34722222222222227")] <- "8:20"
Dive_time[Dive_time%in%c("0.34027777777777773")] <- "8:10"
Dive_time[Dive_time%in%c("08;40", "0.3611111111111111","0.36458333333333331")] <-"8:40"
Dive_time[Dive_time%in%c("0.3125")]<-"7:30"
Dive_time[Dive_time%in%c("0.29166666666666669", "7;00",  "1899-12-31 07:00:00",
	"07;00")] <-"7:00"
Dive_time[Dive_time%in%c("0.32291666666666669","07;45")] <-"7:45"
Dive_time[Dive_time%in%c("0.2986111111111111")] <-"7:10"
Dive_time[Dive_time%in%c("0.30208333333333331")] <-"7:15"
Dive_time[Dive_time%in%c("0.27083333333333331")] <-"6:30"
Dive_time[Dive_time%in%c("0.40625", "0.40972222222222227")] <-"9:45"
Dive_time[Dive_time%in%c("0.39583333333333331","09;30", "1899-12-31 09:30:00")] <-"9:30"
Dive_time[Dive_time%in%c("09;15","0.38541666666666669")] <-"9:15"


Dive_time[Dive_time%in%c("10;00","1899-12-31 10:00:00","0.41666666666666669","0.41666666666666702")]<-"10:00"
Dive_time[Dive_time%in%c("0.4236111111111111")]<-"10:10"
Dive_time[Dive_time%in%c("1899-12-31 10:45:00","10;45", "0.44791666666666669")]<-"10:45"
Dive_time[Dive_time%in%c("0.44444444444444442")]<-"10:40"
Dive_time[Dive_time%in%c("10;30","0.4375")]<-"10:30"
Dive_time[Dive_time%in%c("11l;00","1899-12-31 11:00:00","11;00","1100", "0.45833333333333331",
	'11"00',"11 00","0.4604166666666667","0.45833333333333298")]<-"11:00"
Dive_time[Dive_time%in%c("11;10","0.46527777777777773")]<-"11:10"
Dive_time[Dive_time%in%c("11;15","0.46875")]<-"11:15"
Dive_time[Dive_time%in%c('11"30',"1899-12-31 11:30:00","11:30;","11;30","0.47916666666666669")]<-"11:30"
Dive_time[Dive_time%in%c("1899-12-31 11:40:00","0.4861111111111111")]<-"11:40"
Dive_time[Dive_time%in%c("0.47222222222222227")]<-"11:20"
Dive_time[Dive_time%in%c("1899-12-31 11:45:00","0.49305555555555558","0.48958333333333331")]<-"11:45"
Dive_time[Dive_time%in%c("1899-12-31 12:00:00","12;00","0.5", "12:'00")]<-"12:00"
Dive_time[Dive_time%in%c("0.52083333333333337","12;30")]<-"12:30"
Dive_time[Dive_time%in%c("0.53125")]<-"12:45"
Dive_time[Dive_time%in%c("1899-12-31 13:00:00","01;00","0.54166666666666663",
	"4.1666666666666664E-2","0.54166666666666696")]<-"13:00"
Dive_time[Dive_time%in%c("6.25E-2","0.5625")]<-"13:30"
Dive_time[Dive_time%in%c("01;45","0.57291666666666663")]<-"13:45"


Dive_time[Dive_time%in%c("2;00", "14;00","0.58333333333333337","1899-12-31 14:00:00",
	"02;00","8.3333333333333329E-2","0.5854166666666667","0.58333333333333304")]<-"14:00"
Dive_time[Dive_time%in%c("1899-12-31 14:15:00", "0.59375")]<-"14:15"
Dive_time[Dive_time%in%c("2;30","14;30","0.60416666666666663", "0.10416666666666667",
	"1899-12-31 02:30:00","1899-12-31 14:30:00","02;30")]<-"14:30"
Dive_time[Dive_time%in%c("02;45","2;45","0.11458333333333333","0.61458333333333337")]<-"14:45"
Dive_time[Dive_time%in%c("1899-12-31 15:00:00", "15;00", "3;00","0.625","0.125",
	"1899-12-31 03:00:00","0.12708333333333333","300","15 00", "15::00","03;00","3:000")]<-"15:00"
Dive_time[Dive_time%in%c("0.63888888888888895")]<-"15:20"
Dive_time[Dive_time%in%c("03;15","1899-12-31 15:15:00", "0.21875","0.63541666666666663")]<-"15:15"
Dive_time[Dive_time%in%c("15;30","3;30", "0.64583333333333337","0.14583333333333334",
	"0.13541666666666666","1899-12-31 15:30:00", "1899-12-31 03:30:00",
	"330",'3"30',"3:30pm","03;30")]<-"15:30"
Dive_time[Dive_time%in%c("0.65277777777777779","0.15277777777777776","1899-12-31 15:40:00")]<-"15:40"
Dive_time[Dive_time%in%c("0.65625","0.15625")]<-"15:45"
Dive_time[Dive_time%in%c("0.66666666666666663","0.66666666666666696","0.16666666666666666","16:00")]<-"16:00"
Dive_time[Dive_time%in%c("0.6875","0.1875")]<-"16:30"
Dive_time[Dive_time%in%c("0.70833333333333337")]<-"17:00"


Dive_time[Dive_time%in%c("Nigh")]<-"Night"

Dive_time[Dive_time%in%c("6;00", "18;00","0.75","0.25","18:00", "18:00:00 pm",
	"1899-12-31 18:00:00","1899-12-31 06:00:00")]<-"18:00"
Dive_time[Dive_time%in%c("0.26041666666666669")]<-"18:15"
Dive_time[Dive_time%in%c("0.77083333333333337")]<-"18:30"
Dive_time[Dive_time%in%c("0.79166666666666696")]<-"19:00"

#get and clean temperature data and change into Celsius 
Temp<-as.character(data$"Sea.Temp..ºF.")

Temp= str_replace_all(Temp[1:length(Temp)], "[c,Fft']", "") #remove letters from explanatory variables
Temp=as.numeric(as.character(Temp))

Temp_C<-t(data.frame(lapply(Temp, Fahr_to_Cel_Func)))

#get sea condition
Sea_Conditions<-data[, which(names(data)%in%c("Sea.Condition.1.10", "Sea...Condition...1.10"))]

#get and clean visibility and change to meters
Visibility<-as.character(data$"Visibility..ft.")

Visibility= str_replace_all(Visibility[1:length(Visibility)], "[c,FftTd]", "") #remove letters from explanatory variables
Visibility=as.numeric(as.character(Visibility))

Visibility_m<-round(Visibility*0.3048)

#get current strength and direction data
Current<-data$"Current.1.10"
Current_direction<-data$"Current.Direction"

#collecting the shark and ray columns
Hammerheads<-data$Hammerheads
Galapagos<-data$Galapagos
Whitetips<-data$Whitetips
WhaleSharks<-data$"Whale.Shark"
Tiger<-data[, which(names(data)%in%c("Tiger.Shark", "Tiger", "Tiger.shark", "Tiger.Sharks", 
	"Tiger....put.in.comment.behavior...physical.characteristics."))]
EagleRays<-data[, which(names(data)%in%c("Eagle.Rays","Eagle...Rays", "Eagle.rays", "Eagle..Rays"))]
MobulaRays<-data[, which(names(data)%in%c("Mobula.Rays", "Mobula.rays", "Mobula..Rays","Mobula...Rays"))]
MantaRays<-data[, which(names(data)%in%c("Manta.Rays", "Manta.rays", "Manta..Rays", "Manta.Ray"))]
Turtles<-data$Turtles

#Some shark species did not have dedicated columns and so do not appear in any sheet
#Put column of NAs if they were not seen, otherwise include the counts in the column
marbled_temp<-data[, which(names(data)%in%c("Marbled.Rays", "Marbel.Ray", "Marbled..Rays"))]

if (length(marbled_temp)==0) {
	MarbledRays<-rep("-2", length(Hammerheads))} else { #some sheets lack a Marbled Ray column for some reason
		MarbledRays<-marbled_temp}

silver_temp<-data[, which(names(data)%in%c("Silvertips", "Silvertip"))]

if (length(silver_temp)==0) {
	Silvertips<-rep(NA, length(Hammerheads))} else {
		Silvertips<-silver_temp}

black_temp<-data[, which(names(data)%in%c("Blacktip", "Blacktips", "Blacktip.shark"))]

if (length(black_temp)==0) {
	Blacktips<-rep(NA, length(Hammerheads))} else {
		Blacktips<-black_temp}

silky_temp<-data[, which(names(data)%in%c("Silky.shark", "Silky", "Silky.Shark"))]

if (length(silky_temp)==0) {
	Silky<-rep(NA, length(Hammerheads))} else {
		Silky<-silky_temp}

other_temp<-data[, which(names(data)%in%c("Other.Sharks...............put.in.comment.", "Other.Sharks",
	"Other.sharks", "Other.Sharks..............put.in.comment.", "Other.Sharks............put.in.comment."))]

if (length(other_temp)==0) {
	Other<-rep(NA, length(Hammerheads))} else {
		Other<-other_temp}

#Bring all columns together
compiled_data<-data.frame(Boat=rep(Boat, length(Hammerheads)), DiverName=rep(Guide, length(Hammerheads)), 
	Type=rep(Director_or_guide, length(Hammerheads)),
	Date=Date, Site=Site, Time=Dive_time, Temperature=Temp_C[,1], Condition=Sea_Conditions, 
	Visibility=Visibility_m, Current_stength=Current, Current_direction=Current_direction,
	Hammerheads=Hammerheads, Galapagos=Galapagos, Whitetips=Whitetips, Silky=Silky, Blacktips=Blacktips,
	Silvertips=Silvertips, WhaleSharks=WhaleSharks, TigerSharks=Tiger, EagleRays=EagleRays, MobulaRays=MobulaRays,
	MantaRays=MantaRays, MarbledRays=MarbledRays, Turtles=Turtles, Other=Other)

compiled_data[,12:25]<-apply(compiled_data[,12:25], 2, as.character)


compiled_data[,12:25][is.na(compiled_data[,12:25])]<-"0" #change NA's in the species columns to zeros


if(exists("data_for_cleaning")) {data_for_cleaning<-rbind(data_for_cleaning,compiled_data)} else {
	data_for_cleaning<-compiled_data
}

}}

################################################################
################################################################
################################################################

rownames(data_for_cleaning)<-1:nrow(data_for_cleaning)

#Make sure it is all character
data_for_cleaning[]<-lapply(data_for_cleaning[], as.character)

#clean up "pluses" and other characters from data
#Hammerheads
data_for_cleaning$Hammerheads= str_replace_all(data_for_cleaning$Hammerheads[1:length(data_for_cleaning$Hammerheads)], "[+]", "")
data_for_cleaning$Hammerheads[data_for_cleaning$Hammerheads=="100="]<-"100"
data_for_cleaning<-subset(data_for_cleaning, Hammerheads!="X") # no data recorded on dives with X
data_for_cleaning$Hammerheads[data_for_cleaning$Hammerheads%in%c("25,3", "3,2", "53,3")]<-"NA" # weird data we can't understand, get rid of
data_for_cleaning$Hammerheads[data_for_cleaning$Hammerheads=="-2"]<-"NA"
data_for_cleaning$Hammerheads[data_for_cleaning$Hammerheads=="O"]<-"0"
data_for_cleaning$Hammerheads[data_for_cleaning$Hammerheads=="2O"]<-"20"

#Whitetips
data_for_cleaning$Whitetips = str_replace_all(data_for_cleaning$Whitetips[1:length(data_for_cleaning$Whitetips)], "[+]", "")
data_for_cleaning$Whitetips = str_replace_all(data_for_cleaning$Whitetips[1:length(data_for_cleaning$Whitetips)], " ", "")
data_for_cleaning$Whitetips[data_for_cleaning$Whitetips%in%c("10.8666666666667","11.352380952381", 
	"12.8095238095238", "11.8380952380952", "12.3238095238095")] <-"-1"
data_for_cleaning$Whitetips[data_for_cleaning$Whitetips%in%c("6.5","7,8","23,2","23.1","55","17,3","17,2",
	"6,2","5,4","15,7")]<-"NA"
data_for_cleaning$Whitetips[data_for_cleaning$Whitetips=="-2"]<-"NA"
data_for_cleaning$Whitetips[data_for_cleaning$Whitetips=="O"]<-"0"
data_for_cleaning$Whitetips[data_for_cleaning$Whitetips=="`15"]<-"15"
data_for_cleaning$Whitetips[data_for_cleaning$Whitetips=="1O"]<-"10"

#Galapagos
data_for_cleaning$Galapagos = str_replace_all(data_for_cleaning$Galapagos[1:length(data_for_cleaning$Galapagos)], "[+]", "")
data_for_cleaning$Galapagos = str_replace_all(data_for_cleaning$Galapagos[1:length(data_for_cleaning$Galapagos)], " ", "")
data_for_cleaning$Galapagos[data_for_cleaning$Galapagos=="1,5"]<-"NA"
data_for_cleaning$Galapagos[data_for_cleaning$Galapagos=="-2"]<-"NA"
data_for_cleaning$Galapagos[data_for_cleaning$Galapagos=="O"]<-"0"

#Silky
data_for_cleaning$Silky = str_replace_all(data_for_cleaning$Silky[1:length(data_for_cleaning$Silky)], "[+]", "")
data_for_cleaning$Silky = str_replace_all(data_for_cleaning$Silky[1:length(data_for_cleaning$Silky)], " ", "")
data_for_cleaning$Silky[data_for_cleaning$Silky=="-2"]<-"NA"

#Blacktip
data_for_cleaning$Blacktips = str_replace_all(data_for_cleaning$Blacktips[1:length(data_for_cleaning$Blacktips)], "[+]", "")
data_for_cleaning$Blacktips = str_replace_all(data_for_cleaning$Blacktips[1:length(data_for_cleaning$Blacktips)], " ", "")
data_for_cleaning$Blacktips[data_for_cleaning$Blacktips=="-2"]<-"NA"

#Silvertip
data_for_cleaning$Silvertips = str_replace_all(data_for_cleaning$Silvertips[1:length(data_for_cleaning$Silvertips)], "[+]", "")
data_for_cleaning$Silvertips = str_replace_all(data_for_cleaning$Silvertips[1:length(data_for_cleaning$Silvertips)], " ", "")
data_for_cleaning$Silvertips[data_for_cleaning$Silvertips=="-2"]<-"NA"

#Whale Sharks
data_for_cleaning$WhaleSharks = str_replace_all(data_for_cleaning$WhaleSharks[1:length(data_for_cleaning$WhaleSharks)], " ", "")
data_for_cleaning$WhaleSharks[data_for_cleaning$WhaleSharks=="-2"]<-"NA"
data_for_cleaning$WhaleSharks[data_for_cleaning$WhaleSharks=="no"]<-"0"

#Tiger Sharks
data_for_cleaning$TigerSharks = str_replace_all(data_for_cleaning$TigerSharks[1:length(data_for_cleaning$TigerSharks)], " ", "")
data_for_cleaning$TigerSharks[data_for_cleaning$TigerSharks=="-2"]<-"NA"

#Eagle Rays
data_for_cleaning$EagleRays = str_replace_all(data_for_cleaning$EagleRays[1:length(data_for_cleaning$EagleRays)], " ", "")
data_for_cleaning$EagleRays[data_for_cleaning$EagleRays=="-2"]<-"NA"
data_for_cleaning$EagleRays[data_for_cleaning$EagleRays=="7,3"]<-"NA"
data_for_cleaning$EagleRays[data_for_cleaning$EagleRays=="no"]<-"0"

#Mobula Rays
data_for_cleaning$MobulaRays = str_replace_all(data_for_cleaning$MobulaRays[1:length(data_for_cleaning$MobulaRays)], " ", "")
data_for_cleaning$MobulaRays[data_for_cleaning$MobulaRays=="-2"]<-"NA"
data_for_cleaning$MobulaRays[data_for_cleaning$MobulaRays=="no"]<-"0"

#Manta Rays
data_for_cleaning$MantaRays = str_replace_all(data_for_cleaning$MantaRays[1:length(data_for_cleaning$MantaRays)], " ", "")
data_for_cleaning$MantaRays[data_for_cleaning$MantaRays=="-2"]<-"NA"
data_for_cleaning$MantaRays[data_for_cleaning$MantaRays=="no"]<-"0"
data_for_cleaning$MantaRays[data_for_cleaning$MantaRays=="si1"]<-"1"

#Marbled Rays
data_for_cleaning$MarbledRays = str_replace_all(data_for_cleaning$MarbledRays[1:length(data_for_cleaning$MarbledRays)], "[+]", "")
data_for_cleaning$MarbledRays = str_replace_all(data_for_cleaning$MarbledRays[1:length(data_for_cleaning$MarbledRays)], " ", "")
data_for_cleaning$MarbledRays[data_for_cleaning$MarbledRays=="-2"]<-"NA"
data_for_cleaning$MarbledRays[data_for_cleaning$MarbledRays=="1`"]<-"1"
data_for_cleaning$MarbledRays[data_for_cleaning$MarbledRays%in%c("3.2","3,5","5,3")]<-"NA"
data_for_cleaning$MarbledRays[data_for_cleaning$MarbledRays=="O"]<-"0"
data_for_cleaning$MarbledRays[data_for_cleaning$MarbledRays=="5g"]<-"5"
data_for_cleaning$MarbledRays[data_for_cleaning$MarbledRays=="10-"]<-"10"

#Turtles
data_for_cleaning$Turtles = str_replace_all(data_for_cleaning$Turtles[1:length(data_for_cleaning$Turtles)], " ", "")
data_for_cleaning$Turtles[data_for_cleaning$Turtles=="O"]<-"0"
data_for_cleaning$Turtles[data_for_cleaning$Turtles=="o"]<-"0"
data_for_cleaning$Turtles[data_for_cleaning$Turtles=="-2"]<-"NA"


#Make species data numeric
data_for_cleaning[,12:25]<-apply(data_for_cleaning[,12:25], 2, as.numeric)

#Clean up dive sites
data_for_cleaning$Site[data_for_cleaning$Site%in%c("Manuelita outside", "Manuelita out", 
	"M.Out", "M Out", "Manuelita our", "manuelita out", "manuelita outside",
	"Manueltta ouside", "Maunelita Outside")]<-"Manuelita Outside"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("Manuelita inside")]<-"Manuelita Inside"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("Manuelita Channel", "Manuelita CHANNEL",
	"Channel", "Manuelita Canal", "manuelita chanel",  "ManuelitaChannel", "manuelta channel", 
	"Manuelita Chanel","manuelita channel","Manuelita channel", "Maunelita Channel")]<-"Manuelita Channel"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("Dirty rock", "Roca sucia", 
	"R.Sucia", "Roca Sucia")]<-"Dirty Rock"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("Manuelita Coral", "Coral", "Gardens",
	"coral garden","Coral garden", "Coral Garden", "Coral Graden", "Manueota coral", 
	"manuelita coral", "Manuelita coral", "manuelita coral garden",
	"Manuelita coral garden")]<-"Manuelita Coral Garden"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("Small dos amigos","Dos amigos pequena", "small dos amigos",
	"Dos amigos pequeno", "Small Dos Amigos", "Dos Amigos Pequeño")]<-"Dos Amigos Pequenos"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("P.De Azucar", "P.Azucar", "Pan Azucar")]<-"Pan de Azucar"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("Chatam bay", "Bahia Chatham",
	"chatam", "Chatam", "Chatham", "chatham bay","chatham Bay", "Chatam Bay", "Chatham bay")]<-"Chatham Bay"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("alcyone", "Alycone")]<-"Alcyone"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("big dos amigos", "Big Dos Amigos",
	"Dos amigos grande","Dos Amigos Grande")]<-"Dos Amigos Grandes"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("Direty Rock", "dirty rock")]<-"Dirty Rock"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("pajara", "PAJARA", "Pajara Rock")]<-"Pajara"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("punta maria")]<-"Punta Maria"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("Submerged rock", "submerged rock", "Sumergida",
	"Sumerged Rock")]<-"Submerged Rock"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("ULLOA", "ulloa", "Lobster Rock", "Lobster rock", "Ulloa")]<-"Lobster Rock/Ulloa"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("viking rock", "Viking", "Vaikig Rock", "Vikinga",
	"Viking rock")]<-"Viking Rock"
data_for_cleaning$Site[data_for_cleaning$Site%in%c("Shark Fin Rock")]<-"Sharkfin Rock"


data_for_cleaning<-subset(data_for_cleaning, !is.na(Site))

#Clean up dive master names
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("ARIK")]<-"Arik"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("brayan", "BRAYAN")]<-"Brayan"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("Carlos de la Cruz", "Carlos del Monte Campana")]<-"Carlos"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("chipopa")]<-"Chipopa"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("fede", "Fede", "FEDERICO")]<-"Federico"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("FELIPE", "Felipe Chacon", "Felo",
	"FELIPE CHACON")]<-"Felipe"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("Fernado", "Fernandito", "FERNANDITO",
	"FELIPE CHACON", "FERNANDO", "Fernando Sanchez")]<-"Fernando"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("GEOVANNY")]<-"Geovanny"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("hector", "HECTOR", "hector v", 
	"Hector Venegas")]<-"Hector"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("J MANUEL", "J. Manuel", "Jmanuel",
	"J Manuel", "JUAN MANUEL", "Juan Manuel Camargo", "Manuel")]<-"Juan Manuel"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("LITUS")]<-"Litus"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("mao", "Mao", "MAO", "Mau", "MAU", 
	"mauricio", "mauricio \"mao\"", "mauricio sanchez", "mauricio, mao", "Mou")]<-"Mauricio"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("merlin", "MERLIN")]<-"Merlin"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("Michi", "ROY","Roy \"El Michi\"",
	"Roy \"Michi\"", "Roy Mora", "ROY MORA")]<-"Roy"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("pipi", "PIPI")]<-"Pipi"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("pochet")]<-"Pochet"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("SERGIO")]<-"Sergio"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("stefan","Stefan Glezer", "Estefan")]<-"Stefan"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("YOYO")]<-"Yoyo"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("MORADO")]<-"Morado"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("DM Pius")]<-"Pius"
data_for_cleaning$DiverName[data_for_cleaning$DiverName%in%c("Jim Bo", "JIM")]<-"Jim"

#Clean up boat names

data_for_cleaning$Boat[data_for_cleaning$Boat%in%c("ARGO", "argo")]<-"Argo"
data_for_cleaning$Boat[data_for_cleaning$Boat%in%c("Sea hunter", "SEA HUNTER", 
	"SH", "SG", "sea hunter")]<-"Sea Hunter"

write.csv(data_for_cleaning, file="~/Documents/git_repos/Cocos_El_Nino/Data/Cocos_Aug2019_CleanedJan2020.csv") # write file for new data cleaned

write.csv(data_for_cleaning, file="c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/Data/Cocos_Aug2019_CleanedJan2020.csv") # write file for new data cleaned

