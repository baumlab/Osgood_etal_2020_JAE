#this script creates a figure that displayed the observation distributions for each species with count data

#call in data
setwd("c:/Users/gjosg/Documents/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019

#Remove NAs in Visibility
cocos<-subset(cocos, !is.na(Visibility))

#Turn fixed and random effects into factors if necessary
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)
cocos$ID <- as.factor(cocos$ID)

CocosData<-cocos

nf<-layout(matrix(c(1:4), 2, 2, byrow=TRUE),respect=T)

par(oma=c(2,0,0,0))


o<-par(mar=c(2,4,1,0),mgp=c(3,0.6,0))

#histogram with zeros removed from distributions
bbb=hist(CocosData$Hammerhead[CocosData$Hammerheads>0],breaks='Scott',plot=F)
   		plot(bbb,ylim=c(0,12000),ylab='',xlab='',main='',las=1,col='black')
 
mtext('(a)',3,line=-1.5,cex=0.8,font=2,adj=1)


#histogram with zeros removed from distributions
bbb=hist(CocosData$Whitetips[CocosData$Whitetips>0],breaks='Scott',plot=F)
   		plot(bbb,ylab='',xlab='',main='',las=1,col='black',ylim=c(0,7000))
 
mtext('(b)',3,line=-1.5,cex=0.8,font=2,adj=1)

#################

bbb=hist(CocosData$MarbledRays[CocosData$MarbledRays>0],breaks='Scott',plot=F)
   		plot(bbb,ylim=c(0,7000),ylab='',xlab='',main='',las=1,col='black')

mtext('(c)',3,line=-1.5,cex=0.8,font=2,adj=1)

##########################
names(CocosData)

bbb=hist(CocosData$EagleRays[CocosData$EagleRays>0],breaks='Scott',plot=F)
   		plot(bbb,ylim=c(0,5000),xlim=c(0,60),ylab='',xlab='',main='',las=1,col='black')
mtext('(d)',3,line=-1.5,cex=0.8,font=2,adj=1)

mtext('Frequency',side=2,outer=T,font=2,line=-1,cex=0.8)
mtext('Number observed per dive',side=1,outer=T,font=2,line=-0.1,cex=0.8)