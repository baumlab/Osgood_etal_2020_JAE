#This code is for making Figure 1 and S1 of Osgood et al. 2020

setwd("c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/")
setwd("~/Documents/git_repos/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019

#Remove NAs in Visibility
cocos<-subset(cocos, !is.na(Visibility))

#Turn fixed and random effects into factors if necessary
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)
cocos$ID <- as.factor(cocos$ID)

#mean, max, min SST by year
ag_mean<-aggregate(SST~Year, cocos, mean)
ag_min<-aggregate(SST~Year, cocos, min)
ag_max<-aggregate(SST~Year, cocos, max)

#mean, max, min ONI by year
ag_mean_ONI<-aggregate(ONI~Year, cocos, mean)
ag_min_ONI<-aggregate(ONI~Year, cocos, min)
ag_max_ONI<-aggregate(ONI~Year, cocos, max)

par(mfrow=c(1,2))
#SST plot (b)
plot(SST~Year, ag_mean, type="n", ylim=c(24,32), xlim=c(1993,2019), bty="n", xaxt="n",
 las=2, cex.axis=1.8, ylab="", xlab="")
axis(1, labels=c("", seq(1995,2019,3)), at=seq(1992,2019,3), pos=24, las=2, cex.axis=1.5)
rect(1997,24,1998,32, col="pink")
rect(2015,24,2016,32, col="pink")
#mtext("SST", side=2, line=3, cex=1.2)
points(SST~Year, ag_max, type="l", col="red", lwd=2)
points(SST~Year, ag_min, type="l", col="blue", lwd=2)
points(SST~Year, ag_mean, type="l", col="black", lwd=2)

#ONI plot (c)
plot(ONI~Year, ag_mean_ONI, type="n", ylim=c(-2,3), xlim=c(1993,2019), bty="n", xaxt="n",
 las=2, cex.axis=1.8, ylab="", xlab="")
axis(1, labels=c("", seq(1995,2019,3)), at=seq(1992,2019,3), pos=-2, las=2, cex.axis=1.5)
rect(1997,-2,1998,3, col="pink")
rect(2015,-2,2016,3, col="pink")
#mtext("ONI", side=2, line=3, cex=1.2)
points(ONI~Year, ag_max_ONI, type="l", col="red", lwd=2)
points(ONI~Year, ag_min_ONI, type="l", col="blue", lwd=2)
points(ONI~Year, ag_mean_ONI, type="l", col="black", lwd=2)



#old versions when mean, min, and max were plotted in seperate panels

#par(mfrow=c(1,3), mar=c(7,6,1.5,1))
#plot(SST~Year, ag_mean, type="l", ylim=c(26,29), xlim=c(1993,2019), bty="n", xaxt="n",
# las=2, cex.axis=1.5, ylab="", xlab="")
#axis(1, labels=c("", seq(1995,2019,3)), at=seq(1992,2019,3), pos=26, las=2, cex.axis=1.5)
#mtext("SST", side=2, line=4, cex=1.2)
#mtext("(a)", side=3, adj=0, cex=1.2)
#plot(SST~Year, ag_min, type="l", col="red", ylim=c(24,27), xlim=c(1993,2019), bty="n", cex.lab=1.5, xaxt="n",
# las=2, cex.axis=1.5, ylab="", cex.lab=1.5)
#axis(1, labels=c(1993:2019), at=c(1993:2019), pos=24, las=2, cex.axis=1.5)
#mtext("(b)", side=3, adj=0, cex=1.2)
#plot(SST~Year, ag_max, type="l", col="blue", ylim=c(27,32), xlim=c(1993,2019), bty="n", xaxt="n",
# las=2, cex.axis=1.5, ylab="", xlab="")
#axis(1, labels=c(1993:2019), at=c(1993:2019), pos=27, las=2, cex.axis=1.5)
#mtext("(c)", side=3, adj=0, cex=1.2)

#par(mfrow=c(1,3), mar=c(7,6,1.5,1))
#plot(ONI~Year, ag_mean, type="l", ylim=c(-1.5,2), xlim=c(1993,2019), bty="n", xaxt="n",
# las=2, cex.axis=1.5, ylab="", xlab="")
#axis(1, labels=c(1993:2019), at=c(1993:2019), pos=-1.5, las=2, cex.axis=1.5)
#mtext("ONI", side=2, line=4, cex=1.2)
#mtext("(a)", side=3, adj=0, cex=1.2)
#plot(ONI~Year, ag_min, type="l", col="red", ylim=c(-2,1), xlim=c(1993,2019), bty="n", cex.lab=1.5, xaxt="n",
# las=2, cex.axis=1.5, ylab="", cex.lab=1.5)
#axis(1, labels=c(1993:2019), at=c(1993:2019), pos=-2, las=2, cex.axis=1.5)
#mtext("(b)", side=3, adj=0, cex=1.2)
#plot(ONI~Year, ag_max, type="l", col="blue", ylim=c(-1,3), xlim=c(1993,2019), bty="n", xaxt="n",
# las=2, cex.axis=1.5, ylab="", xlab="")
#axis(1, labels=c(1993:2019), at=c(1993:2019), pos=-1, las=2, cex.axis=1.5)
#mtext("(c)", side=3, adj=0, c3ex=1.2)

lm.sst<-lm(SST~StudyYear, cocos)
summary(lm.sst)

lm.oni<-lm(ONI~StudyYear, cocos)
summary(lm.oni)

plot(ONI~factor(Year), cocos)