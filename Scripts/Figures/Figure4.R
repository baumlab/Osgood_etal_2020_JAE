##This script is for making an updated figure of trends for each species
#created by Geoffrey J. Osgood
#
setwd("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino")
#######################################################
#load in models########################################
#######################################################
load(file="Models/hammerhead_linear_yre.Rdata")

load(file="Models/MobulaRay_linear_yre.Rdata")

load(file="Models/MarbledRay_linear_yre.Rdata")

load(file="Models/EagleRay_quad_yre.Rdata")

load(file="Models/Whitetip_linear_yre.Rdata")

load(file="Models/Blacktip_linear_yre.Rdata")

load(file="Models/TigerSharks_cubic_yre.Rdata")


#######################################################
#time series figure ###################################
#######################################################
cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019
cocos<-subset(cocos, !is.na(Visibility))

hammerheads<-subset(cocos, Hammerheads>=0)
hammerheads<-hammerheads[!is.na(hammerheads$Hammerheads),]
whitetips<-subset(cocos, Whitetips>=0 & Whitetips<1000)
whitetips<-whitetips[!is.na(whitetips$Whitetips),]
marbledrays<-subset(cocos, MarbledRays>=0)
marbledrays<-marbledrays[!is.na(marbledrays$MarbledRays),]
eaglerays<-subset(cocos, EagleRays>=0)
eaglerays<-eaglerays[!is.na(eaglerays$EagleRays),]

cocos$TigerSharks[cocos$TigerSharks<0]<-1 #keep all records for pres/abs analysis
cocos$TigerSharks<-as.numeric(cocos$TigerSharks>0) #change to pres/abs only
cocos$Blacktips[cocos$Blacktips<0]<-1 #keep all records for pres/abs analysis
cocos$Blacktips<-as.numeric(cocos$Blacktips>0) #change to pres/abs only

cocos$MobulaRays[cocos$MobulaRays<0]<-1 #change -1 to 1 to indicate presence
cocos$MantaRays[cocos$MantaRays<0]<-1 #change -1 to 1 to indicate presence
cocos$MobulaRays<-(cocos$MantaRays+cocos$MobulaRays) #combine all Mobula rays
cocos$MobulaRays<-as.numeric(cocos$MobulaRays>0) #change to pres/abs only

ag.ham<-aggregate(Hammerheads~Year, hammerheads, mean)
ag.whitetip<-aggregate(Whitetips~Year, hammerheads, mean)
ag.marbled<-aggregate(MarbledRays~Year, marbledrays, mean)
ag.eagle<-aggregate(EagleRays~Year, eaglerays, mean)
ag.blacktip<-aggregate(Blacktips~Year, cocos, mean)
ag.mobula<-aggregate(MobulaRays~Year, cocos, mean)
ag.tiger<-aggregate(TigerSharks~Year, cocos, mean)
ag.oni<-aggregate(ONI~Year, cocos, mean)


pred.ham<-predict(hammerhead.linear.YRE, type="response")
pred.white<-predict(Whitetip.linear.YRE, type="response")
pred.black<-predict(Blacktip.linear.YRE, type="response")
pred.mobula<-predict(MobulaRay.linear.YRE, type="response")
pred.marbled<-predict(MarbledRay.linear.YRE, type="response")
pred.eagle<-predict(EagleRay.quad.YRE, type="response")
pred.tiger<-predict(TigerSharks.cubic.YRE, type="response")


ag.ham.pred<-aggregate(pred.ham,
                       by=list(hammerheads$Year), mean)
ag.whitetip.pred<-aggregate(pred.white,
                       by=list(whitetips$Year), mean)
ag.blacktip.pred<-aggregate(pred.black,
                            by=list(cocos$Year), mean)
ag.mobula.pred<-aggregate(pred.mobula,
                            by=list(cocos$Year), mean)
ag.marbled.pred<-aggregate(pred.marbled,
                         by=list(marbledrays$Year), mean)
ag.eagle.pred<-aggregate(pred.eagle,
                         by=list(eaglerays$Year), mean)
ag.tiger.pred<-aggregate(pred.tiger,
                         by=list(cocos$Year), mean)

#Figure 4
size<-0.7 #set size of text in the plot
line_sp<-1.8

pdf(file="c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Manuscript/Figures/Figure_4_revised.pdf", 
  width = 6, height = 4)

par(mfrow=c(4,2), mar=c(1,1,1,3), oma=c(2,2,1,2), mgp=c(0,0.2,0), xpd=TRUE)

plot(TigerSharks~Year, ag.tiger, type="n", xaxt="n", las=2, 
     bty="n", ylim=c(0,0.25), ylab="", xlab="", cex.axis=size, lwd=1, tck=-0.02)
segments(1993, mean(cocos$TigerSharks), 2019, mean(cocos$TigerSharks))


rect(2004,0,2005,0.25, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2006,0,2007,0.25, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2014,0,2015,0.25, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2018,0,2019,0.25, col=rgb(0.8,0,0.2,alpha=0.2))
rect(1994,0,1995,0.25, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2002,0,2003,0.25, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2009,0,2010,0.25, col=rgb(0.8,0,0.2,alpha=0.4))
rect(1997,0,1998,0.25, col=rgb(0.8,0,0.2,alpha=0.7))
rect(2015,0,2016,0.25, col=rgb(0.8,0,0.2,alpha=0.7))

rect(2000,0,2001,0.25, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2005,0,2006,0.25, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2008,0,2009,0.25, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2016,0,2018,0.25, col=rgb(0.1,0,0.9,alpha=0.2))
rect(1995,0,1996,0.25, col=rgb(0.1,0,0.9,alpha=0.4))
rect(2011,0,2012,0.25, col=rgb(0.1,0,0.9,alpha=0.4))
rect(1998,0,2000,0.25, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2007,0,2008,0.25, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2010,0,2011,0.25, col=rgb(0.1,0,0.9,alpha=0.7))

points(TigerSharks~Year, ag.tiger, type="l", xaxt="n", lty=1, lwd=1)
points(x~Group.1, ag.tiger.pred, type="l", xaxt="n", lty=2, lwd=1)
mtext("(a)", side=3, adj=0, cex=size)
mtext("Mean proportion", side=2, line=line_sp, cex=size)
par(new=TRUE)
plot(ONI~Year, ag.oni, type="l", xaxt="n", 
     bty="n", ylim=c(-2,2), ylab="", xlab="", yaxt="n", col="slategray4", lwd=1)


plot(Hammerheads~Year, ag.ham, type="n", xaxt="n", las=2, 
  bty="n", ylim=c(0,200), ylab="", xlab="", cex.lab=size, cex.axis=size, lwd=1, tck=-0.02)
segments(1993, mean(hammerheads$Hammerheads), 2019, mean(hammerheads$Hammerheads))

rect(2004,0,2005,200, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2006,0,2007,200, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2014,0,2015,200, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2018,0,2019,200, col=rgb(0.8,0,0.2,alpha=0.2))
rect(1994,0,1995,200, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2002,0,2003,200, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2009,0,2010,200, col=rgb(0.8,0,0.2,alpha=0.4))
rect(1997,0,1998,200, col=rgb(0.8,0,0.2,alpha=0.7))
rect(2015,0,2016,200, col=rgb(0.8,0,0.2,alpha=0.7))

rect(2000,0,2001,200, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2005,0,2006,200, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2008,0,2009,200, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2016,0,2018,200, col=rgb(0.1,0,0.9,alpha=0.2))
rect(1995,0,1996,200, col=rgb(0.1,0,0.9,alpha=0.4))
rect(2011,0,2012,200, col=rgb(0.1,0,0.9,alpha=0.4))
rect(1998,0,2000,200, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2007,0,2008,200, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2010,0,2011,200, col=rgb(0.1,0,0.9,alpha=0.7))

points(Hammerheads~Year, ag.ham, type="l", xaxt="n", lty=1, lwd=1)
points(x~Group.1, ag.ham.pred, type="l", xaxt="n", lty=2, lwd=1)
mtext("(b)", side=3, adj=0, cex=size)
mtext("Mean count", side=2, line=1.5, cex=size)
par(new=TRUE)
plot(ONI~Year, ag.oni, type="l", xaxt="n", 
     bty="n", ylim=c(-2,2), ylab="", xlab="", yaxt="n", col="slategray4", lwd=1)
axis(4, labels=c(-2:2), 
     at=c(-2:2), las=2, cex.axis=size, tck=-0.02)
mtext("ONI value", line=0.7 ,side = 4, cex=size)

plot(Blacktips~Year, ag.blacktip, type="n", xaxt="n", las=2, 
 bty="n", ylim=c(0,0.15), ylab="", xlab="", cex.lab=size, cex.axis=size, lwd=1, tck=-0.02)
segments(1993,mean(cocos$Blacktips),2019,mean(cocos$Blacktips))


rect(2004,0,2005,0.15, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2006,0,2007,0.15, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2014,0,2015,0.15, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2018,0,2019,0.15, col=rgb(0.8,0,0.2,alpha=0.2))
rect(1994,0,1995,0.15, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2002,0,2003,0.15, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2009,0,2010,0.15, col=rgb(0.8,0,0.2,alpha=0.4))
rect(1997,0,1998,0.15, col=rgb(0.8,0,0.2,alpha=0.7))
rect(2015,0,2016,0.15, col=rgb(0.8,0,0.2,alpha=0.7))

rect(2000,0,2001,0.15, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2005,0,2006,0.15, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2008,0,2009,0.15, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2016,0,2018,0.15, col=rgb(0.1,0,0.9,alpha=0.2))
rect(1995,0,1996,0.15, col=rgb(0.1,0,0.9,alpha=0.4))
rect(2011,0,2012,0.15, col=rgb(0.1,0,0.9,alpha=0.4))
rect(1998,0,2000,0.15, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2007,0,2008,0.15, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2010,0,2011,0.15, col=rgb(0.1,0,0.9,alpha=0.7))

points(Blacktips~Year, ag.blacktip, type="l", xaxt="n", lty=1, lwd=1)
points(x~Group.1, ag.blacktip.pred, type="l", xaxt="n", lty=2, lwd=1)
mtext("(c)", side=3, adj=0, cex=size)
mtext("Mean proportion", side=2, line=line_sp, cex=size)
par(new=TRUE)
plot(ONI~Year, ag.oni, type="l", xaxt="n", 
     bty="n", ylim=c(-2,2), ylab="", xlab="", yaxt="n", col="slategray4", lwd=1)

plot(Whitetips~Year, ag.whitetip, type="n", xaxt="n", las=2, 
     bty="n", ylim=c(0,80), ylab="", xlab="", cex.lab=size, cex.axis=size, lwd=1, tck=-0.02)
segments(1993, mean(whitetips$Whitetips, na.rm=TRUE), 2019,  mean(whitetips$Whitetips, na.rm=TRUE))

rect(2004,0,2005,80, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2006,0,2007,80, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2014,0,2015,80, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2018,0,2019,80, col=rgb(0.8,0,0.2,alpha=0.2))
rect(1994,0,1995,80, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2002,0,2003,80, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2009,0,2010,80, col=rgb(0.8,0,0.2,alpha=0.4))
rect(1997,0,1998,80, col=rgb(0.8,0,0.2,alpha=0.7))
rect(2015,0,2016,80, col=rgb(0.8,0,0.2,alpha=0.7))

rect(2000,0,2001,80, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2005,0,2006,80, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2008,0,2009,80, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2016,0,2018,80, col=rgb(0.1,0,0.9,alpha=0.2))
rect(1995,0,1996,80, col=rgb(0.1,0,0.9,alpha=0.4))
rect(2011,0,2012,80, col=rgb(0.1,0,0.9,alpha=0.4))
rect(1998,0,2000,80, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2007,0,2008,80, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2010,0,2011,80, col=rgb(0.1,0,0.9,alpha=0.7))

points(Whitetips~Year, ag.whitetip, type="l", xaxt="n", lty=1, lwd=1)
points(x~Group.1, ag.whitetip.pred, type="l", xaxt="n", lty=2, lwd=1)
mtext("(d)", side=3, adj=0, cex=size)
mtext("Mean count", side=2, line=1.5, cex=size)
par(new=TRUE)
plot(ONI~Year, ag.oni, type="l", xaxt="n", 
     bty="n", ylim=c(-2,2), ylab="", xlab="", yaxt="n", col="slategray4", lwd=1)
axis(4, labels=c(-2:2), 
     at=c(-2:2), las=2, cex.axis=size, tck=-0.02)
mtext("ONI value", line=0.7,side = 4, cex=size)

plot(MobulaRays~Year, ag.mobula, type="n", xaxt="n", las=2, 
     bty="n", ylim=c(0,0.4), ylab="", xlab="", cex.lab=size, cex.axis=size, lwd=1, tck=-0.02)
segments(1993, mean(cocos$MobulaRays, na.rm=TRUE), 2019, mean(cocos$MobulaRays, na.rm=TRUE))


rect(2004,0,2005,0.4, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2006,0,2007,0.4, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2014,0,2015,0.4, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2018,0,2019,0.4, col=rgb(0.8,0,0.2,alpha=0.2))
rect(1994,0,1995,0.4, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2002,0,2003,0.4, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2009,0,2010,0.4, col=rgb(0.8,0,0.2,alpha=0.4))
rect(1997,0,1998,0.4, col=rgb(0.8,0,0.2,alpha=0.7))
rect(2015,0,2016,0.4, col=rgb(0.8,0,0.2,alpha=0.7))

rect(2000,0,2001,0.4, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2005,0,2006,0.4, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2008,0,2009,0.4, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2016,0,2018,0.4, col=rgb(0.1,0,0.9,alpha=0.2))
rect(1995,0,1996,0.4, col=rgb(0.1,0,0.9,alpha=0.4))
rect(2011,0,2012,0.4, col=rgb(0.1,0,0.9,alpha=0.4))
rect(1998,0,2000,0.4, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2007,0,2008,0.4, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2010,0,2011,0.4, col=rgb(0.1,0,0.9,alpha=0.7))

points(MobulaRays~Year, ag.mobula, type="l", xaxt="n", lty=1, lwd=1)
points(x~Group.1, ag.mobula.pred, type="l", xaxt="n", lty=2, lwd=1)
mtext("(e)", side=3, adj=0, cex=size)
mtext("Mean proportion", side=2, line=line_sp, cex=size)
par(new=TRUE)
plot(ONI~Year, ag.oni, type="l", xaxt="n", 
     bty="n", ylim=c(-2,2), ylab="", xlab="", yaxt="n", col="slategray4", lwd=1)

plot(EagleRays~Year, ag.eagle, type="n", xaxt="n", las=2, 
  bty="n", ylim=c(0,2.5), ylab="", xlab="", cex.lab=size, cex.axis=size, lwd=1, tck=-0.02)
segments(1993, mean(eaglerays$EagleRays, na.rm=TRUE), 2019, mean(eaglerays$EagleRays, na.rm=TRUE))

rect(2004,0,2005,2.5, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2006,0,2007,2.5, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2014,0,2015,2.5, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2018,0,2019,2.5, col=rgb(0.8,0,0.2,alpha=0.2))
rect(1994,0,1995,2.5, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2002,0,2003,2.5, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2009,0,2010,2.5, col=rgb(0.8,0,0.2,alpha=0.4))
rect(1997,0,1998,2.5, col=rgb(0.8,0,0.2,alpha=0.7))
rect(2015,0,2016,2.5, col=rgb(0.8,0,0.2,alpha=0.7))

rect(2000,0,2001,2.5, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2005,0,2006,2.5, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2008,0,2009,2.5, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2016,0,2018,2.5, col=rgb(0.1,0,0.9,alpha=0.2))
rect(1995,0,1996,2.5, col=rgb(0.1,0,0.9,alpha=0.4))
rect(2011,0,2012,2.5, col=rgb(0.1,0,0.9,alpha=0.4))
rect(1998,0,2000,2.5, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2007,0,2008,2.5, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2010,0,2011,2.5, col=rgb(0.1,0,0.9,alpha=0.7))

points(EagleRays~Year, ag.eagle, type="l", xaxt="n", lty=1, lwd=1)
points(x~Group.1, ag.eagle.pred, type="l", xaxt="n", lty=2, lwd=1)
mtext("(f)", side=3, adj=0, cex=size)
mtext("Mean count", side=2, line=1.5, cex=size)
axis(1, labels=seq(1993,2019,2), 
     at=seq(1993,2019,2), las=2, pos=0, cex.axis=size, tck=-0.02)
par(new=TRUE)
plot(ONI~Year, ag.oni, type="l", xaxt="n", 
     bty="n", ylim=c(-2,2), ylab="", xlab="", yaxt="n", col="slategray4", lwd=1)
axis(4, labels=c(-2:2), 
     at=c(-2:2), las=2, cex.axis=size, tck=-0.02)
mtext("ONI value", line=0.7,side = 4, cex=size)
mtext("Year", line=line_sp,side = 1, cex=size)

plot(MarbledRays~Year, ag.marbled, type="n", xaxt="n", las=2, 
     bty="n", ylim=c(0,20), ylab="", xlab="", cex.lab=2, cex.axis=size, lwd=1, tck=-0.02)
segments(1993, mean(marbledrays$MarbledRays),2019, mean(marbledrays$MarbledRays))

rect(2004,0,2005,20, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2006,0,2007,20, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2014,0,2015,20, col=rgb(0.8,0,0.2,alpha=0.2))
rect(2018,0,2019,20, col=rgb(0.8,0,0.2,alpha=0.2))
rect(1994,0,1995,20, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2002,0,2003,20, col=rgb(0.8,0,0.2,alpha=0.4))
rect(2009,0,2010,20, col=rgb(0.8,0,0.2,alpha=0.4))
rect(1997,0,1998,20, col=rgb(0.8,0,0.2,alpha=0.7))
rect(2015,0,2016,20, col=rgb(0.8,0,0.2,alpha=0.7))

rect(2000,0,2001,20, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2005,0,2006,20, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2008,0,2009,20, col=rgb(0.1,0,0.9,alpha=0.2))
rect(2016,0,2018,20, col=rgb(0.1,0,0.9,alpha=0.2))
rect(1995,0,1996,20, col=rgb(0.1,0,0.9,alpha=0.4))
rect(2011,0,2012,20, col=rgb(0.1,0,0.9,alpha=0.4))
rect(1998,0,2000,20, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2007,0,2008,20, col=rgb(0.1,0,0.9,alpha=0.7))
rect(2010,0,2011,20, col=rgb(0.1,0,0.9,alpha=0.7))

points(MarbledRays~Year, ag.marbled, type="l", xaxt="n", lty=1, lwd=1)
points(x~Group.1, ag.marbled.pred, type="l", xaxt="n", lty=2, lwd=1)
mtext("(g)", side=3, adj=0, cex=size)
mtext("Mean count", side=2, line=line_sp, cex=size)
axis(1, labels=seq(1993,2019,2), 
     at=seq(1993,2019,2), las=2, pos=0, cex.axis=size, tck=-0.02)
par(new=TRUE)
plot(ONI~Year, ag.oni, type="l", xaxt="n", 
     bty="n", ylim=c(-2,2), ylab="", xlab="", yaxt="n", col="slategray4", lwd=1)
axis(4, labels=c(-2:2), 
     at=c(-2:2), las=2, cex.axis=size, tck=-0.02)
mtext("ONI value", line=0.7, side = 4, cex=size, tck=-0.02)
mtext("Year", line=line_sp,side = 1, cex=size)

plot(TigerSharks~Year, ag.tiger, type="n", xaxt="n", yaxt="n", bty="n", ylab="", xlab="")
legend(1994,0.2, c("Strong La Nina", "Moderate La Nina", "Weak La Nina", "Neutral",
    "Weak El Nino", "Moderate El Nino", "Strong El Nino"), 
        fill=c(rgb(0.1,0,0.9,alpha=0.7), rgb(0.1,0,0.9,alpha=0.4),
            rgb(0.1,0,0.9,alpha=0.2), "white",
            rgb(0.8,0,0.2,alpha=0.2), rgb(0.8,0,0.2,alpha=0.4),
            rgb(0.8,0,0.2,alpha=0.7)), cex=size, bty="n", ncol=2)

dev.off()