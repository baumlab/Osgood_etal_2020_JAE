#This script is for creating Figure 3 of Osgood et al. 2020
#created by Geoffrey J. Osgood
#

setwd("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/")

library(glmmTMB)
library(lme4)

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019

cocos$El_Nino_Strength<-NULL
for (i in 1:nrow(cocos)) {
if (cocos$ONI[i]>=0.5 & cocos$ONI[i]<1){
  cocos$El_Nino_Strength[i]<-"Weaker\nEl Nino"
} else { 
  if (cocos$ONI[i]>=(1)){
    cocos$El_Nino_Strength[i]<-"Stronger\nEl Nino"} else {
  if (cocos$ONI[i]<=(-0.5)& cocos$ONI[i]>(-1)){
   cocos$El_Nino_Strength[i]<-"Weaker\nLa Nina"} else { 
     if (cocos$ONI[i]<=(-1)){
     cocos$El_Nino_Strength[i]<-"Stronger\nLa Nina"} else {
      cocos$El_Nino_Strength[i]<-"Neutral"}}}}
}

levels<-c("Stronger\nLa Nina", "Weaker\nLa Nina", "Neutral", "Weaker\nEl Nino", 
  "Stronger\nEl Nino")

dat <- expand.grid(El_Nino_Strength = c(levels(factor(cocos$El_Nino_Strength, levels=levels))))


#Prepare hammerhead data
hammerhead<-subset(cocos, Hammerheads>=0) #remove -1's which indicate presence only
ham_model<-glmmTMB(Hammerheads~El_Nino_Strength, family="nbinom2", hammerhead)
ham_model1<-glmmTMB(Hammerheads~StudyYear, family="nbinom2", hammerhead)

#pick most precise year
pred_ham1<-predict(ham_model1, newdata = data.frame(StudyYear=c(0:27)), se.fit=T, type="response")
pred_frame_ham1 <- data.frame(StudyYear = c(0:27), 
                                    fit = pred_ham1$fit,
                                    error = pred_ham1$se.fit/pred_ham1$fit)

#get fits for each el nino strength level
pred_ham<-predict(ham_model, newdata = data.frame(El_Nino_Strength =dat),
  se.fit=T, type="response")
pred_frame_ham <- data.frame(El_Nino_Strength = dat, 
                                    fit = pred_ham$fit,
                                    ci.lower = pred_ham$fit - qnorm(0.975,0,1)*pred_ham$se.fit, 
                                    ci.upper = pred_ham$fit + qnorm(0.975,0,1)*pred_ham$se.fit)

#Prepare Whitetip
whitetips<-subset(cocos, Whitetips>=0) #remove -1's which indicate presence only
white_model<-glmmTMB(Whitetips~El_Nino_Strength, whitetips)
white_model1<-glmmTMB(Whitetips~StudyYear+(1|StudyYear), family="nbinom2", whitetips)

#pick most precise year
pred_white1<-predict(white_model1, newdata = data.frame(StudyYear=c(0:27)), se.fit=T, type="response")
pred_frame_white1 <- data.frame(StudyYear = c(0:27), 
                                    fit = pred_white1$fit,
                                    error = pred_white1$se.fit/pred_white1$fit)

#get fits for each el nino strength level
pred_white<-predict(white_model, newdata = data.frame(El_Nino_Strength =dat),
  se.fit=T, type="response")
pred_frame_white <- data.frame(El_Nino_Strength = dat, 
                                    fit = pred_white$fit,
                                    ci.lower = pred_white$fit - qnorm(0.975,0,1)*pred_white$se.fit, 
                                    ci.upper = pred_white$fit + qnorm(0.975,0,1)*pred_white$se.fit)


#Prepare mobula ray data
cocos$MobulaRays[cocos$MobulaRays<0]<-1 #change -1 to 1 to indicate presence
cocos$MantaRays[cocos$MantaRays<0]<-1 #change -1 to 1 to indicate presence
cocos$MobulaRays<-(cocos$MantaRays+cocos$MobulaRays) #combine all Mobula rays
cocos$MobulaRays<-as.numeric(cocos$MobulaRays>0) #change to pres/abs only

mobula_model<-glmmTMB(MobulaRays~El_Nino_Strength, cocos, family="binomial")
mobula_model1<-glmmTMB(MobulaRays~StudyYear+(1|StudyYear), cocos, family="binomial")

#pick most precise year
pred_mobula1<-predict(mobula_model1, newdata = data.frame(StudyYear=c(0:27)), se.fit=T, type="response")
pred_frame_mobula1 <- data.frame(StudyYear = c(0:27), 
                                    fit = pred_mobula1$fit,
                                    error = pred_mobula1$se.fit/pred_mobula1$fit)

#get fits for each el nino strength level
pred_mobula<-predict(mobula_model, newdata = data.frame(El_Nino_Strength =dat),
  se.fit=T, type="response")
pred_frame_mobula <- data.frame(El_Nino_Strength = dat, 
                                    fit = pred_mobula$fit,
                                    ci.lower = pred_mobula$fit - qnorm(0.975,0,1)*pred_mobula$se.fit, 
                                    ci.upper = pred_mobula$fit + qnorm(0.975,0,1)*pred_mobula$se.fit)


#Prepare Eagle ray data
eagleray<-subset(cocos, EagleRays>=0) #remove -1's which indicate presence only
eagleray_model<-glmmTMB(EagleRays~El_Nino_Strength,  family="nbinom2", eagleray)
eagleray_model1<-glmmTMB(EagleRays~StudyYear+(1|StudyYear),  family="nbinom2", eagleray)

#pick most precise year
pred_eagleray1<-predict(eagleray_model1, newdata = data.frame(StudyYear=c(0:27)), se.fit=T, type="response")
pred_frame_eagleray1 <- data.frame(StudyYear = c(0:27), 
                                    fit = pred_eagleray1$fit,
                                    error = pred_eagleray1$se.fit/pred_eagleray1$fit)

#get fits for each el nino strength level
pred_eagleray<-predict(eagleray_model, newdata = data.frame(El_Nino_Strength =dat),
  se.fit=T, type="response")
pred_frame_eagleray <- data.frame(El_Nino_Strength = dat, 
                                    fit = pred_eagleray$fit,
                                    ci.lower = pred_eagleray$fit - qnorm(0.975,0,1)*pred_eagleray$se.fit, 
                                    ci.upper = pred_eagleray$fit + qnorm(0.975,0,1)*pred_eagleray$se.fit)


#Prepare Marbled Ray data
marbledray<-subset(cocos, MarbledRays>=0) #remove -1's which indicate presence only
marbledray_model<-glmmTMB(MarbledRays~El_Nino_Strength, marbledray, family="nbinom2")
marbledray_model1<-glmmTMB(MarbledRays~StudyYear+(1|StudyYear), marbledray, family="nbinom2")

#pick most precise year
pred_marbledray1<-predict(marbledray_model1, newdata = data.frame(StudyYear=c(0:27)), se.fit=T, type="response")
pred_frame_marbledray1 <- data.frame(StudyYear = c(0:27), 
                                    fit = pred_marbledray1$fit,
                                    error = pred_marbledray1$se.fit/pred_marbledray1$fit)

#get fits for each el nino strength level
pred_marbledray<-predict(marbledray_model, newdata = data.frame(El_Nino_Strength =dat),
  se.fit=T, type="response")
pred_frame_marbledray <- data.frame(El_Nino_Strength = dat, 
                                    fit = pred_marbledray$fit,
                                    ci.lower = pred_marbledray$fit - qnorm(0.975,0,1)*pred_marbledray$se.fit, 
                                    ci.upper = pred_marbledray$fit + qnorm(0.975,0,1)*pred_marbledray$se.fit)

#Prepare Blacktip data
cocos$Blacktips[cocos$Blacktips<0]<-1 #change -1 to 1 to indicate presence
cocos$Blacktips<-as.numeric(cocos$Blacktips>0) #change to pres/abs only

blacktip_model<-glmmTMB(Blacktips~El_Nino_Strength, cocos, family = "binomial")
blacktip_model1<-glmmTMB(Blacktips~StudyYear+(1|StudyYear), cocos, family = "binomial")

#pick most precise year
pred_blacktip1<-predict(blacktip_model1, newdata = data.frame(StudyYear=c(0:27)), se.fit=T, type="response")
pred_frame_blacktip1 <- data.frame(StudyYear = c(0:27), 
                                    fit = pred_blacktip1$fit,
                                    error = pred_blacktip1$se.fit/pred_blacktip1$fit)

#get fits for each el nino strength level
pred_blacktip<-predict(blacktip_model, newdata = data.frame(El_Nino_Strength =dat),
  se.fit=T, type="response")
pred_frame_blacktip <- data.frame(El_Nino_Strength = dat, 
                                    fit = pred_blacktip$fit,
                                    ci.lower = pred_blacktip$fit - qnorm(0.975,0,1)*pred_blacktip$se.fit, 
                                    ci.upper = pred_blacktip$fit + qnorm(0.975,0,1)*pred_blacktip$se.fit)

#Prepare Tiger shark data
cocos_tiger<-subset(cocos, Year>=2006)
cocos_tiger$TigerSharks[cocos_tiger$TigerSharks<0]<-1 #change -1 to 1 to indicate presence
cocos_tiger$TigerSharks<-as.numeric(cocos_tiger$TigerSharks>0) #change to pres/abs only

tiger_model<-glmmTMB(TigerSharks~El_Nino_Strength, cocos_tiger, family = "binomial")
tiger_model1<-glmmTMB(TigerSharks~StudyYear+(1|StudyYear), cocos_tiger, family = "binomial")

#pick most precise year
pred_tiger1<-predict(tiger_model1, newdata = data.frame(StudyYear=c(0:27)), se.fit=T, type="response")
pred_frame_tiger1 <- data.frame(StudyYear = c(0:27), 
                                   fit = pred_tiger1$fit,
                                   error = pred_tiger1$se.fit/pred_tiger1$fit)

#get fits for each el nino strength level
pred_tiger<-predict(tiger_model, newdata = data.frame(El_Nino_Strength = dat),
                       se.fit=T, type="response")
pred_frame_tiger <- data.frame(El_Nino_Strength = dat, 
                    fit = pred_tiger$fit,
                    ci.lower = pred_tiger$fit - qnorm(0.975,0,1)*pred_tiger$se.fit, 
                    ci.upper = pred_tiger$fit + qnorm(0.975,0,1)*pred_tiger$se.fit)

par(mfrow=c(2,4), mar=c(3,5.6,2,3))

plot(pred_frame_tiger$fit~c(1:5), pch=16, bty="n",
     xaxt="n", ylab="", xlab="", las=2,  xlim=c(0.9,5.1), ylim=c(0,0.20),
     cex.lab=2, cex.axis=1.5, cex=2)
arrows(1:5, pred_frame_tiger$ci.lower, 1:5, pred_frame_tiger$ci.upper,angle=90,code=3, length =0.1,
  lwd=2)
axis(1, pos=0, labels = rep("", 5), at=1:5)
segments(-0.5,0,3.2,0, lty=1, lwd=1)
mtext("(a)", side=3, adj=0, cex=1.5)
mtext("Mean proportion of dives seen", side=2, line=4, cex=1.3)

plot(pred_frame_ham$fit~c(1:5), pch=16, bty="n",
     xaxt="n", ylab="", xlab="", las=2, xlim=c(0.9,5.1), ylim=c(10,70),
     cex.lab=2, cex.axis=1.5, cex=2)
arrows(1:5, pred_frame_ham$ci.lower, 1:5, pred_frame_ham$ci.upper,angle=90,code=3, length =0.1,
  lwd=2)
axis(1, pos=10, labels = rep("", 5), at=1:5)
segments(-0.5,10,3.2,10, lty=1, lwd=1)
mtext("(b)", side=3, adj=0, cex=1.5)
mtext("Mean count per dive", side=2, line=4, cex=1.3)

plot(pred_frame_blacktip$fit~c(1:5), pch=16, bty="n",
     xaxt="n", ylab="", xlab="", las=2, xlim=c(0.9,5.1), , ylim=c(0.02,0.09),
     cex.lab=2, cex.axis=1.5, cex=2)
arrows(1:5, pred_frame_blacktip$ci.lower, 1:5, pred_frame_blacktip$ci.upper,angle=90,code=3, length =0.1,
  lwd=2)
axis(1, pos=0.02, labels = rep("", 5), at=1:5)
segments(-0.5,0.02,3.2,0.02, lty=1, lwd=1)
mtext("(c)", side=3, adj=0, cex=1.5)
mtext("Mean proportion of dives seen", side=2, line=4, cex=1.3)

plot(pred_frame_white$fit~c(1:5), pch=16, bty="n",
     xaxt="n", ylab="", xlab="", las=2, xlim=c(0.9,5.1), ylim=c(20,40),
     cex.lab=2, cex=2, yaxt="n")
arrows(1:5, pred_frame_white$ci.lower, 1:5, pred_frame_white$ci.upper,angle=90,code=3, length =0.1,
  lwd=2)
axis(1, pos=20, labels = rep("",5), at=1:5, cex.axis=1.3)
mtext(levels, at=1:5, side=1, line=2, cex=1)
axis(2, labels = seq(20,40,5), at=seq(20,40,5), cex.axis=1.7, las=2)
segments(-0.5,20,3.2, 20, lty=1, lwd=1)
mtext("(d)", side=3, adj=0, cex=1.5)
mtext("Mean count per dive", side=2, line=4, cex=1.3)

plot(pred_frame_mobula$fit~c(1:5), pch=16, bty="n",
     xaxt="n", ylab="", xlab="", las=2, xlim=c(0.9,5.1), ylim=c(0.06,0.12),
     cex.lab=2, cex=2, yaxt="n")
arrows(1:5, pred_frame_mobula$ci.lower, 1:5, pred_frame_mobula$ci.upper,angle=90,code=3, length =0.1,
  lwd=2)
axis(1, pos=0.06, labels = rep("",5), at=1:5, cex.axis=1.3)
mtext(levels, at=1:5, side=1, line=2, cex=1)
axis(2, labels = c("0.06","0.07","0.08", "0.09", "0.10", "0.11", "0.12"), at=seq(0.06,0.12,0.01), cex.axis=1.7, las=2)
segments(-0.5,0.06,3.2, 0.06, lty=1, lwd=1)
mtext("(e)", side=3, adj=0, cex=1.5)
mtext("Mean proportion of dives seen", side=2, line=4, cex=1.3)

plot(pred_frame_eagleray$fit~c(1:5), pch=16, bty="n",
     xaxt="n", ylab="", xlab="", las=2, xlim=c(0.9,5.1), ylim=c(0.3,0.8),
     cex.lab=2, cex.axis=1.5, cex=2)
arrows(1:5, pred_frame_eagleray$ci.lower, 1:5, pred_frame_eagleray$ci.upper,angle=90,code=3, length =0.1,
  lwd=2)
axis(1, pos=0.3, labels = rep("",5), at=1:5, cex.axis=1.3)
mtext(levels, at=1:5, side=1, line=2, cex=1)
segments(-0.5,0.3,3.2, 0.3, lty=1, lwd=1)
mtext("(f)", side=3, adj=0, cex=1.5)
mtext("Mean count per dive", side=2, line=4, cex=1.3)

plot(pred_frame_marbledray$fit~c(1:5), pch=16, bty="n",
     xaxt="n", ylab="", xlab="", las=2, xlim=c(0.9,5.1), ylim=c(2,12),
     cex.lab=2, cex.axis=1.5, cex=2)
arrows(1:5, pred_frame_marbledray$ci.lower, 1:5, pred_frame_marbledray$ci.upper,angle=90,code=3, 
  length =0.1, lwd=2)
axis(1, pos=2, labels = rep("",5), at=1:5, cex.axis=1.3)
mtext(levels, at=1:5, side=1, line=2, cex=1)
segments(-0.5,2,3.2, 2, lty=1, lwd=1)
mtext("(g)", side=3, adj=0, cex=1.5)
mtext("Mean count per dive", side=2, line=4, cex=1.3)
