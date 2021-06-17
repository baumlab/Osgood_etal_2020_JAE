#################################
####Plots of predicted vs fitted#
#################################

setwd("c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/")

#######################################################
#load in models########################################
#######################################################
load(file="Models/hammerhead_linear_yre.Rdata")

load(file="Models/MobulaRay_linear_yre.Rdata")

load(file="Models/MarbledRay_linear_yre.Rdata")

load(file="Models/EagleRay_quad_yre.Rdata")

load(file="Models/Whitetip_linear_yre.Rdata")

load(file="Models/Blacktip_linear_yre.Rdata")

load(file="Models/TigerSharks_cubic_short_yre.Rdata")


#predictions from the models
tiger.predict<-predict(TigerSharks.cubic.short.YRE, type="response")
hammerhead.predict<-predict(hammerhead.linear.YRE, type="response")
blacktip.predict<-predict(Blacktip.linear.YRE, type="response")
whitetip.predict<-predict(Whitetip.linear.YRE, type="response")
mobula.predict<-predict(MobulaRay.linear.YRE, type="response")
eagleray.predict<-predict(EagleRay.quad.YRE, type="response")
marbledray.predict<-predict(MarbledRay.linear.YRE, type="response")


par(mfrow=c(3,3))

plot(tiger.predict~factor(TigerSharks.cubic.short.YRE$frame$TigerSharks), xlab="Observed", ylab="predicted")
mtext("a) Tiger sharks", side=3, adj=0, line=0.5)
plot(hammerhead.predict~hammerhead.linear.YRE$frame$Hammerheads, xlab="Observed", ylab="predicted")
abline(0,1, col="red")
mtext("b) Scalloped Hammerhead Sharks", side=3, adj=0, line=0.5)
plot(blacktip.predict~factor(Blacktip.linear.YRE$frame$Blacktips), xlab="Observed", ylab="predicted")
mtext("c) Blacktip Reef Sharks", side=3, adj=0, line=0.5)
plot(whitetip.predict~Whitetip.linear.YRE$frame$Whitetips, xlab="Observed", ylab="predicted")
abline(0,1, col="red")
mtext("d) Whitetip Reef Sharks", side=3, adj=0, line=0.5)
plot(mobula.predict~factor(MobulaRay.linear.YRE$frame$MobulaRays), xlab="Observed", ylab="predicted")
mtext("e) Mobula Rays", side=3, adj=0, line=0.5)
plot(eagleray.predict~EagleRay.quad.YRE$frame$EagleRays, xlab="Observed", ylab="predicted")
abline(0,1, col="red")
mtext("f) Eagle Rays", side=3, adj=0, line=0.5)
plot(marbledray.predict~MarbledRay.linear.YRE$frame$MarbledRays, xlab="Observed", ylab="predicted")
abline(0,1, col="red")
mtext("g) Marbled Rays", side=3, adj=0, line=0.5)
