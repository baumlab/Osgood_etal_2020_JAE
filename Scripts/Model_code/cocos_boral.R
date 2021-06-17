#Created by Geoffrey Osgood on February 1, 2020 to run boral models on Cocos data to study El Nino effects

library(boral)

setwd("c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/")
setwd("~/Documents/git_repos/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019


cocos<-subset(cocos, Hammerheads>=0) #remove -1's which indicate presence only
cocos<-subset(cocos, Whitetips>=0) #remove -1's which indicate presence only
cocos<-subset(cocos, MantaRays>=0) #remove -1's which indicate presence only
cocos<-subset(cocos, EagleRays>=0) #remove -1's which indicate presence only
cocos<-subset(cocos, Blacktips>=0) #remove -1's which indicate presence only

#Remove NAs in Visibility
cocos<-subset(cocos, !is.na(Visibility))

#Turn fixed and random effects into factors if necessary
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)
cocos$ID <- as.factor(cocos$ID)


#set up env
env<-cocos[,c("SST", "ONI", "StudyYear", "Visibility", "COS_TIME")]

sharks<-cocos[,c("Hammerheads", "Blacktips", "Whitetips", "MantaRays", "EagleRays", "MarbledRays")]

#boral models
boral.out.latent<-boral(sharks, X=env, row.eff = "none",
  family="poisson", lv.control=list(num.lv=2), save.model=T, prior.control=list(ssvs.index=0))

save(boral.out.latent, file="boral_cocos_latent.Rdata")
 
 #Variance partitioning 

summary(boral.out) #Parameter estimates, model information

#Residual analysis
par(mfrow=c(2,2))
plot(boral.out) 
lvsplot(boral.out)
get.enviro.cor(boral.out) #correlations due to environment 
#get.residual.cor(boral.out) # residual correlations (ie species interactions)
#ssvs.index argument check it out (stochastic search variable selection)

#Latent variables determine site location, coefficients of latent variables determine species locations on biplot
names(env)
var.out<-calc.varpart(boral.out)
data.frame(Source=names(env),
  AvgPercentImportance=rowSums(var.out)/17)

par(mfrow=c(1,2), mar=c(6,15,1,1))
coefsplot("SST", boral.out)
par(mar=c(6,1,1,15))
coefsplot("ONI", boral.out, yaxt="n")

envcors <- get.enviro.cor(boral.out)

rescors <- get.residual.cor(boral.out)

library(corrplot)

corrplot(envcors$sig.cor, type = _lower",
diag = FALSE,
title = _Correlations due to covariates",
mar = c(3,0.5,2,1), tl.srt = 45)
corrplot(rescors$sig.cor, type = _lower",
diag = FALSE,
title = _Residual correlations",
mar = c(3,0.5,2,1), tl.srt = 45)

rescors$trace

############################
###Plot#####################
############################
biplot.out_sharks<-lvsplot(boral.out_sharks_latent_roweff, biplot=TRUE, est = "mean", alpha=0.6, 
	return.val=TRUE)

head(boral.out_sharks_latent_roweff)

##Figure 4b - boral ordination
par(mar=c(5,6,5,5))
plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
#mtext("(a)", side=3, line=0.5, adj=0, cex=2)
plot(biplot.out_sharks$scaled.lvs, col=color_ellipse_shark[env$Habitat], pch=16, 
     ylab="Latent variable 2", xlab="Latent variable 1", 
     cex.axis=1.5, cex.lab=2, xlim=c(-2,2), ylim=c(-2.5,2))
for (i in unique(as.numeric(env$Habitat))) ordiellipse (biplot.out_sharks$scaled.lvs[,], 
                                                        groups = as.numeric(env$Habitat), show.group = i, 
                                                        col = color_ellipse_shark[i], 
                                                        kind="sd", label = F, lwd=4)
text(biplot.out_sharks$scaled.lv.coefs+0.01, labels=c("BS", "BG", "BW", "DS", "ER", "LC", "LG", "PS",
            "PJ","SR", "SD", "SH", "HH",  "SF", "SN", "SG", "SJ", "TC"), cex=1.6,
col=c("#0072B2"))
legend(-2, 2, c("Sand", "Reef", "Kelp"), col=c("#000000", "#e79f00", "#009E73"), pch=16, cex=1.5)
#mtext("(b)", side=3, line=0.5, adj=0, cex=2)
