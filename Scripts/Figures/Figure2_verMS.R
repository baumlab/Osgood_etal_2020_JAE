#This script is for creating Figure 2 of Osgood et al. 2020
#created by Geoffrey J. Osgood
#

library(glmmTMB)
library(mgcv)

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

#############################################
##Prepare data for figure####################
#############################################

#list of models and species
modelsSST<-list(TigerSharks.cubic.short.YRE, hammerhead.linear.YRE, MobulaRay.linear.YRE,
	Blacktip.linear.YRE, MarbledRay.linear.YRE, 
	Whitetip.linear.YRE, EagleRay.quad.YRE)
	 #SST and ONI models


#create empty data frames to save results of below for loops
SST<-NULL
SST_CI_u<-NULL
SST_CI_l<-NULL

ONI<-NULL
ONI_CI_u<-NULL
ONI_CI_l<-NULL


#for CI's - quantile normal distribution
n<-qnorm(0.975, 0, 1)
#for loop to get coefficients and SE for SST and ONI effects for this list of models
for (i in 1:length(modelsSST)) {
SST[i]<-summary(modelsSST[[i]])$coefficients$cond[3,1]
SST_CI_l[i]<-SST[i]-summary(modelsSST[[i]])$coefficients$cond[3,2]*n
SST_CI_u[i]<-SST[i]+summary(modelsSST[[i]])$coefficients$cond[3,2]*n

ONI[i]<-summary(modelsSST[[i]])$coefficients$cond[4,1]
ONI_CI_l[i]<-ONI[i]-summary(modelsSST[[i]])$coefficients$cond[4,2]*n
ONI_CI_u[i]<-ONI[i]+summary(modelsSST[[i]])$coefficients$cond[4,2]*n

}

#quadratic term for Eagle Rays and cubic for Tiger Sharks
ONI<-c(ONI,summary(EagleRay.quad.YRE)$coefficients$cond[5,1])
ONI_CI_l<-c(ONI_CI_l, (ONI[8]-summary(EagleRay.quad.YRE)$coefficients$cond[5,2]*n))
ONI_CI_u<-c(ONI_CI_u, (ONI[8]+summary(EagleRay.quad.YRE)$coefficients$cond[5,2]*n))

ONI<-c(ONI,summary(TigerSharks.cubic.short.YRE)$coefficients$cond[5,1], 
	summary(TigerSharks.cubic.short.YRE)$coefficients$cond[6,1])
ONI_CI_l<-c(ONI_CI_l, (ONI[9]-summary(TigerSharks.cubic.short.YRE)$coefficients$cond[5,2]*n))
ONI_CI_l<-c(ONI_CI_l, (ONI[10]-summary(TigerSharks.cubic.short.YRE)$coefficients$cond[6,2]*n))
ONI_CI_u<-c(ONI_CI_u, (ONI[9]+summary(TigerSharks.cubic.short.YRE)$coefficients$cond[5,2]*n))
ONI_CI_u<-c(ONI_CI_u, (ONI[10]+summary(TigerSharks.cubic.short.YRE)$coefficients$cond[6,2]*n))

#Reorder based on mobility
ONI<-ONI[c(1,9,10,2:4,7:8,5:6)]
SST<-SST[c(1:4,7,5:6)]

ONI_CI_l<-ONI_CI_l[c(1,9,10,2:4,7:8,5:6)]
ONI_CI_u<-ONI_CI_u[c(1,9,10,2:4,7:8,5:6)]

SST_CI_l<-SST_CI_l[c(1:4,7,5:6)]
SST_CI_u<-SST_CI_u[c(1:4,7,5:6)]

Species=c("Tiger shark", "Scalloped hammerhead", "Mobula ray", "Blacktip shark", 
	"Eagle ray", "Marbled ray", "Whitetip reef shark")

Species_ONI=c("Tiger shark", "Tiger shark quadratic term", "Tiger shark cubic term", "Scalloped hammerhead", "Mobula ray", "Blacktip shark", "Eagle ray",
	"Eagle ray quadratic term", "Marbled ray", "Whitetip reef shark")

Species_legend=expression(italic("Galeocerdo cuvier"), 
		paste(italic("Galeocerdo cuvier"), " quadratic"), 
		paste(italic("Galeocerdo cuvier"), " cubic"), 
		italic("Sphyrna lewini"), 
		paste(italic("Mobula"), " spp."), 
		italic("Carcharhinus limbatus"), 
		italic("Aetobatus narinari"),
		paste(italic("Aetobatus narinari"), " quadratic"),
		 italic("Taeniurops meyeni"), 
		 italic("Triaenodon obesus"))


#Put it all together into data frames
SST_data<-data.frame(Species=as.character(Species), SST=SST, Lower=SST_CI_l, Upper=SST_CI_u)
ONI_data<-data.frame(Species=as.character(Species_ONI),ONI=ONI, Lower=ONI_CI_l, Upper=ONI_CI_u)

write.csv(ONI_data, "ONI_data.csv")

#######################################################
#figure 2##############################################
#######################################################

pdf(file="c:/Users/gjosg/Dropbox/Osgood_et_al_CocosElNino/Manuscript/Figures/Figure_2_revised.pdf", 
  width = 8, height = 7.08661)

plot(10:1~ONI, ONI_data, pch=c(16,17,18, rep(16,4), 17, 16), col=c(1,1,1,2:4,5,5,6, "gold"), bty="n", 
     yaxs="i", yaxt="n", ylab="", xlab="Coefficient", xlim=c(-0.8,0.6), ylim=c(0.5,18.7), 
     cex.axis=1.7, cex.lab=1.7, cex=2.2)
arrows(ONI_data$Lower, 10:1, ONI_data$Upper, 10:1,angle=90,code=3, length =0.1, 
	col=c(1,1,1,2:4,5,5,6, "gold"), lwd=2)
points(18.2:12.2~SST, SST_data, pch=16, col=c(1:6,"gold"), cex=2.2)
arrows(SST_data$Lower, 18.2:12.2, SST_data$Upper, 18.2:12.2,angle=90,code=3,
 length =0.1, col=c(1:6,"gold"), lwd=2)
segments(0,0.5,0,18.7, lty=2)
segments(-0.82,0.5,-0.82,18.7, lty=1)
axis(2, labels=c("ONI", "SST"), at=c(6, 15), las=2, pos=-0.82, cex.axis=1.5)
segments(-0.82,11,0.6,11, lty=4)
legend(0.27, 10, Species_legend, col=c(1,1,1,2:4,5,5,6,"gold"), 
	pch=c(16,17,18, rep(16,3),17, rep(16,2)), bty="n", cex=0.7)

dev.off()
