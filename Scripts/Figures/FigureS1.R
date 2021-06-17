#this creates a figure for the Cocos Paper that describes the Cocos data itsel
#12 species plots

#call in data
cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019

#Remove NAs in Visibility
cocos<-subset(cocos, !is.na(Visibility))

#Turn fixed and random effects into factors if necessary
cocos$SiteCode <- as.factor(cocos$SiteCode)
cocos$CurrentCode <- as.factor(cocos$CurrentCode)
cocos$DiverCode <- as.factor(cocos$DiverCode)
cocos$ID <- as.factor(cocos$ID)

nf<-layout(matrix(c(1,1,2,3), 2, 2, byrow=TRUE),respect=T)

par(oma=c(2,2,0,1))


o<-par(mar=c(2,2,1,0),mgp=c(3,0.6,0))
dives_per_year=data.frame(table(CocosData$StudyYear))
dives_per_year$Var1=1993:2019
fig1=barplot(dives_per_year$Freq,names.arg=dives_per_year$Var1,las=2,
	ylab='',xlab='',ylim=c(0,2500),border=NA,axisnames=F,cex.axis=0.8)
	mtext('Number of dives',2,line=2.5,font=2,cex=0.8)
	
	#o<-par(mgp=c(3,0.6,0)) 
	axis(1, at=fig1[1:27], labels=1993:2019, las=2,cex.axis=1)	
	box()
	
	mtext('(a)',side=3,line=-1.1,adj=0.01,font=2,cex=0.8)



##############################################
o<-par(mar=c(3,0,1,0.5),mgp=c(3,1,0))

	dives_per_diver_year=table(CocosData$DiverCode,CocosData$StudyYear)
	colnames(dives_per_diver_year)=1993:2019
	dives_per_diver_year[dives_per_diver_year>0]=1
	dives_per_diver_year[dives_per_diver_year==0]=0
	
	
	plot(colnames(dives_per_diver_year),dives_per_diver_year[1,],type='n',
		yaxt='n',xaxt='n',ylab='',xlab='',las = 1, ylim=c(3,46.5),pch=16)
	
	 abline(h=2:47,col='grey')	
	
	for (z in 1:nrow(dives_per_diver_year)){
		points(colnames(dives_per_diver_year),(dives_per_diver_year[z,]+dives_per_diver_year[z,]*z),
			ylim=c(1,51),pch=16)
	}
	
o<-par(mgp=c(3,0.2,0))
	 axis(2, at=1:nrow(dives_per_diver_year)+1, 
	 	labels=rownames(dives_per_diver_year), tick=F,las=1,cex.axis=0.40)

	 
o<-par(mgp=c(3,0.6,0)) 
	axis(1, at=seq(1993,2019,by=2), labels=seq(1993,2019,by=2), tick=T,las=2,cex.axis=1)
	
	mtext('Year',1,line=2.5,font=2,cex=0.8)
	mtext('Divemaster ID',2,line=1,font=2,cex=0.8)
	
	mtext('(b)',side=3,line=-1.1,adj=0.01,font=2,cex=0.8)
	
##################################################
o<-par(mar=c(3,0,1,0),mgp=c(3,1,0))
	dives_per_site_diver=table(CocosData$DiverCode,CocosData$SiteCode)
	dives_per_site_diver[dives_per_site_diver>0]=1
	dives_per_site_diver[dives_per_site_diver==0]=0
	
	plot(20,46,type='n',yaxt='n',xaxt='n',ylab='',xlab='',las = 1, ylim=c(3,46.5),
		xlim=c(0,17),pch=16)
		 
	for (j in 2:47){
	segments(-1,j,18,j,col='grey')
		} 
		 
	for (z in 1:nrow(dives_per_site_diver)){
		points(1:17,(dives_per_site_diver[z,]+dives_per_site_diver[z,]*z),ylim=c(1,40),pch=16)
	}
	
	

	#axis(2, at=1:nrow(dives_per_site_diver), labels=rownames(dives_per_site_diver), tick=F,las=1,cex.axis=0.60,hadj=0.2)
	axis(1, at=1:ncol(dives_per_site_diver), labels=colnames(dives_per_site_diver), 
		tick=T,las=2,cex.axis=1,hadj=0.8)
	
		mtext('Site ID',1,line=2.5,font=2,cex=0.8)

	mtext('(c)',side=3,line=-1.1,adj=0.01,font=2,cex=0.8)