###Figure 1 - Map of study site


library(maptools)
library(maps)
library(PBSmapping)
library(mapproj)
library(sp)

#Import shape files

#Import shapefiles
Cocos<-importShapefile("c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/Shapefiles/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
proj.abbr<-attr(Cocos, "projection")
proj.full<-attr(Cocos, "prj")
ylim=c(2.5,13)
xlim=c(-90,-74.5)

#Import Cocos shape files

Island<-importShapefile("c:/Users/shark_000/Documents/git_repos/Cocos_El_Nino/Shapefiles/ne_10m_reefs/ne_10m_reefs.shp")
proj.abbr.island<-attr(Island, "projection")
proj.full.island<-attr(Island, "prj")
ylim_island=c(5.475,5.575)
xlim_island=c(-87.125,-87.025)

#Large map from: https://www.naturalearthdata.com/downloads/50m-cultural-vectors/
#v <- c(0.68, 0.94, 0.48, 1)


#par(fig = c(0,1,0,1), oma=c(0.5,0.5,0.5,1))

plotPolys(Cocos, projection=proj.abbr, 
          xlab="", ylab="", bty="n", axes=F, cex=1, col="grey" , ylim=ylim, xlim=xlim)
compassRose(-79.3, 11.7, cex=1)
points(-87, 5.5, pch=16, cex=5, lwd=2)
text(-84.1, 9.9, expression(italic("Costa\nRica")), cex=2)
text(-87, 6, expression(italic("Cocos Island")), cex=2)
box(lwd=2)


plotPolys(Island, projection=proj.abbr.island,  , 
          xlab="", ylab="", bty="n", axes=F, cex=1, col="grey", 
          ylim=ylim_island, xlim=xlim_island) 
axis(1, at=seq(xlim_island[1], xlim_island[2], by=0.025),  cex.axis=2)
axis(2, at=seq(ylim_island[1], ylim_island[2], by=0.025), cex.axis=2, las=2)
box(lwd=2)
mtext("Latitude", side=2, line=7, cex=2)
mtext("Longitude", side=1, line=3, cex=2)
map.scale(x=-87.075, y=5.485, ratio=FALSE, relwidth=0.3, cex=2)







par(fig=v, new=TRUE, mar=c(0,0,0,0))
# map of africa 
plotPolys(AfricaClip, projection=proj.abbrAfrica, bty="n", 
	ylim=ylimAfrica, axes=F, xlab="", ylab="", col="white")
points(18.89, -34.352, pch=0, cex=2, lwd=2)
text(23.3, -31.5, expression(italic("South Africa")), cex=0.6)
dev.off()
#d

