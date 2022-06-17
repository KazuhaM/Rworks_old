library(sf)
library(sp)
library(gstat)

help(variogram)
help(vgm)

#S1#
S1<-sf::st_read("S1_grid_2.shp")
plot(S1)
X<-(S1$left+S1$right)/2
Y<-(S1$top+S1$bottom)/2
Z<-S1$shrub
dat <- matrix(cbind(X,Y,Z),ncol=3)
colnames(dat)<-c("X","Y","Z")
dat<-data.frame(dat)
dat.coords<-cbind(dat$X,dat$Y)
dat<-SpatialPointsDataFrame(dat.coords,dat)
dat.vario <- variogram(Z~X+Y,dat)
plot(dat.vario,pch=1,cex=1.2)
spm.model<-vgm(psill=0.151,model="Sph",range=2.972,nugget=0.022)
plot(dat.vario,spm.model,xlim=c(0,15),ylim=c(0,0.27),main="Site8")
spm.fit<-fit.variogram(dat.vario,spm.model)
spm.fit

#異方性
dat.vario2<- variogram(Z~X+Y,dat,alpha=0:4*45)
dev.new()
plot(dat.vario2,pch=1,cex=1.2)
spm.model2<-vgm(psill=0.156,model="Sph",range=2.993,nugget=0.018,anis=c(0,0.3))
plot(dat.vario2,spm.model2,xlim=c(0,15),ylim=c(0,0.27),main="Site8")
spm.fit2<-fit.variogram(dat.vario2,spm.model2)
summary(spm.fit2)
spm.fit2
