library(sf)
library(sp)
library(gstat)
library(RColorBrewer)
library(tcltk)

# color palette
cols <- brewer.pal(9, "YlOrRd")
cols <- c("#FFFFFF", cols, "#000000")

# set working directory
setwd("D:/SpAr")

# data load
site.name <- "W2_3"
sp_shp<-sf::st_read(paste("21forVariogram/var_", site.name, ".shp", sep = ""))
# plot(sp_shp)

# make coordinate data as the center of each mesh
d.x<- (sp_shp$left+sp_shp$right)*20/5400/2
d.y<- 20 + (sp_shp$top+sp_shp$bottom)*20/5400/2
d.z<- sp_shp$area / 725.805 / 100

# make spatial point data frame
d <- data.frame("X" = d.x, "Y" = d.y, "Z" = d.z)
d.coords<-cbind(d$X,d$Y)
d<-SpatialPointsDataFrame(d.coords,d)
# plot(d, pch = 1, cex = 0.5, col = cols[round(d$Z * 10,0) + 1] )

# variogram
d.vari <- variogram(Z~X+Y,d)
plot(d.vari,pch=1,cex=1.2, xlim = c(0, 9))
spm.model<-vgm(psill=0.09616594   ,model="Exp",range=0.6482628, nugget=0.000) #W2_3
# spm.model<-vgm(psill=0.01467778,model="Exp",range=0.2586829, nugget=0.000) # S1_1

plot(d.vari,spm.model,main="")
spm.fit<-fit.variogram(d.vari,spm.model)
# summary(spm.fit)
spm.fit
