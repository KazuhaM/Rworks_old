library(sf)
library(sp)
library(gstat)
library(RColorBrewer)
library(tcltk)

################## functions #####################
variogram.function <- function(x,model,xsill,xrange,xnugget){
  if (model == "Exp") {
    if(0 < x){
      y <- xnugget + xsill * (1 - exp(-x / xrange))
    }else if(x == 0){
      y <- xnugget
    }else{
      y <- "ERROR"
    }
  }else if (model == "Sph") {
    if(x > xrange){
      y <- xsill
    }else if(0 < x & x <= xrange ){
      y <- xnugget + xsill * (1.5 * x / xrange - 0.5 * (x / xrange)^3)
    }else if(x == 0){
      y <- xnugget
    }else{
      y <- "ERROR"
    }
  }
  
  return(y)
}

variogram.prediction <- function(x,model,xsill,xrange,xnugget){
  y <- numeric(length(x))
  for (i in 1:length(x)) {
    y[i] <- variogram.function(x[i],model,xsill,xrange,xnugget)
  }
  return(y)
}

####################################################

# color palette
cols <- brewer.pal(9, "YlOrRd")
cols <- c("#FFFFFF", cols, "#000000")

# set working directory
setwd("D:/SpAr")

# sites file
sites <- read.csv("Sites_var.csv", header = T)

result.all <- data.frame("model" = "base", 
                        "sill" = 0.0,
                        "range" = 0.0, 
                        "nugget" = 0.0,
                         "SiteID"="base")
# data load
for (i_site in 1:nrow(sites)) {
  
  # result <- data.frame("model" = "base", 
  #                      "sill" = 0.0,
  #                      "range" = 0.0, 
  #                      "nugget" = 0.0)
# site.name <- "W2_3"
  site.name <- sites[i_site,1]
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
  
  print(paste(site.name,"    ", i_site,"/",nrow(sites),sep = ""))
  # cul variogram as a isotropy for graph lim
  d.vari <- variogram(Z~X+Y,d)
  spm.model<-vgm(psill=max(d.vari$gamma)*0.4  ,model="Exp",
                 range=0.6, nugget=1.0 * 10^(-6)) #W2_3
  spm.fit<-fit.variogram(d.vari, spm.model)
    
  # for (i_re in 1:100) {
  #   spm.model<-vgm(psill=spm.fit$psill[2]  ,
  #                        model=spm.fit$model[2],
  #                        range=spm.fit$range[2],
  #                        nugget=spm.fit$psill[1]) #W2_3
  #   spm.fit<-fit.variogram(d.vari, spm.model)
  # }
  plot(d.vari$dist,d.vari$gamma,
       xlim = c(0, max(d.vari$dist)*1.1),
       ylim = c(0,max(d.vari$gamma)*1.1),
       cex.axis=1.2, cex.lab=1.5,
       ylab = "variogram",xlab = "distance (m)", cex = 1.2)
  par(new = T)
  plot(seq(0, max(d.vari$dist)*1.1,0.1),
       variogram.prediction(seq(0, max(d.vari$dist)*1.1,0.1),
                            model = spm.fit$model[2],
                            xsill = spm.fit$psill[2],
                            xrange = spm.fit$range[2],
                            xnugget = spm.fit$psill[1]),
       xlim = c(0, max(d.vari$dist)*1.1),
       ylim = c(0,max(d.vari$gamma)*1.1),
       type = "l", ylab = "", xlab = "", yaxt="n", xaxt = "n")
  dev.copy(pdf,
           file=paste("21forVariogram/var_", site.name, ".pdf", sep = ""),
           width = 20, height = 10)
  dev.off()

  # plot(sill.conv[-1],type = "l", )
  
  result<- data.frame("model" =spm.fit$model[2], 
                       "sill" = spm.fit$psill[2],
                       "range" = spm.fit$range[2], 
                       "nugget" = spm.fit$psill[1])
  # result <- result[-1,]
  result <- transform(result,"SiteID" = site.name)
  
  result.all <- rbind(result.all, result)
}
result.all <- result.all[-1,]
write.csv(result.all, "21forVariogram/var_allresult.csv")


##########################################################################################
# for (i_plt in 1:nrow(result)) {
#   spm.model.aniso<-vgm(psill=result[i_plt,"sill"]  ,
#                        model=result[i_plt,"model"]  ,
#                        range=result[i_plt,"range"]  ,
#                        nugget=result[i_plt,"nugget"]  ) 
#   d.aniso<- variogram(Z~X+Y,d,alpha=dir.list[i_plt], tol.hor = 90/dir.len)
#   plot(d.aniso,spm.model.aniso)
  # dev.copy(pdf,
  #          file=paste("21forVariogram/var_", site.name, "_", dir.list[i_plt] , ".pdf", sep = ""),
  #          width = 10, height = 10)
#   dev.off()
# }



# d.aniso<- variogram(Z~X+Y,d,alpha=dir.list[i], tol.hor = 90/8)
# plot(d.aniso,pch=1,cex=1.2)
# spm.model.aniso<-vgm(psill=0.09617213     ,model="Exp",range=0.6486082     , nugget=0.000,anis=c(0,0.9)) #W2_3
# # spm.model.aniso<-vgm(psill=0.01458745 ,model="Exp",range=0.2535 , nugget=0.000,anis=c(0,0.9)) # S1_1
# 
# plot(d.aniso,spm.model.aniso)
# spm.fit.aniso<-fit.variogram(d.aniso, spm.model.aniso)
# summary(spm.fit.aniso)
# spm.fit.aniso


