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
      y <- xnugget + xsill
    }else if(0 < x & x <= xrange ){
      y <- xnugget + xsill * (1.5 * x / xrange - 0.5 * x^3 / xrange)
    }else if(x == 0){
      y <- xnugget 
    }else{
      y <- "ERROR"
    }
  }else if (model == "Gau") {
    if(0 < x){
      y <- xnugget + xsill * (1 - exp(-x^2 / xrange^2))
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
initval <- read.csv("22forVariogram500/aniso500g_InitValue.csv",header = T)
i_init <- 1

result.all <- data.frame("model" = "base", 
                        "sill" = 0.0,
                        "range" = 0.0, 
                        "nugget" = 0.0,
                        "degree" = 0.0,
                         "SiteID"="base")
# data load
for (i_site in 1:nrow(sites)) {

# site.name <- "W2_3"
  site.name <- sites[i_site,1]
  sp_shp<-sf::st_read(paste("22forVariogram500/500g_", site.name, ".shp", sep = ""))
  # plot(sp_shp)
  # sp_shp[1,] + sp_shp[2,]
  
  # make coordinate data as the center of each mesh
  d.x<- (sp_shp$left+sp_shp$right)*20/5400/2
  d.y<- 20 + (sp_shp$top+sp_shp$bottom)*20/5400/2
  d.z<- sp_shp[,grep("_p",colnames(sp_shp))]  * 0.5 ^ 2
  d.z <- d.z[,1]
  
  # make spatial point data frame
  d <- data.frame("X" = d.x, "Y" = d.y, "Z" = d.z)
  d.coords<-cbind(d$X,d$Y)
  d<-SpatialPointsDataFrame(d.coords,d)
  # plot(d, pch = 1, cex = 0.5, col = cols[round(d$Z * 10,0) + 1] )
  
  # cul variogram as a isotropy for graph lim
  d.vari <- variogram(Z~X+Y,d)
  
  #anisotropy
  dir.len <- 8
  
  dir.list <- 0:(dir.len*2) * (90/dir.len)
  dir.list <- dir.list[0:(dir.len-1)*2+1]
  
  result <- data.frame("model" = "base", 
                       "sill" = 0.0,
                       "range" = 0.0, 
                       "nugget" = 0.0,
                       "degree" = 0.0)
  
  print(paste(site.name,"    ", i_site,"/",nrow(sites),sep = ""))
  pb <- txtProgressBar(min = 1, max = dir.len, style = 3)
  # i_dir <- 1 # 1:dir.len (8)
  for (i_dir in 1:dir.len) {
    i_model = initval[i_init,"model"]
    i_sill = initval[i_init,"sill"]
    i_range = initval[i_init,"range"]
    i_nugget = initval[i_init,"nugget"]
    i_onoff = initval[i_init,"OnOff"]
    
    sill.conv <- c(0)
    # print(i_dir)
    d.aniso<- variogram(Z~X+Y,d,alpha=dir.list[i_dir], tol.hor = 90/dir.len,width=0.4)
    plot(d.aniso)
    spm.model.aniso<-vgm(psill=i_sill ,
                         model=i_model, range= i_range, nugget= i_nugget) 
    
    #########手作業init value 探索用 
    spm.model.aniso<-vgm(psill=40,model="Exp",range=2, nugget=5)
    plot(d.aniso,spm.model.aniso)
    spm.fit.aniso<-fit.variogram(d.aniso, spm.model.aniso)
    plot(d.aniso,spm.fit.aniso)
    spm.fit.aniso
    i_dir
    i_site

    ##############################
    
    if (is.na(i_onoff)) {
      spm.fit.aniso<-fit.variogram(d.aniso, spm.model.aniso)
    }else if (i_onoff == 1){
      spm.fit.aniso<- vgm(psill=i_sill  ,
                          model=i_model, range= i_range, nugget= i_nugget)
    }
    
    
    # spm.fit.aniso
    # for (i_re in 1:100) {
    #   spm.model.aniso<-vgm(psill=spm.fit.aniso$psill[2]  ,
    #                        model=spm.fit.aniso$model[2],
    #                        range=spm.fit.aniso$range[2],
    #                        nugget=spm.fit.aniso$psill[1]) #W2_3
    #   spm.fit.aniso<-fit.variogram(d.aniso, spm.model.aniso)
    #   sill.conv <- c(sill.conv,spm.fit.aniso$psill[2] )
    # }
    sill.conv <- sill.conv[-1]
    # plot(sill.conv)
    # plot(d.aniso,spm.model.aniso)
    # plot(d.aniso,spm.fit.aniso)
    
    if(i_dir == 1){
      par(mfrow=c(2,4))
    }
    plot(d.aniso$dist,d.aniso$gamma,
         xlim = c(0, max(d.vari$dist)*1.1),
         ylim = c(0,max(d.vari$gamma)*1.1),
         cex.axis=1.2, cex.lab=1.5,
         ylab = "variogram",xlab = "distance (m)", cex = 1.2,
         main = dir.list[i_dir])
    par(new = T)
    plot(seq(0, max(d.vari$dist)*1.1,0.1),
         variogram.prediction(seq(0, max(d.vari$dist)*1.1,0.1),
                              model = spm.fit.aniso$model[2],
                              xsill = spm.fit.aniso$psill[2],
                              xrange = spm.fit.aniso$range[2],
                              xnugget = spm.fit.aniso$psill[1]),
         xlim = c(0, max(d.vari$dist)*1.1),
         ylim = c(0,max(d.vari$gamma)*1.1),
         type = "l", ylab = "", xlab = "", yaxt="n", xaxt = "n")
    if (i_dir == dir.len) {
      dev.copy(pdf,
               file=paste("22forVariogram500/aniso500g_", site.name, ".pdf", sep = ""),
               width = 20, height = 10)
      dev.off()
    }
    
    # plot(sill.conv[-1],type = "l", )
    
    result.temp <- data.frame("model" =spm.fit.aniso$model[2], 
                         "sill" = spm.fit.aniso$psill[2],
                         "range" = spm.fit.aniso$range[2], 
                         "nugget" = spm.fit.aniso$psill[1],
                         "degree" = dir.list[i_dir])
    result <- rbind(result, result.temp)
    i_init <- i_init + 1
    setTxtProgressBar(pb, i_dir)
  }
  result <- result[-1,]
  result <- transform(result,"SiteID" = site.name)
  
  result.all <- rbind(result.all, result)
}
result.all <- result.all[-1,]
write.csv(result.all, "22forVariogram500/aniso500g_allresult2.csv")


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


