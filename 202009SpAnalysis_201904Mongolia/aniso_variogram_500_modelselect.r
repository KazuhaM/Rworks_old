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
      y <- xnugget + xsill * (1.5 * x / xrange - 0.5 * (x / xrange)^3)
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
  }else if (model == "Lin") {
    if(x > xrange){
      y <- xnugget + xsill
    }else if(0 < x & x <= xrange ){
      y <- xnugget + xsill * (x / xrange)
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

# azimuth that asio-variogram should be culculated with
asiodata <- read.csv("22forVariogram500/aniso500g_InitValue2.csv",header =  T)

# sites file
# sites <- read.csv("Sites_var.csv", header = T)
# initval <- read.csv("22forVariogram500/aniso500g_InitValue.csv",header = T)
# i_init <- 1

# site dir
# sitedir <- read.csv("22forVariogram500/SiteDir.csv",header = T)
# # site順一致チェック
# if (sum((sitedir[,1] == sites)==FALSE) != 0) {
#   stop("The site order is invalid, so the script must end here")
# }

result.all <- data.frame("model" = "base", 
                        "sill" = 0.0,
                        "range" = 0.0, 
                        "nugget" = 0.0,
                        "degree" = 0.0,
                         "SiteID"="base")
# data load
pb <- txtProgressBar(min = 1, max = nrow(asiodata), style = 3)
for (i_site in 1:nrow(asiodata)) {
  
  
# site.name <- "W2_3"
  site.name <- asiodata[i_site,1]
  asio_culdir <- asiodata[i_site,5]
  
  # for init value
  i_model = asiodata[i_site,"model"]
  i_sill = asiodata[i_site,"sill"]
  i_range = asiodata[i_site,"range"]
  i_nugget = asiodata[i_site,"nugget"]
  i_onoff = asiodata[i_site,"OnOff"]
  
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
  # 
  # dir.list <- 0:(dir.len*2) * (90/dir.len)
  # dir.list <- dir.list[0:(dir.len-1)*2+1]
  
  result <- data.frame("model" = "base", 
                       "sill" = 0.0,
                       "range" = 0.0, 
                       "nugget" = 0.0,
                       "degree" = 0.0)
  
  print(paste(site.name,"    ", i_site,"/",nrow(asiodata),sep = ""))
  
  sill.conv <- c(0)
  # print(i_dir)
  d.aniso<- variogram(Z~X+Y,d,alpha=asio_culdir,
                      tol.hor = 90/dir.len,width=0.4)
  #各モデル計算
  #指数
  spm.model.aniso.ex<-vgm(psill=i_sill ,
                    model="Exp", range= i_range, nugget= i_nugget) 
  spm.fit.ex<-fit.variogram(d.aniso, spm.model.aniso.ex)
  
  # 球
  spm.model.aniso.sp<-vgm(psill=i_sill ,
                          model="Sph", range= i_range, nugget= i_nugget) 
  spm.fit.sp<-fit.variogram(d.aniso, spm.model.aniso.sp)
  
  # 線形
  spm.model.aniso.ln<-vgm(psill=i_sill ,
                          model="Lin", range= i_range, nugget= i_nugget)
  spm.fit.ln<-fit.variogram(d.aniso, spm.model.aniso.ln)
  
  # ガウス
  spm.model.aniso.ga<-vgm(psill=i_sill ,
                          model="Gau", range= i_range, nugget= i_nugget) 
  spm.fit.ga<-fit.variogram(d.aniso, spm.model.aniso.ga)

  # 描画
  par(mfrow=c(2,2))
  # 指数モデル
  plot(d.aniso$dist,d.aniso$gamma,
       xlim = c(0, max(d.vari$dist)*1.1),
       ylim = c(0,max(d.vari$gamma)*1.1),
       cex.axis=1.2, cex.lab=1.5,
       ylab = "variogram",xlab = "distance (m)", cex = 1.2,
       main = paste(site.name,"_",asiodata$Event[i_site],sep=""),
       sub="Exp")
  par(new = T)
  plot(seq(0, max(d.vari$dist)*1.1,0.1),
       variogram.prediction(seq(0, max(d.vari$dist)*1.1,0.1),
                            model = spm.fit.ex$model[2],
                            xsill = spm.fit.ex$psill[2],
                            xrange = spm.fit.ex$range[2],
                            xnugget = spm.fit.ex$psill[1]),
       xlim = c(0, max(d.vari$dist)*1.1),
       ylim = c(0,max(d.vari$gamma)*1.1),
       type = "l", ylab = "", xlab = "", yaxt="n", xaxt = "n")
  abline(v=spm.fit.ex$range[2]*3)
  
  #球モデル
  plot(d.aniso$dist,d.aniso$gamma,
       xlim = c(0, max(d.vari$dist)*1.1),
       ylim = c(0,max(d.vari$gamma)*1.1),
       cex.axis=1.2, cex.lab=1.5,
       ylab = "variogram",xlab = "distance (m)", cex = 1.2,
       main = asiodata$asioVari_dir[i_site] ,sub="Sph")
  par(new = T)
  plot(seq(0, max(d.vari$dist)*1.1,0.1),
       variogram.prediction(seq(0, max(d.vari$dist)*1.1,0.1),
                            model = spm.fit.sp$model[2],
                            xsill = spm.fit.sp$psill[2],
                            xrange = spm.fit.sp$range[2],
                            xnugget = spm.fit.sp$psill[1]),
       xlim = c(0, max(d.vari$dist)*1.1),
       ylim = c(0,max(d.vari$gamma)*1.1),
       type = "l", ylab = "", xlab = "", yaxt="n", xaxt = "n")
  abline(v=spm.fit.sp$range[2])
  
  #線形
  plot(d.aniso$dist,d.aniso$gamma,
       xlim = c(0, max(d.vari$dist)*1.1),
       ylim = c(0,max(d.vari$gamma)*1.1),
       cex.axis=1.2, cex.lab=1.5,
       ylab = "variogram",xlab = "distance (m)", cex = 1.2,
       main = site.true.dir,sub="Lin")
  par(new = T)
  plot(seq(0, max(d.vari$dist)*1.1,0.1),
       variogram.prediction(seq(0, max(d.vari$dist)*1.1,0.1),
                            model = spm.fit.ln$model[2],
                            xsill = spm.fit.ln$psill[2],
                            xrange = spm.fit.ln$range[2],
                            xnugget = spm.fit.ln$psill[1]),
       xlim = c(0, max(d.vari$dist)*1.1),
       ylim = c(0,max(d.vari$gamma)*1.1),
       type = "l", ylab = "", xlab = "", yaxt="n", xaxt = "n")
  abline(v=spm.fit.ln$range[2])
  
  #ガウス
  plot(d.aniso$dist,d.aniso$gamma,
       xlim = c(0, max(d.vari$dist)*1.1),
       ylim = c(0,max(d.vari$gamma)*1.1),
       cex.axis=1.2, cex.lab=1.5,
       ylab = "variogram",xlab = "distance (m)", cex = 1.2,sub="Gau")
  par(new = T)
  plot(seq(0, max(d.vari$dist)*1.1,0.1),
       variogram.prediction(seq(0, max(d.vari$dist)*1.1,0.1),
                            model = spm.fit.ga$model[2],
                            xsill = spm.fit.ga$psill[2],
                            xrange = spm.fit.ga$range[2],
                            xnugget = spm.fit.ga$psill[1]),
       xlim = c(0, max(d.vari$dist)*1.1),
       ylim = c(0,max(d.vari$gamma)*1.1),
       type = "l", ylab = "", xlab = "", yaxt="n", xaxt = "n")
  abline(v=spm.fit.ga$range[2]*2)
  
  
  dev.copy(pdf,
           file=paste("22forVariogram500/mdl_slct/aniso500g_", site.name,
                      "_",asio_culdir, ".pdf", sep = ""),
           width = 20, height = 10)
  dev.off()
  
    # plot(sill.conv[-1],type = "l", )
  
  # result.temp <- data.frame("model" =spm.fit.aniso$model[2], 
  #                      "sill" = spm.fit.aniso$psill[2],
  #                      "range" = spm.fit.aniso$range[2], 
  #                      "nugget" = spm.fit.aniso$psill[1],
  #                      "degree" = dir.list[i_dir])
  # result <- rbind(result, result.temp)
  # i_init <- i_init + 1
  setTxtProgressBar(pb, i_site)
  # result <- result[-1,]
  # result <- transform(result,"SiteID" = site.name)
  # 
  # result.all <- rbind(result.all, result)
}
# result.all <- result.all[-1,]
# write.csv(result.all, "22forVariogram500/aniso500g_allresult2.csv")


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


