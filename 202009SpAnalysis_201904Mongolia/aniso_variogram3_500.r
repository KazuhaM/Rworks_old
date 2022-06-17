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

# sites file
sites <- read.csv("Sites_var.csv", header = T)
initval <- read.csv("22forVariogram500/aniso500g_InitValue2.csv",header = T)
i_init <- 1

result.all <- data.frame("model" = "base", 
                        "sill" = 0.0,
                        "range" = 0.0, 
                        "nugget" = 0.0,
                        "degree" = 0.0,
                         "SiteID"="base",
                        "Event" = 0)
# data load
pb <- txtProgressBar(min = 1, max = nrow(initval), style = 3)

for (i_site in 1:nrow(initval)) {

# site.name <- "W2_3"
  site.name <- initval[i_site,"SiteID"]
  asio_culdir <- initval[i_site,"asioVari_dir"]
  site.ev <- initval[i_site,"Event"]
  
  # for init value
  i_model = initval[i_site,"model"]
  i_sill = initval[i_site,"sill"]
  i_range = initval[i_site,"range"]
  i_nugget = initval[i_site,"nugget"]
  i_onoff = initval[i_site,"OnOff"]
  
  sp_shp<-sf::st_read(paste("22forVariogram500/500g_", site.name, ".shp", sep = ""))

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

  result <- data.frame("model" = "base", 
                       "sill" = 0.0,
                       "range" = 0.0, 
                       "nugget" = 0.0,
                       "degree" = 0.0)
  
  print(paste(site.name,"    ", i_site,"/",nrow(initval),sep = ""))

  sill.conv <- c(0)
  # print(i_dir)
  d.aniso<- variogram(Z~X+Y,d,alpha=asio_culdir,
                      tol.hor = 90/dir.len,width=0.4)
  # plot(d.aniso)
  spm.model.aniso<-vgm(psill=i_sill ,
                       model=i_model, range= i_range, nugget= i_nugget) 

  if (i_onoff == 0) {
    spm.fit.aniso<-fit.variogram(d.aniso, spm.model.aniso)
  }else if (i_onoff == 1){
    spm.fit.aniso<- vgm(psill=i_sill  ,
                        model=i_model, range= i_range, nugget= i_nugget)
  }
  
  

  sill.conv <- sill.conv[-1]

  plot(d.aniso$dist,d.aniso$gamma,
       xlim = c(0, max(d.vari$dist)*1.1),
       ylim = c(0,max(d.vari$gamma)*1.1),
       cex.axis=1.2, cex.lab=1.5,
       ylab = "variogram",xlab = "distance (m)", cex = 1.2)
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
  # if (i_dir == dir.len) {
    dev.copy(pdf,
             file=paste("22forVariogram500/aniso500g_", site.name,
                        "_",site.ev, ".pdf", sep = ""), 
                        width = 10, height = 10)
    dev.off()
  # }
  
  # plot(sill.conv[-1],type = "l", )
  
  result.temp <- data.frame("model" =spm.fit.aniso$model[2], 
                       "sill" = spm.fit.aniso$psill[2],
                       "range" = spm.fit.aniso$range[2], 
                       "nugget" = spm.fit.aniso$psill[1],
                       "degree" = asio_culdir)
  result <- rbind(result, result.temp)
  i_init <- i_init + 1


  result <- result[-1,]
  result <- transform(result,"SiteID" = site.name, "Event" = site.ev)
  
  result.all <- rbind(result.all, result)
  
  setTxtProgressBar(pb, i_site)
}
result.all <- result.all[-1,]
write.csv(result.all, "22forVariogram500/aniso500g_allresult3.csv")



