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

result.all <- data.frame("SiteID" = "base", 
                        "CoverSHP" = 0.0)
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
  d.z<- sp_shp$area / 725.805 / 100
  d.cover <- sum(d.z) / 400
  
  result<- data.frame("SiteID" = site.name,
                       "CoverSHP" = d.cover)

  result.all <- rbind(result.all, result)
}
result.all <- result.all[-1,]
write.csv(result.all, "21forVariogram/CoverSHP.csv")


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


