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

# 
resol <- c("05","1","17")

# tolerance angle = 90 / dir.len
dir.len <- 8

# sites file
sites <- c(paste("rand",1:5,sep=""),"regular2")

# model file
site.model <- data.frame(SiteID=c(paste("rand",1:5,sep=""),"regular2"),
                         model = rep("Exp",length(sites)),
                         model2 = rep("Sph",length(sites)))

# site順一致チェック
if (sum((site.model[,1] == sites)==FALSE) != 0) {
  stop("The site order is invalid, so the script must end here")
}

result.all <- data.frame("model" = "base", 
                        "sill" = 0.0,
                        "range" = 0.0, 
                        "nugget" = 0.0,
                         "SiteID"="base",
                        "resol" = "base")

# canopy gap histgram ylim
cg.ylim <- data.frame(min=rep(0,length(i_resol*i_site)),
                      max=c(rep(60,6),rep(20,6),rep(15,6)))
cg.count <- 1

# data load
for(i_resol in 1:length(resol)){
for (i_site in 1:length(sites)) {
  
  # result <- data.frame("model" = "base", 
  #                      "sill" = 0.0,
  #                      "range" = 0.0, 
  #                      "nugget" = 0.0)
# site.name <- "W2_3"
  site.name <- sites[i_site]
  sp_shp<-sf::st_read(paste("31Variogram_test/",resol[i_resol],"_vtest_", site.name, ".shp", sep = ""))
  # plot(sp_shp)
  
  # make coordinate data as the center of each mesh
  d.x<- (sp_shp$left+sp_shp$right)*20/5400/2
  d.y<- 20 + (sp_shp$top+sp_shp$bottom)*20/5400/2
  # colnames(sp_shp)
  # grep("_p",colnames(sp_shp))
  d.z<- sp_shp[,grep("_1",colnames(sp_shp))] *  0.5 ^ 2
  d.z <- d.z[,1]
  
  # make spatial point data frame
  d <- data.frame("X" = d.x, "Y" = d.y, "Z" = d.z)
  d.coords<-cbind(d$X,d$Y)
  d<-SpatialPointsDataFrame(d.coords,d)
  
  # hist of inter-shrub street
  # this include streets that is from a shrub to the edge of site
  # wind direction N0 only (azimth range = 0 degree)
  {
    x.lev <- as.numeric(levels(as.factor(d.x)))
    hd.tl <- c(0,0,0)
    for(i_xlev in x.lev){
      z.temp <- d.z[d.x==i_xlev]
      if(sum(z.temp) == 0){
        hd.tl <- rbind(hd.tl, c(i_xlev, 0,40))
      }else{
        up0.z <- which(z.temp>0)
        if(length(up0.z) ==1 ){
          hd.tl <- rbind(hd.tl, 
                         c(i_xlev, 0.5,up0.z[1]))
          hd.tl <- rbind(hd.tl, 
                         c(i_xlev, up0.z[1],40))
        }else{
          # find intershrub street
          for(i_y in 1:(length(up0.z)-1)){
            if(i_y == 1 & up0.z[i_y] != 1){
              hd.tl <- rbind(hd.tl, 
                             c(i_xlev, 0.5, up0.z[i_y]))
            }else if(up0.z[i_y + 1] != up0.z[i_y] + 1 ){
              hd.tl <- rbind(hd.tl, 
                             c(i_xlev, up0.z[i_y],up0.z[i_y + 1]))
            }
            if(i_y == length(up0.z)-1 & up0.z[i_y + 1] != 40){
              hd.tl <- rbind(hd.tl,
                             c(i_xlev, up0.z[i_y + 1],40))
            }
          }
        }
      }
    }
    if(sum(hd.tl[1,])==0){
      hd.tl <- hd.tl[-1,]
    }
    hd.tl <- data.frame(x = hd.tl[,1],head = hd.tl[,2],tail = hd.tl[,3])
    hd.tl$len <- 0.5 * (hd.tl$tail - hd.tl$head)
  }
  # plot histogram
  par(mar = c(2.5,2.5,1,1))
  hist(hd.tl$len,breaks=seq(0,20,by=0.5),xlim = c(0,20),
       ylim = c(cg.ylim[cg.count,1],cg.ylim[cg.count,2]),
       # main= paste("pi=",resol[i_resol],",", site.name,sep=""),
       main = "",
       xlab = "",
       ylab = "")
  cg.count <- cg.count + 1
  abline(v = mean(hd.tl$len),col=2,lwd = 2)
  mtext(round(mean(hd.tl$len),1),at=mean(hd.tl$len),side = 1)
  abline(v = median(hd.tl$len),col=4,lwd = 2)
  mtext(round(median(hd.tl$len),1),at=median(hd.tl$len))
  
  # legend("topright", c("mean","median"),col=c(2,4),lty=1)
  # dev.copy(pdf,
  #          file=paste("31Variogram_test/sthist",resol[i_resol],"_vtest_", site.name, ".pdf", sep = ""),
  #          width = 5, height = 5)
  # dev.off()
  dev.copy(svg,
           file=paste("31Variogram_test/sthist",resol[i_resol],"_vtest_", site.name, ".svg", sep = ""),
           width = 5, height = 5,family = "Times New Roman")
  dev.off()
  
  # plot shrub distribution
  # {
  #   par(mar = c(3,3,3,3),xpd=F)
  #   plot.new()
  #   plot.window(xlim=c(-2, 22), ylim=c(-2, 22)) 
  #   par(new=T)
  #   plot(d, pch = 21, cex = 0.8,
  #        col= cols[round((sp_shp[,grep("_1",colnames(sp_shp))]/100)[,1]* 10,0) + 1] ,
  #        bg = cols[round((sp_shp[,grep("_1",colnames(sp_shp))]/100)[,1]* 10,0) + 1],
  #        xlim=c(-2, 22), ylim=c(-2, 22) )
  #   for (j in 1:4) {
  #     axis(side = j, at=seq(0, 20, by = 5))
  #   }
  #   for (j in 0:20) {
  #     segments(j, 0, j, 20,col = "gray")
  #     segments(0, j, 20, j,col = "gray")
  #   }
  #   for (j in 0:4) {
  #     abline(h = j*5, lty=3,col = "gray")
  #     abline(v = j*5, lty=3,col = "gray")
  #   }
  #   for(j in 1:10){
  #     rect(4+j, -1, 5+j, -0.5,border = TRUE,col=cols[j])
  #   }
  #   rect(5, -2, 15, -1.1,border="White",col="White")
  #   text(5,-1.5,0)
  #   # text(15,-1.5,max(temp_WsiteD3$c))
  #   text(15,-1.5,100)
  #   text(10,-1.5,"cover (%)",adj=0.5)
  #   
  #   dev.copy(pdf,
  #            file=paste("31Variogram_test/plotsh_",resol[i_resol],"_vtest_", site.name, ".pdf", sep = ""),
  #            width = 10, height = 10)
  #   dev.off()
  # }
  
  
  print(paste(resol[i_resol],"_",site.name,"    ", i_site,"/",length(sites),sep = ""))
  # cul variogram as a isotropy for graph lim
  d.vari <- variogram(Z~X+Y,d,alpha=0,tol.hor = 90/dir.len,width=0.4)
  
  # temp.model = site.model[i_site,2]
  # if (temp.model == "Exp") {
  #   #指数
  #   spm.model<-vgm(psill=max(d.vari$gamma)*0.4  ,model="Exp",
  #                     range=0.6, nugget=1.0 * 10^(-6)) #W2_3
  # }else if(temp.model == "Sph"){
  #   # 球
  #   spm.model<-vgm(psill=max(d.vari$gamma)-10  ,model="Sph",
  #                     range=2, nugget=10) #W2_3
  # }else if(temp.model == "Lin"){
  #   # 線形
  #   spm.model<-vgm(psill=max(d.vari$gamma)-10  ,model="Lin",
  #                     range=2, nugget=10) #W2_3
  # }else if(temp.model == "Gau"){
  #   # ガウス
  #   spm.model<-vgm(psill=max(d.vari$gamma)  ,model="Gau",
  #                     range=2, nugget=1.0 * 10^(-6)) #W2_3
  # }

  spm.fit<-fit.variogram(d.vari, spm.model)#,debug.level=3)
  
  # #サイトごと個別調整用
  # spm.fit<-vgm(psill=116  ,model="Exp",
  #                range=1.8, nugget=0) #W2_3
  # plot(d.vari,spm.fit)

  ##################
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

  #### バリオグラムプロット画像保存
  # dev.copy(pdf,
  #          file=paste("31Variogram_test/",resol[i_resol],"_vtest_", site.name, ".pdf", sep = ""),
  #          width = 10, height = 10)
  # dev.off()

  # plot(sill.conv[-1],type = "l", )
  
  result<- data.frame("model" =spm.fit$model[2], 
                       "sill" = spm.fit$psill[2],
                       "range" = spm.fit$range[2], 
                       "nugget" = spm.fit$psill[1])
  # result <- result[-1,]
  result <- transform(result,"SiteID" = site.name,"resol" = resol[i_resol])
  
  result.all <- rbind(result.all, result)
}
}
result.all <- result.all[-1,]
write.csv(result.all, "31Variogram_test/vtest_allresult2.csv")


###########################################################################################
r.data <- data.frame(resol =c(5,1,17,5,5,5,5,5,1,1,1,1,1,17,17,17,17,17),
                     range=c(1.4,3.9,5.4,1.8,1.8,1.7,1.7,1.7,3.8,3.0,2.1,3.9,4.1,4.2,4.5,6.6,4.8,5.7))
r.data$resol <- as.factor(r.data$resol)
plot(range ~ resol,data=r.data[4:nrow(r.data),],
     ylim = c(0,max(r.data$range)))
points(r.data$range[1:3]~c(2,1,3),pch=21,bg=1)

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


