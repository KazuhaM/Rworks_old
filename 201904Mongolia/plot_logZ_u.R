path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
setwd(path3)
averate <- c("60")
i <- 1

data <- read.csv(paste("Z0Us_MoM_",averate[i],"_sumdata.csv",sep = ""))

sitelev <- levels(data$SiteID)
data$Event <- as.factor(data$Event)
eventlev <- levels(data$Event)[levels(data$Event)!="99"]


for(j in 1:length(sitelev)){
  for (k in 1:length(eventlev)) {
    temp.d <- data[data$SiteID == sitelev[j] & data$Event == eventlev[k],]
    if (nrow(temp.d) > 1 ) {
      temp.d <- temp.d[!is.na(temp.d$Z0),]
      
      # 背景描画
      plot(1, type ="n", xlim = c(0,max(temp.d$WS_h)*1.1), #ylim = c(0,5), 
           ylim = c(temp.d$Height_WM_l[1] - temp.d$d0[1],5),log = "y",
            xlab = "wind speed (m/s)", ylab = "z - d_0(m)",yaxt="n",
           main = paste(sitelev[j],eventlev[k],sep="_"))
      axis(side=2,at = 10^seq(-1,1))
      axis(side=2,at = 2:5)
      axis(side=2,at = temp.d$Height_WM_l[1])
      # axis(side=2,at = 1:5)
      
      # データプロット
      points(temp.d$WS_h,temp.d$Height_WM_h - temp.d$d0[1] ,ann = F,col = rainbow(12))
      points(temp.d$WS_m,temp.d$Height_WM_m - temp.d$d0[1] ,ann = F,col = rainbow(12))
      points(temp.d$WS_l,temp.d$Height_WM_l - temp.d$d0[1] ,ann = F,col = rainbow(12))

      # 1データずつのプロット
      # l <- 6
      # points(temp.d$WS_h[l],temp.d$Height_WM_h[l] - temp.d$d0[1] ,ann = F,col = rainbow(12)[l])
      # points(temp.d$WS_m[l],temp.d$Height_WM_m[l] - temp.d$d0[1] ,ann = F,col = rainbow(12)[l])
      # points(temp.d$WS_l[l],temp.d$Height_WM_l[l] - temp.d$d0[1] ,ann = F,col = rainbow(12)[l])
      # 
      # zset <- seq(temp.d$Height_WM_l[l]+0.01,by= 0.01,length = 300)
      # zset <- zset - temp.d$d0[l]
      # zset <- c(min(zset),max(zset))
      # lines(Lwall(zset,temp.d$Z0[l],temp.d$Us[l]),zset,type = "l",col = rainbow(12)[l])
      
      # 対数則プロット
      # abline(temp.d$Z0,temp.d$Us/0.4)
      
      for(i in 1:length(temp.d$Z0[!is.na(temp.d$Z0)])){
        if(!is.na(temp.d$Z0[i])){
          zset <- seq(temp.d$Height_WM_l[i]+0.01,by= 0.01,length = 300)
          zset <- zset - temp.d$d0[i]
          zset <- c(min(zset),max(zset))
          lines(Lwall(zset,temp.d$Z0[i],temp.d$Us[i]),zset,type = "l",col = rainbow(12)[i])
        }
      }
      text(0,4,labels =paste("z_0: ",signif(temp.d$Z0[i],3),"\n","d_0: ",temp.d$d0[i],sep = ""),adj = 0 ) 
    }
    dev.copy(pdf, file=paste(path3,"/lnZ_u/lnZ_u_",sitelev[j],"_",eventlev[k],".pdf",sep=""), width = 10, height = 10)
    dev.off()
  }
}

Lwall <- function(z,z0,us){
  return(us*log(z /z0)/0.4)
}
