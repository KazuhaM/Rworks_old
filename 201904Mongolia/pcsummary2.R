# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
path <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"

path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
setwd(path)

averate <- c("60","180","300","600","1800")

d.flist <- list.files(path, pattern="csv")
d.flist300 <- d.flist[grep(averate,d.flist)]

pc.col = 4
wm.col = 6

n <- 3 + pc.col + wm.col*1.5 


######全サイトデータ結合########################################
for(k in 1 : length(averate)){
  d.flist300 <- d.flist[grep(averate[k],d.flist)]
  result.df <- data.frame(matrix(rep(NA, n), nrow=1))[numeric(0), ]
  colnames(result.df) <- c("Time", "SiteID", "Event", "PC_10_1", "PC_10_2", "PC_10_3", "PC_50_1",
                           "WS_h", "WS_m", "WS_l", "WD_h", "WD_m", "WD_l",
                           "Height_WM_h", "Height_WM_m", "Height_WM_l")
for(i in 1 : length(d.flist300)){
  d <- read.csv(d.flist300[i],header=T)
  dname <- strsplit(d.flist300[i], "_")
  dname <- sub(".csv", "", dname[[1]][2])
  d.pc <- d[,c(1, grep("PC",colnames(d)))]
  d.pc <- d.pc[, colnames(d.pc) != "PC18_50"]
  d.ws <- d[,c(1, grep("WS",colnames(d)))]
  d.wd <- d[,c(1, grep("WD",colnames(d)))]
  
  height.wm <- as.numeric(sub("WS_","",colnames(d.ws)[2:length(colnames(d.ws))]))
  height.wm.mat <- matrix(0,nrow(d),length(height.wm))
  for(j in 1 : nrow(d)){
    height.wm.mat[j,] <- height.wm
  }
  
  
  if(ncol(d.pc) == 4){
    temp.df <- cbind(d[,1], rep(dname,nrow(d)),rep(1,nrow(d)),rep(99999,nrow(d)),
                     d.pc[2:ncol(d.pc)],d.ws[2:ncol(d.ws)],d.wd[2:ncol(d.wd)],height.wm.mat)
  }else if(ncol(d.pc) == 5){
    temp.df <- cbind(d[,1], rep(dname,nrow(d)),rep(1,nrow(d)),
                     d.pc[2:ncol(d.pc)],d.ws[2:ncol(d.ws)],d.wd[2:ncol(d.wd)],height.wm.mat)
  }
  colnames(temp.df) <- colnames(result.df)
  result.df = rbind(result.df,temp.df)

  par(mfrow=c(3,1))
  #drifting sands
  ts.plot(d.pc[2],gpars=list(xlab="Time", ylab="Drifting Sands(n/s)", main = paste(averate[k],"_", dname,sep="")),
          ylim = c(0,max(d.pc[,2:ncol(d.pc)])),col=1) 
  for(j in 3:ncol(d.pc)){
    par(new = T)
    ts.plot(d.pc[j],gpars=list(xlab="", ylab=""),
            ylim = c(0,max(d.pc[,2:ncol(d.pc)])),col=j-1)
  }
  legend(max(as.ts(d[,1]))*0.9,max(d.pc[,2:ncol(d.pc)]),colnames(d.pc)[2:ncol(d.pc)],lty=1, 
         col =1:(ncol(d.pc)-1))
  
  #wind speed
  ts.plot(d.ws[2],gpars=list(xlab="Time", ylab="Wind Speed(m/s)"),
          ylim = c(0,max(d.ws[,2:ncol(d.ws)])),col=1)
  
  for(j in 3:ncol(d.ws)){
    par(new = T)
    ts.plot(d.ws[j],gpars=list(xlab="", ylab=""),
            ylim = c(0,max(d.ws[,2:ncol(d.ws)])),col=j -1)
  }
  legend(max(as.ts(d[,1]))*0.9,max(d.ws[,2:ncol(d.ws)]),colnames(d.ws)[2:ncol(d.ws)],lty=1,
         col =1:(ncol(d.ws)-1))
  
  #wind dir.
  ts.plot(d.wd[2],gpars=list(xlab="Time", ylab="Wind Direction(degree)"),
          ylim = c(0,max(d.wd[,2:ncol(d.wd)])),col=1)
  
  for(j in 3:ncol(d.wd)){
    par(new = T)
    ts.plot(d.wd[j],gpars=list(xlab="", ylab=""),
            ylim = c(0,max(d.wd[,2:ncol(d.wd)])),col=j -1)
  }
  legend(max(as.ts(d[,1]))*0.9,max(d.wd[,2:ncol(d.wd)]),colnames(d.wd)[2:ncol(d.wd)],lty=1,
         col =1:(ncol(d.wd)-1))
  abline(h = 120)
  abline(h = 240)
  
  dev.copy(pdf, file=paste(averate[k],"_", dname,".pdf",sep=""), width = 10, height = 10)
  dev.off()
}

write.csv(result.df, paste(sub("/avebyn","",path),"/", averate[k],"_sumdata.csv", sep = ""),row.names=FALSE)

}

############################臨界風速等算出###################
setwd(path2)
i <- 1
ev_filename = paste("Ev_",averate[i],"sumdata.csv",sep="")
ev.d <- read.csv(ev_filename,header=T)
ev.d["SF_gs"] <- NaN
ev.d["SF_sl"] <- NaN
ev.d["Z0"] <- NaN
ev.d["Us"] <- NaN
ev.d["Ust"]<-NaN
ev.d["c"]<-NaN

#粒径データ
d50 <- read.csv("SiteParticle.csv",header=T)
iSiteD50 <- d50[d50[,1]==ev.d$SiteID,]

#サルテーションフラックス算出
for(j in 1:nrow(ev.d)){
  if(ev.d$PC_10_1[j] == 99999){
    #Udo et al. 2008 の式
    #中央粒径：ground surface (Ishizuka et al. 2012)
    #中央粒径：saltation particle(abutaiti et al. 2013)
    ev.d$SF_gs[j] <- (2*2.5*(iSiteD50[,2]^3)*(ev.d$PC_10_2[j] + ev.d$PC_10_3[j]))/
      (3000*0.012^2*2*as.numeric(averate[i]))
    ev.d$SF_sl[j] <- (2*2.5*(iSiteD50[,3]^3)*(ev.d$PC_10_2[j] + ev.d$PC_10_3[j]))/
      (3000*0.012^2*2*as.numeric(averate[i]))
  } else{
    ev.d$SF_gs[j] <- (2*2.5*(iSiteD50[,2]^3)*(ev.d$PC_10_1[j] + ev.d$PC_10_2[j] + ev.d$PC_10_3[j]))/
      (3000*0.012^2*3*as.numeric(averate[i]))
    ev.d$SF_sl[j] <- (2*2.5*(iSiteD50[,3]^3)*(ev.d$PC_10_1[j] + ev.d$PC_10_2[j] + ev.d$PC_10_3[j]))/
      (3000*0.012^2*3*as.numeric(averate[i]))
  }
  #粗度、摩擦速度算出
  if(ev.d$WS_h[j] >= ev.d$WS_m[j] && ev.d$WS_m[j] >= ev.d$WS_l[j] && ev.d$WS_h[j] >= 8 &&  ev.d$Event != 99){
    iws <- c(ev.d$WS_h[j],ev.d$WS_m[j],ev.d$WS_l[j])
    ih <- c(ev.d$Height_WM_h[j],ev.d$Height_WM_m[j],ev.d$Height_WM_l[j])
    ih <- log(ih)
    
    tmp.result <- lm(ih ~ iws)
    ev.d$Z0[j] <- exp(as.numeric(tmp.result$coefficients[1]))
    ev.d$Us[j] <- 0.4/as.numeric(tmp.result$coefficients[2])
  }
  
}


sitelev <- levels(ev.d$SiteID)
ev.d$Event <- as.factor(ev.d$Event)
# eventlev <- levels(ev.d$Event)
###結果出力用配列作成
result.df <- data.frame(matrix(rep(NA, n), nrow=1))[numeric(0), ]
colnames(result.df) <- c("SiteID", "Event", "Ust", "c", "AveZ0", "Coverage",
                         "AveHeight", "WS_m", "WS_l", "WD_h", "WD_m", "WD_l",
                         "Height_WM_h", "Height_WM_m", "Height_WM_l")
for(i in 1:length(sitelev)){
  for(j in 1:length(eventlev)){
    tmp.d2 <- ev.d[ev.d$SiteID == sitelev[i] & ev.d$Event == eventlev[i],]
    if(nrow(tmp.d2) == 0){
      next#そのイベントが存在しない場合スキップ
    }
    #移動開始限界風速が定義できるか判定
    if(length(tmp.d2[tmp.d2$avePC>= 1])==0){
      v.Ut <- NA
      v.D <- NA
    }else{
      #移動開始限界風速が定義できる場合
      v.minW<-min(tmp.d2[tmp.d2$avePC>= 1])
      tmp.d2t <- tmp.d2[tmp.d2$avePC>= 1]

      ##臨界風速を変化させて適切な値を見つける
      #変化させる仮の臨界風速の値のベクトル作成
      #step.u <- seq(max(min(d2$WindSpeed)-1,0),max(d2$WindSpeed)*0.8, by = 0.01)
      step.u <- seq(max(min(d2$WindSpeed)-1,0),
                    length = round((max(d2$WindSpeed) - max(min(d2$WindSpeed)-1,0))*0.75/0.01), 
                    by = 0.01)
      #各仮の臨界風速以上の風速の場合のデータのみで臨界風速算出
      R2 <- c()
      R2.sum <- cbind(c(0,0,0,0,0),c(0,0,0,0,0))
      for (k in step.u) {
        #各仮の臨界風速以上の風速の場合のデータ抽出
        d3 <- d2[d2$WindSpeed >= k, ]
        d3 <- d3[!is.na(d3[,1]),]
        
        x <- d3$WindSpeed
        y <- d3$DriftSand
        
        a3 <- sum(x^3 * y - k^2 * x * y)/sum(k^4 * x^2 - 2 * k^2 * x^4 + x^6)
        
        Pred <- a3 * d3$WindSpeed * (d3$WindSpeed^2 - k^2)
        if((length(d3$WindSpeed)-1-1) <= 0){
          next
        }
        R2 <- 1- (sum((d3$DriftSand - Pred)^2)/(length(d3$WindSpeed)-1-1))/
          (sum((d3$DriftSand - mean(d3$DriftSand))^2)/(length(d3$WindSpeed)-1))
        R2.sum <- cbind(R2.sum,c(k,R2,length(d3$WindSpeed),k,a3))
        
      }
      R2.sum <-R2.sum[,3:ncol(R2.sum)]
      R2.sum <- t(R2.sum)
      
      plot(R2.sum[,2],main=paste("R2.Event",lev[[4]][j],"_P.C.",
                                 lev[[3]][l],sep=""))
      dev.copy(pdf, file=paste("Step_R2.Event",lev[[4]][j],"_P.C.",
                               lev[[3]][l],".pdf",sep=""), width = 10, height = 10)
      dev.off()
      R2.sum.max <- R2.sum[which.max(R2.sum[,2]),]#決定係数が最小となるときの各種値
      v.SUt <- R2.sum.max[1]
      v.r2 <- R2.sum.max[2]
      v.ns <- R2.sum.max[3]
      v.n <- length(d2$WindSpeed)
      v.Ut <- R2.sum.max[4]
      v.D <- R2.sum.max[5]
      
      plot(d2$WindSpeed, d2$DriftSand, main=paste("Event",lev[[4]][j],"_P.C.",
                                                  lev[[3]][l],sep=""), xlab ="WindSpeed",
           ylab ="DriftSand",xlim=c(0,max(d2$WindSpeed)*1.1),
           ylim = c(0,max(d2$DriftSand)*1.1))
      
      abline(v = v.Ut, col = 1)
      mtext(paste("Ut:",round(v.Ut,1),sep=""), side = 1, line = 2, at = v.Ut)
      abline(v = v.minW, col =2)
      mtext(paste("Utf:",round(v.minW,1),sep=""), side = 1, line = 2, at = v.minW)
      par(new=T)
      Appro <- function(x) v.D * x^3 - v.D * v.Ut^2 * x  # 標準正規分布の密度
      plot(Appro,v.Ut,max(d2$WindSpeed),
           xlim=c(0,max(d2$WindSpeed)*1.1),ylim = c(0,max(d2$DriftSand)*1.1),
           xlab ="WindSpeed", ylab ="DriftSand")
      dev.copy(pdf, file=paste("Step_SN_WS_Event",lev[[4]][j],"_P.C.",
                               lev[[3]][l],".pdf",sep=""), width = 10, height = 10)
      dev.off()
    }
    #結果配列作成
    ut <- cbind(ut,c(v.minW,v.SUt,v.r2,v.ns,v.n,v.Ut,v.D
                     ,as.numeric(as.character(d4.t$P.C.No.[1])),d4.t$Event[1]
                     ,as.numeric(as.character(d4.t$Date[1]))
                     ,d4.t$SiteCover[1],d4.t$Richness[1]
                     ,d4.t$Ave.Height[1],d4.t$DominantAve.Height[1]
                     ,d4.t$Sum.Veg.Vol.[1],d4.t$Sum.DominantVeg.Vol.[1]
                     ,as.numeric(d4.t$E.Veg.Type2[1]),d4.t$DCA3[1] ))
    dimnames(ut) <- list(c("Utf","Step.Ut","R2","sump num","all num","Ut","D","P.C.No.",
                           "Event","Date","Cov",
                           "Richness", "Ave.Height","DominantAve.Height",
                           "Sum.Veg.Vol.", "Sum.DominantVeg.Vol.","E.Veg.Type2"
                           ,"DCA3"),
                         c(colnames(ut)[1:ncol(ut)-1],
                           paste("Event",lev[[4]][j],"_P.C.",lev[[3]][l],sep="")))
  }
}

####飛砂数風速風向

d <- read.csv("SummaryAllData-19.csv",header=T)
d <- d[d[,6]!="",]
d <- d[!is.na(d$DriftSand),]
d <- d[!is.na(d$WindSpeed),]
d[,1]<- as.numeric(d[,1])

for(i in 2:5){
  d[,i] <- factor(d[,i])  
}
str(d)
lev <- list()
for(i in 2:5){
  lev[[i]] <- levels(d[,i])  
}
lev

windows()
par(mfrow=c(3,1),mar = c(2, 7, 4, 2)) 

k=2
d2 <- d[d$P.C.No.==lev[[4]][k], ]
d2 <- d2[!is.na(d2[,1]),]

plot(d2$Date_Hour, d2$DriftSand,xlab="", ylab="DriftSands
     (n/s)",
     ylim=c(0,ceiling(max(d$DriftSand[!is.na(d$DriftSand)])*1.2)),
     xlim=c(43307.4204861111,43315.5315972222),type = "p",xaxt  = "n", cex.axis = 2,
     cex.lab= 2)

for(k in 2 : length(lev[[4]])){  
  d2 <- d[d$P.C.No.==lev[[4]][k], ]
  d2 <- d2[!is.na(d2[,1]),]
  if(nrow(d2) == 0){
    next
  }
  par(new=T)
  plot(d2$Date_Hour, d2$DriftSand,xlab="", ylab="",
       ylim=c(0,ceiling(max(d$DriftSand[!is.na(d$DriftSand)])*1.2)),
       xlim=c(43307.4239583333,43315.5315972222),type = "p",xaxt  = "n",yaxt  = "n", cex.axis = 2,
       cex.lab= 2)

}

event.t <- c(43308.3020833333, 43308.7326388889, 43309.6458333333, 43309.8472222222, 43311.4444444444,
             43311.6631944444, 43312.1666666666, 43312.7430555556, 43314.2881944444, 43315.2743055556)
for (i in 1:5) {
  abline(v = event.t[2*i-1])
  abline(v = event.t[2*i])
}


k = 0
d2 <- d[d$P.C.No.==lev[[4]][k], ]
d2 <- d2[!is.na(d2[,1]),]
plot(d2$Date_Hour, d2$WindSpeed,xlab="", ylab="WindSpeed
     (m/s)",xlim=c(43307.4239583333,43315.5315972222),type = "l",xaxt  = "n",ylim=c(0,max(d2$WindSpeed)*1.1), cex.axis = 2,
     cex.lab= 2)
par(mar = c(5, 7, 1, 2)) 
plot(d2$Date_Hour, d2$WindDir.,xlab="Time", ylab="WindDirection
     (degree)",xlim=c(43307.4239583333,43315.5315972222),type = "l",ylim=c(0,360), cex.axis = 2,
     cex.lab= 2)
abline(h = 120)
abline(h = 240)

dev.copy(pdf, file="Summary_WheatherSite0.pdf", width = 20, height = 10)
dev.off()


######イベンドごとの風配図
library("fmsb")

for (l in 1:5) {
  d3 <- d2[d2$Date_Hour<=event.t[2*l] & d2$Date_Hour>=event.t[2*l-1] , ]
  counttime <- nrow(d3)
  wr.break <- seq(0, 360, length = 16)
  Windrose <- rbind(numeric(16),1:16)
  colnames(Windrose) <- c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W"
                          ,"WNW","NW","NNW")
  rownames(Windrose) <- c("du0","du1")
  for (i in 5:1) {
    temp<- hist(d3[d3$WindSpeed < 2*i & d3$WindSpeed >= 2*(i-1), 8],
                breaks=seq(0, 360, length = 17))
    Windrose <- rbind(Windrose,temp$counts*100/counttime)
    dimnames(Windrose) <- list(c(rownames(Windrose)[1:nrow(Windrose)-1],
                                 paste(2*(i-1),"~",2*i,"m/s",sep="")),
                               c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W"
                                 ,"WNW","NW","NNW"))
  }
  Windrose <- t(Windrose)
  Windrose <- Windrose[order(-Windrose[,2]),]
  Windrose <- t(Windrose)
  Windrose <-cbind(Windrose[,16],Windrose[,1:(ncol(Windrose)-1)])
  colnames(Windrose) <- c("N","NNW","NW","WNW","W","WSW","SW","SSW","S","SSE","SE","ESE",
                              "E","ENE","NE","NNE")

  Windrose <-Windrose[3:nrow(Windrose),]
  
  
  wr.min <- numeric(16)
  wr.max <- rep(max(Windrose),length=16)
  
  Windrose = rbind(wr.max,wr.min,Windrose)
  Windrose <- data.frame(Windrose)

  radarchart(Windrose, axistype = 1, seg = 5, plty = 1, palcex = 2,vlcex = 2,pty=32,cglwd=2,
             centerzero = TRUE, vlabels = colnames(Windrose),
             #title = paste("Windrose at Site",lev[[4]][k]," Event",l,sep=""),
             plwd=2 ,col = 1:5,
             caxislabels=paste(round(seq(0,max(Windrose),length = 6),1),"%",sep=""))
  dev.copy(pdf, file=paste("Windrose_Event",l,"_P.C.",
                           lev[[4]][k],".pdf",sep=""), width = 10, height = 10)
  dev.off()
}

Windrose <- rbind(1:16,Windrose)
Windrose <- cbind(nrow(Windrose):1,Windrose)

Windrose <- Windrose[order(Windrose[,1]),]

Windrose <-Windrose[,2:ncol(Windrose)]

plot.new()
legend(0,0.1, row.names(Windrose[1:5,]),lty=1,col=1:5,cex=1.2,ncol=5,lwd = 2)
dev.copy(pdf, file=paste("WrLegend.pdf",sep=""), width = 10, height = 10)
dev.off()

#####植被率、植生量＿飛砂数

i = 0
d5 <- d[d$E.Veg.Type2==i, ]
d5 <- d5[!is.na(d5[,1]),]

plot(d5$Sum.DominantVeg.Vol., d5$DriftSand,xlab="Dominant Veg.Vol", ylab="DriftSands(num)",
     ylim=c(0,ceiling(max(d$DriftSand[!is.na(d$DriftSand)])*1.2)),
     xlim=c(0,max(d$Sum.DominantVeg.Vol.)*1.1),type = "p", cex.axis = 1.5,
     cex.lab= 1.5,col=i+1)

for(i in 1 : 4){  
  d5 <- d[d$E.Veg.Type2==i, ]
  d5 <- d5[!is.na(d5[,1]),]
  if(nrow(d5) == 0){
    next
  }
  par(new=T)
  plot(d5$Sum.DominantVeg.Vol., d5$DriftSand,xlab="Dominant Veg.Vol", ylab="DriftSands(num)",
       ylim=c(0,ceiling(max(d$DriftSand[!is.na(d$DriftSand)])*1.2)),
       xlim=c(0,max(d$Sum.DominantVeg.Vol.)*1.1),type = "p", cex.axis = 1.5,
       cex.lab= 1.5,col=i+1)
  
}
legend(locator(1), legend=c("Shift","Annual","Grass","Shrub","Shrub/Grass"),pch=1,col=1:5 )
dev.copy(pdf, file="SN_VegVol.pdf", width = 10, height = 10)
dev.off()

###植被率
i = 0
d5 <- d[d$E.Veg.Type2==i, ]
d5 <- d5[!is.na(d5[,1]),]

plot(d5$SiteCover, d5$DriftSand,xlab="Coverage", ylab="DriftSands(num)",
     ylim=c(0,ceiling(max(d$DriftSand[!is.na(d$DriftSand)])*1.2)),
     xlim=c(0,max(d$Sum.DominantVeg.Vol.)*1.1),type = "p", cex.axis = 1.5,
     cex.lab= 1.5,col=i+1)

for(i in 1 : 4){  
  d5 <- d[d$E.Veg.Type2==i, ]
  d5 <- d5[!is.na(d5[,1]),]
  if(nrow(d5) == 0){
    next
  }
  par(new=T)
  plot(d5$SiteCover, d5$DriftSand,xlab="Coverage", ylab="DriftSands(num)",
       ylim=c(0,ceiling(max(d$DriftSand[!is.na(d$DriftSand)])*1.2)),
       xlim=c(0,max(d$Sum.DominantVeg.Vol.)*1.1),type = "p", cex.axis = 1.5,
       cex.lab= 1.5,col=i+1)
  
}
legend(locator(1), legend=c("Shift","Annual","Grass","Shrub","Shrub/Grass"),pch=1,col=1:5 )
dev.copy(pdf, file="SN_Coverage.pdf", width = 10, height = 10)
dev.off()

###height
i = 0
d5 <- d[d$E.Veg.Type2==i, ]
d5 <- d5[!is.na(d5[,1]),]

plot(d5$DominantAve.Height, d5$DriftSand,xlab="Height", ylab="DriftSands(num)",
     ylim=c(0,ceiling(max(d$DriftSand[!is.na(d$DriftSand)])*1.2)),
     xlim=c(0,max(d$Sum.DominantVeg.Vol.)*1.1),type = "p", cex.axis = 1.5,
     cex.lab= 1.5,col=i+1)

for(i in 1 : 4){  
  d5 <- d[d$E.Veg.Type2==i, ]
  d5 <- d5[!is.na(d5[,1]),]
  if(nrow(d5) == 0){
    next
  }
  par(new=T)
  plot(d5$DominantAve.Height, d5$DriftSand,xlab="Height", ylab="DriftSands(num)",
       ylim=c(0,ceiling(max(d$DriftSand[!is.na(d$DriftSand)])*1.2)),
       xlim=c(0,max(d$Sum.DominantVeg.Vol.)*1.1),type = "p", cex.axis = 1.5,
       cex.lab= 1.5,col=i+1)
  
}
legend(locator(1), legend=c("Shift","Annual","Grass","Shrub","Shrub/Grass"),pch=1,col=1:5 )
dev.copy(pdf, file="SN_height.pdf", width = 10, height = 10)
dev.off()