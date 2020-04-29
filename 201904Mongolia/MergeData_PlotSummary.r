library(lubridate)
# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
path <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"

path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
setwd(path)

averate <- c("60","180","300","600","1800")

d.flist <- list.files(path, pattern="csv")
# d.flist300 <- d.flist[grep(averate,d.flist)]

pc.col = 4
wm.col = 6

n <- 3 + pc.col + wm.col*1.5 

# 
eventperiod <- read.csv(paste(path2,"EventPeriod.csv",sep="/"))
eventperiod$Start <- as.POSIXct(eventperiod$Start)
eventperiod$End <- as.POSIXct(eventperiod$End)

colpalette <- c("black","chartreuse","orange","cyan2","blueviolet","pink","wheat","salmon2")


######全サイトデータ結合######################################## 
for(k in 1 : length(averate)){
  d.flist300 <- d.flist[grep(paste(averate[k],"_",sep=""),d.flist)]
  result.df <- data.frame(matrix(rep(NA, n), nrow=1))[numeric(0), ]
  colnames(result.df) <- c("Time", "SiteID", "Event", "PC_10_1", "PC_10_2", "PC_10_3", "PC_50_1",
                           "WS_h", "WS_m", "WS_l", "WD_h", "WD_m", "WD_l",
                           "Height_WM_h", "Height_WM_m", "Height_WM_l")
  for(i in 1 : length(d.flist300)){
    d <- read.csv(d.flist300[i],header=T)
    d$Time <- as.POSIXct(d$Time)
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
    
    # par(mfrow=c(3,1))
    layout(matrix(c(1,1,2,2,3,3,4), nrow = 7, ncol = 1, byrow = TRUE))
    #drifting sands
    plot(d.pc$Time,d.pc[,2],xlab="", ylab="Drifting Sands(n/s)", main = paste(averate[k],"_", dname,sep=""),
         type="l", xaxt="n",ylim = c(0,max(d.pc[,2:ncol(d.pc)])),col=1) 
    axis.POSIXct(side=1,at=seq(d.pc[1,1], d.pc[nrow(d.pc),1],
                               "1 hour"), las=2, format="%m/%d %H:%M")
    for(j in 3:ncol(d.pc)){
      par(new = T)
      plot(d.pc$Time,d.pc[,j],xlab="", ylab="",type="l", xaxt="n",
              ylim = c(0,max(d.pc[,2:ncol(d.pc)])),col=j-1)
    }

    #wind speed
    plot(d.ws$Time,d.ws[,2],xlab="", ylab="Wind Speed(m/s)",type="l", xaxt="n",
            ylim = c(0,max(d.ws[,2:ncol(d.ws)])),col=1)
    axis.POSIXct(side=1,at=seq(d.ws[1,1], d.ws[nrow(d.ws),1],
                               "1 hour"), las=2, format="%m/%d %H:%M")
    for(j in 3:ncol(d.ws)){
      par(new = T)
      plot(d.ws$Time,d.ws[,j],xlab="", ylab="",type="l", xaxt="n",
              ylim = c(0,max(d.ws[,2:ncol(d.ws)])),col=j -1)
    }

    #wind dir.
    plot(d.wd$Time, d.wd[,2], xlab="", ylab="Wind Direction(degree)",
            ylim = c(0,max(d.wd[,2:ncol(d.wd)])),col=1,type="l", xaxt="n")
    axis.POSIXct(side=1,at=seq(d.wd[1,1], d.wd[nrow(d.wd),1],
                               "1 hour"), las=2, format="%m/%d %H:%M")
    
    for(j in 3:ncol(d.wd)){
      par(new = T)
      plot(d.wd$Time,d.wd[,j],xlab="", ylab="",
              ylim = c(0,max(d.wd[,2:ncol(d.wd)])),col=j -1,type = "l", xaxt="n")
    }
    par(xpd=T)
    legend(par()$usr[1],par()$usr[4]+70,colnames(d.ws)[2:ncol(d.ws)],lty=1,
           ncol =ncol(d.ws)-1,col=1:(ncol(d.ws)-1))
    legend((par()$usr[1]+par()$usr[2])/2,par()$usr[4]+70,
           paste("ev",eventperiod$Event,sep = ""),lty=1,lwd = 2,
           col =colpalette[2:(nrow(eventperiod)+1)],ncol=nrow(eventperiod))
    par(xpd=F)
    abline(h = 120)
    abline(h = 240)
    
    # time --------------------------------------------------------------------
    
    # Time
    timescale <- NULL
    # timescale <- data.frame(time = 1:length(d$Time), value = rep(4,length(d$Time)))
    # 時刻、昼・夜、イベント判定（イベント期間以外が0）
    timescale <- data.frame(time = d$Time, value = rep(2,length(d$Time)), event = rep(0,length(d$Time)))
    ## 昼を4に（赤色）(夜を2)
    timetest<-ymd_hms(d$Time)
    timetest<- hms(paste(hour(timetest),minute(timetest),second(timetest),sep=":"))
    sunrise <- hm("05:42")
    sunset <- hm("20:09")
    timescale$value[timetest>sunrise & timetest<sunset] <- 4

    for (j in 1:nrow(eventperiod)) {
      timescale$event[timescale$time > eventperiod$Start[j] & 
                        timescale$time < eventperiod$End[j]] <- j 
    }
    
    # timescale$value[timetest>sunrise & timetest<sunset] <- 0
    plot(timescale$time,timescale$value,ylim=c(0,3),type = "h",xlab="",ylab="timescale",
         xaxt = "n",col = 6-timescale$value)
    par(new = T)
    plot(timescale$time,rep(1,nrow(timescale)),col = colpalette[timescale$event+1],xlab = "",ylim=c(0,3), 
         ylab = "", type = "h", xaxt = "n")
    axis.POSIXct(side=1,at=seq(d$Time[1], d$Time[length(timescale$time)],
                               "1 hour"), las=2, format="%m/%d %H:%M")
    
    dev.copy(pdf, file=paste(averate[k],"_", dname,".pdf",sep=""), width = 20, height = 10)
    dev.off()
  }
  
  write.csv(result.df, paste(sub("/avebyn","",path),"/", averate[k],"_sumdata.csv", sep = ""),row.names=FALSE)
  
}
