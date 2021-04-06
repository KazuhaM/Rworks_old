
# init --------------------------------------------------------------------
library(lubridate)

# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/temp_avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
path <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/temp_avebyn"
# 
path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
setwd(path)


# averate <- c("60","180","300","600","1800")
averate <- c("60","300")

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
  # result.df <- data.frame(matrix(rep(NA, n), nrow=1))[numeric(0), ]
  # colnames(result.df) <- c("Time", "SiteID", "Event", "PC_10_1", "PC_10_2", "PC_10_3", "PC_50_1",
  #                          "T_h", "T_m", "T_l", "H_h", "H_m", "H_l",
  #                          "Height_WM_h", "Height_WM_m", "Height_WM_l")
  # result.df$Time <- as.POSIXlt(result.df$Time)
  for(i in 1 : length(d.flist300)){
    d <- read.csv(d.flist300[i],header=T)
    d$Time <- as.POSIXct(d$Time)
    dname <- strsplit(d.flist300[i], "_")
    dname <- sub(".csv", "", dname[[1]][3])
    d[,grep("T_",colnames(d))] <- d[,grep("T_",colnames(d))] + 273.15
    
    d$Event <- rep(0,nrow(d)) 
    for (j in 1:nrow(eventperiod)) {
      d$Event[d$Time > eventperiod$Start[j] & 
                        d$Time < eventperiod$End[j]] <- j 
    }
    for(l in 1:nrow(eventperiod)){
      d.ev <- d[d$Event == l,]
      if(nrow(d.ev) !=0){
      
        d.pc <- d.ev[,c(1, grep("PC",colnames(d.ev)))]
        d.pc <- d.pc[, colnames(d.pc) != "PC18_50"]
        d.ws <- d.ev[,c(1, grep("T_",colnames(d.ev)))]
        d.wd <- d.ev[,c(1, grep("H_",colnames(d.ev)))]
        
        # height.wm <- as.numeric(sub("T_","",colnames(d.ws)[2:length(colnames(d.ws))]))
        # height.wm.mat <- matrix(0,nrow(d),length(height.wm))
        # for(j in 1 : nrow(d)){
        #   height.wm.mat[j,] <- height.wm
        # }
        
        
        # if(ncol(d.pc) == 4){
        #   temp.df <- cbind(d[,1], rep(dname,nrow(d)),rep(1,nrow(d)),rep(99999,nrow(d)),
        #                    d.pc[2:ncol(d.pc)],d.ws[2:ncol(d.ws)],d.wd[2:ncol(d.wd)],height.wm.mat)
        # }else if(ncol(d.pc) == 5){
        #   temp.df <- cbind(d[,1], rep(dname,nrow(d)),rep(1,nrow(d)),
        #                    d.pc[2:ncol(d.pc)],d.ws[2:ncol(d.ws)],d.wd[2:ncol(d.wd)],height.wm.mat)
        # }
        # colnames(temp.df) <- colnames(result.df)
        # result.df = rbind(result.df,temp.df)
    
    # fig ---------------------------------------------------------------------
    
    
        # par(mfrow=c(3,1))
        layout(matrix(c(1,1,2,2,3,3,4), nrow = 7, ncol = 1, byrow = TRUE))
        #distance of Temperature between heigher and lower  
        d.ws.dist <- d.ws
        for(j in 2:(ncol(d.ws)-1)){
          tempcolname <- colnames(d.ws.dist)
          tempcolname <- c(tempcolname,paste(tempcolname[j],"-",tempcolname[j+1],sep = ""))
          d.ws.dist <- cbind(d.ws.dist,(d.ws[,j] - d.ws[,j+1]))
          colnames(d.ws.dist) <- tempcolname
        }
        d.ws.dist <- d.ws.dist[,c(1,(ncol(d.ws)+1):ncol(d.ws.dist))]
        
        matplot(d.ws.dist[,1],d.ws.dist[,2],xlab="", ylab="Temperature(K)",type="l", xaxt="n",
                ylim = c(min(d.ws.dist[,2:ncol(d.ws.dist)])*1.2,max(d.ws.dist[,2:ncol(d.ws.dist)])*1.2),
                col=1, main=paste(averate[k],dname,"event",d.ev$Event[1]))
        axis.POSIXct(side=1,at=seq(d.ws.dist[1,1], d.ws.dist[nrow(d.ws.dist),1],
                            "1 hour"), las=2, format="%m/%d %H:%M")
        # par(new=T)
        # matplot(d.ws.dist[,1],d.ws.dist[,2],xlab="", ylab="Temperature(K)",type="h", xaxt="n",
        #         ylim = c(min(d.ws.dist[,2:ncol(d.ws.dist)])*1.2,max(d.ws.dist[,2:ncol(d.ws.dist)])*1.2),col=1)
        abline(h = 0,lty=3)
        abline(h = -0.8,lty=3)
        abline(h = -0.0098*0.5,lty=3)
        mtext(0, side = 2, line = 0, at = 0,cex = 0.6)
        mtext(-0.8, side = 2, line = 0, at = -0.8,cex = 0.6)
        mtext(-0.0098*0.5, side = 2, line = 0, at = -0.0098*0.5,cex = 0.6)
        for(j in 3:ncol(d.ws.dist)){
          par(new = T)
          matplot(d.ws.dist[,1]+j,d.ws.dist[,j],xlab="", ylab="",type="l", xaxt="n",
                  ylim =  c(min(d.ws.dist[,2:ncol(d.ws.dist)])*1.2,max(d.ws.dist[,2:ncol(d.ws.dist)])*1.2),col=j -1)
        }
        # legend(max(as.ts(d[,1]))*0.9,max(d.ws.dist[,2:ncol(d.ws.dist)]),colnames(d.ws.dist)[2:ncol(d.ws.dist)],lty=1,
               # col =1:(ncol(d.ws)-1))
    
    # temperature -------------------------------------------------------------
    
        
        #Temperature
        matplot(d.ws[,1],d.ws[,2],xlab="", ylab="Temperature(K)",type="l", xaxt="n",
                ylim = c(min(d.ws[,2:ncol(d.ws)]),max(d.ws[,2:ncol(d.ws)])),col=1)
        axis.POSIXct(side=1,at=seq(d.ws[1,1], d.ws[nrow(d.ws),1],
                                   "1 hour"), las=2, format="%m/%d %H:%M")
        # par(new = T)
        # matplot(d.ws[,1],d.ws[,2],xlab="", ylab="Temperature(K)",type="h", xaxt="n",
                # ylim = c(min(d.ws[,2:ncol(d.ws)]),max(d.ws[,2:ncol(d.ws)])),col=grey(0.5),lty=2 )
        for(j in 3:ncol(d.ws)){
          par(new = T)
          matplot(d.ws[,1],d.ws[j],xlab="", ylab="",type="l", xaxt="n",
                  ylim = c(min(d.ws[,2:ncol(d.ws)]),max(d.ws[,2:ncol(d.ws)])),col=j -1)
        }
        abline(h =273.15)
        
    
    # humidity ----------------------------------------------------------------
    
        
        #humidity
        matplot(d.wd[,1],d.wd[2],xlab="", ylab="Relative humidity(%)",type = "l", xaxt = "n",
                ylim = c(0,max(d.wd[,2:ncol(d.wd)])),col=1)
        # axis.POSIXct(side=1,at=seq(d.wd[1,1], d.wd[nrow(d.wd),1], 
                                   # "1 hour"), las=2, format="%m/%d %H:%M")
        for(j in 3:ncol(d.wd)){
          par(new = T)
          matplot(d.wd[,1],d.wd[j],xlab="", ylab="",type = "l", xaxt = "n",
                  ylim = c(0,max(d.wd[,2:ncol(d.wd)])),col=j -1)
        }
        # legend(max(d[,1]),max(d.wd[,2:ncol(d.wd)]),colnames(d.wd)[2:ncol(d.wd)],lty=1,
               # col =1:(ncol(d.wd)-1))
        # abline(h = 120)
        # abline(h = 240)
        par(xpd=T)
        legend(par()$usr[1],par()$usr[3],colnames(d.ws)[2:ncol(d.ws)],lty=1,
               ncol =ncol(d.ws)-1,col=1:(ncol(d.ws)-1))
        legend((2*par()$usr[1]+par()$usr[2])/3,par()$usr[3],colnames(d.ws.dist)[2:ncol(d.ws.dist)],lty=1,
               col =1:(ncol(d.ws)-1),ncol =ncol(d.ws)-2)
        legend((par()$usr[1]+2*par()$usr[2])/3,par()$usr[3],
               paste("ev",eventperiod$Event,sep = ""),lty=1,lwd = 2,
               col =colpalette[2:(nrow(eventperiod)+1)],ncol=nrow(eventperiod))
        par(xpd=F)
        
    
    # time --------------------------------------------------------------------
        
        # Time
        timescale <- NULL
        # timescale <- data.frame(time = 1:length(d$Time), value = rep(4,length(d$Time)))
        # 時刻、昼・夜、イベント判定（イベント期間以外が0）
        timescale <- data.frame(time = d.ev$Time, value = rep(2,length(d.ev$Time)), event = rep(0,length(d.ev$Time)))
        ## 昼を4に（赤色）(夜を2)
        timetest<-ymd_hms(d.ev$Time,tz ="Asia/Ulaanbaatar")
        timetest<- hms(paste(hour(timetest),minute(timetest),second(timetest),sep=":"))
        sunrise <- hm("05:42")
        sunset <- hm("20:09")
        timescale$value[timetest>sunrise & timetest<sunset] <- 4
        
        # # イベント期間を表示
        # eventperiod <- read.csv(paste(path2,"EventPeriod.csv",sep="/"))
        # eventperiod$Start <- as.POSIXct(eventperiod$Start)
        # eventperiod$End <- as.POSIXct(eventperiod$End)
        
    
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
        axis.POSIXct(side=1,at=seq(d.ev$Time[1], d.ev$Time[length(timescale$time)],
                                   "1 hour"), las=2, format="%m/%d %H:%M")
        
        # cut_loca <- locator(2)
        # 描画
        
        dev.copy(pdf, file=paste("Ev_",averate[k],"_", dname,"_",l,".pdf",sep=""), width = 20, height = 10)
        dev.off()
      }
    }
  }
  
  # write.csv(result.df, paste(sub("/temp_avebyn","",path),"/temp", averate[k],"_sumdata.csv", sep = ""),row.names=FALSE)
  
}

