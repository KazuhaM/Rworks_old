
# library -----------------------------------------------------------------
library(lubridate) #時系列データ用
library("RColorBrewer") # 透明度のある色用


# Initialize --------------------------------------------------------------
path <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"

path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1301MongoliaAnalysis8/Plot_Dust_summary"
setwd(path2)

averate <- c("600")

# ダストデータ
dust.data <- read.csv(paste(path2,"DustMergeData.csv",sep= "/"),header = T)
dust.data$time <- as.POSIXct(dust.data$time, tz = "Asia/Ulaanbaatar")
dust.data$DustTrak.2..mg.m3.[dust.data$Wind.Speed.m.s. < 7.5] <- NA
dust.data$DustTrak.2..mg.m3.[dust.data$DustTrak.2..mg.m3.<0] <- NA


# サイトデータ
site.data <- read.csv(paste(path,"/Sf_MoM_",averate,"_sumdata.csv",sep = ""),header = T)
site.data$Time <- as.POSIXct(site.data$Time, tz = "Asia/Ulaanbaatar")
site.data$SiteID <- as.factor(site.data$SiteID)
site.data$Event <- as.factor(site.data$Event)
# str(site.data)
# 旧イベントデータ
eventperiod <- read.csv(paste(path,"recul_event","EventPeriod2.csv",sep="/"))
eventperiod$Start <- as.POSIXct(eventperiod$Start, tz = "Asia/Ulaanbaatar")
eventperiod$End <- as.POSIXct(eventperiod$End, tz = "Asia/Ulaanbaatar")

colpalette <- c("black","chartreuse","orange","cyan2","blueviolet","pink","wheat","salmon2")

sites <- levels(site.data$SiteID)
# sites <- sites[c(22,23,24)]

# PLOT --------------------------------------------------------------------

for(i_site in sites){
  # i_site <- sites[3]
  
  # 該当サイトデータ抽出
  temp.site.d <- site.data[site.data$SiteID == i_site,]
  
  # 該当サイト調査期間データ抽出
  temp.min.time <- min(temp.site.d$Time)
  temp.max.time <- max(temp.site.d$Time)
  # temp.min.time <- as.POSIXct("2019/4/28 07:00:00",tz = "Asia/Ulaanbaatar")
  # temp.max.time <- as.POSIXct("2019/4/28 18:30:00",tz = "Asia/Ulaanbaatar")
  
  temp.timeseries <- seq(temp.min.time,temp.max.time, paste(averate," sec",sep=""))
  
  # Mergeデータ作成
  temp.mergedata <- data.frame(time = temp.timeseries)
  temp.mergedata <- merge(temp.mergedata,dust.data,by.x = "time",by.y = "time",
                          all.x = TRUE, all.y = FALSE)
  
  temp.mergedata <- merge(temp.mergedata,temp.site.d,by.x = "time",by.y = "Time",
                          all.x = TRUE, all.y = FALSE)
  # colnames(temp.mergedata)
  
  ### plot
  layout(matrix(c(1,1,2,2,3,3,4,4,5,5,6,6), nrow = 12, ncol = 1, byrow = TRUE))

  par(mar = c(0.5,8,2.5,6),family = family_serif)
  ## ダストデータ
  # 軸作成
  plot(SF_sl ~ time, xlab = "", ylab = "",xaxt = "n", yaxt = "n",data = temp.mergedata,
       col=adjustcolor("white", alpha=0),
       xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]),
       main = i_site)
  axis.POSIXct(side=1,at=seq(temp.timeseries[1], temp.timeseries[length(temp.timeseries)],
                             "6 hour"), las=2,label = "", format="%m/%d %H:%M",tck=1.0,lwd = 1,lty = 2,
               adjustcolor("gray", alpha=0.5))
  axis.POSIXct(side=1,at=seq(temp.timeseries[1], temp.timeseries[length(temp.timeseries)],
                             "1 hour"), labels = "",las=2,tck=1.0,lwd = 1,lty = 3,col = "gray",
               adjustcolor("gray", alpha=0.5))
  # saltation flux
  par(new =T)
  plot(SF_sl~time,xlab="", ylab="",
       axes = F,data = temp.mergedata, col=1,type = "h",
       xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]))
  axis(2,  line =5)
  mtext("Saltation flux",side = 2,line = 7,cex  = 0.8)

  # Dust plot
  par(new =T)
  plot(DustTrak.2..mg.m3.~time,xlab="", ylab="Dust (mg/m3)",
       type="p", xaxt="n",data = temp.mergedata, col=2,
       xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]))

  # wind speed in the main site
  par(new =T)
  plot(Wind.Speed.m.s.~time,xlab="",ylab ="",type = "l",
       axes  = F, data = temp.mergedata, col = 4,
       xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]))
  mtext("Wind speed (m/s)",side = 4, line = 3) 
  axis(4)
  
  ## wind speed
  # 軸
  ws.colname <- c("WS_h","WS_m","WS_l")
  plot(WS_h~time,xlab="", ylab="Wind Speed(m/s)",type="l", xaxt="n",data = temp.mergedata,
       col=adjustcolor("white", alpha=0),
       ylim = c(0,max(temp.mergedata[,ws.colname],na.rm = T)),
       xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]))
  axis.POSIXct(side=1,at=seq(temp.timeseries[1], temp.timeseries[length(temp.timeseries)],
                             "6 hour"), las=2, labels = "",tck=1.0,lwd = 1,lty = 2,
               adjustcolor("gray", alpha=0.5))
  axis.POSIXct(side=1,at=seq(temp.timeseries[1], temp.timeseries[length(temp.timeseries)],
                             "1 hour"), labels = "",las=2,tck=1.0,lwd = 1,lty = 3,col = "gray",
               adjustcolor("gray", alpha=0.5))
  # abline(h=8.0)
  
  # プロット
  for(i_height in 1:length(ws.colname)){
    par(new = T)
    eval(parse(text=paste(
      "plot(",ws.colname[i_height],"~time,xlab=\"\", ylab=\"\",type=\"l\",
            xaxt=\"n\",data = temp.mergedata,col=",i_height,
                    ",ylim = c(0,max(temp.mergedata[,ws.colname],na.rm = T)),
       xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]))",sep="")))
  }
  
  ## wind direction
  # 軸
  wd.colname <- c("WD_h","WD_m","WD_l")
  plot(WD_h~time,xlab="", ylab="Wind direction (deg)",type="l", xaxt="n",data = temp.mergedata,
       ylim = c(0,360),col=adjustcolor("white", alpha=0),
       xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]))
  axis.POSIXct(side=1,at=seq(temp.timeseries[1], temp.timeseries[length(temp.timeseries)],
                             "6 hour"), las=2, labels = "",tck=1.0,lwd = 1,lty = 2,
               adjustcolor("gray", alpha=0.5))
  axis.POSIXct(side=1,at=seq(temp.timeseries[1], temp.timeseries[length(temp.timeseries)],
                             "1 hour"), labels = "",las=2,tck=1.0,lwd = 1,lty = 3,col = "gray",
               adjustcolor("gray", alpha=0.5))
  abline(h = 90,lty = 2,col = "gray")
  abline(h = 180,lty = 2,col = "gray")
  abline(h = 270,lty = 2,col = "gray")
  
  # プロット
  for(i_height in 1:length(wd.colname)){
    par(new = T)
    eval(parse(text=paste(
      "plot(",wd.colname[i_height],"~time,xlab=\"\", ylab=\"\",type=\"l\",
            xaxt=\"n\",data = temp.mergedata,
            xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]),
                    ylim = c(0,360),col=",i_height,")",sep="")))
  }
  par(xpd=T)
  legend(par()$usr[1], par()$usr[4]+45,
         legend = c("h","m","l"),lty = 1,
         col = 1:3,ncol = 3,cex=0.8)
  par(xpd=F)
  
  ## temperature
  # 軸
  tp.colname <- c("T_h","T_m","T_l")
  plot(T_h~time,xlab="", ylab="air temperature (K)",type="l", xaxt="n",data = temp.mergedata,
       col=adjustcolor("white", alpha=0),
       ylim = c(min(temp.mergedata[,tp.colname],na.rm = T),max(temp.mergedata[,tp.colname],na.rm = T)),
       xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]))
  axis.POSIXct(side=1,at=seq(temp.timeseries[1], temp.timeseries[length(temp.timeseries)],
                             "6 hour"), las=2, labels = "",tck=1.0,lwd = 1,lty = 2,
               adjustcolor("gray", alpha=0.5))
  axis.POSIXct(side=1,at=seq(temp.timeseries[1], temp.timeseries[length(temp.timeseries)],
                             "1 hour"), labels = "",las=2,tck=1.0,lwd = 1,lty = 3,col = "gray",
               adjustcolor("gray", alpha=0.5))
  abline(h = 273.15,lty = 2,col = "gray")
  mtext(273.15,side = 2,line = -1, at = 273.15,cex = 0.5)
  
  # プロット
  # 気温差分
  tp.delta <- c("Dh_m","Dm_l")
  for(i_height in 1:length(tp.delta)){
    par(new = T)
    eval(parse(text=paste(
      "plot(",tp.delta[i_height],"~time,xlab=\"\", ylab=\"\",type=\"l\",
            xaxt=\"n\",yaxt=\"n\",data = temp.mergedata,col=adjustcolor(colpalette[",i_height+2,"],alpha=0.5)
      ,ylim = c(min(temp.mergedata[,tp.delta],na.rm = T),max(temp.mergedata[,tp.delta],na.rm = T)),
       xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]))",sep="")))
  }
  axis(4)
  mtext("delta air temp. (K)",side = 4,line = 3)
  abline(h = -1,lty = 2,col = "gray")
  abline(h = 0.5,lty = 2,col = "gray")
  mtext(-1,side = 4,line = -1,at = -1,cex = 0.5)
  mtext(0.5,side = 4,line = -1,at = 0.5,cex = 0.5)
  
  # 気温
  for(i_height in 1:length(tp.colname)){
    par(new = T)
    eval(parse(text=paste(
      "plot(",tp.colname[i_height],"~time,xlab=\"\", ylab=\"\",type=\"l\",
            xaxt=\"n\",data = temp.mergedata,col=",i_height,
      ",ylim = c(min(temp.mergedata[,tp.colname],na.rm = T),max(temp.mergedata[,tp.colname],na.rm = T)),
       xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]))",sep="")))
  }
  par(xpd=T)
  legend(par()$usr[1], par()$usr[4]+4,
         legend = c("h-m","m-l" ),lty = 1,
         col = colpalette[3:4],ncol = 2,cex=0.8,lwd = 2)
  par(xpd=F)
  
  ## humidity
  # 軸
  hm.colname <- c("H_h","H_m","H_l")
  plot(H_h~time,xlab="", ylab="Relative humidity (%)",type="l", xaxt="n",data = temp.mergedata,
       col=adjustcolor("white", alpha=0),
       ylim = c(0,max(temp.mergedata[,hm.colname],na.rm = T)),
       xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]))
  axis.POSIXct(side=1,at=seq(temp.timeseries[1], temp.timeseries[length(temp.timeseries)],
                             "6 hour"), las=2, labels = "",tck=1.0,lwd = 1,lty = 2,
               adjustcolor("gray", alpha=0.5))
  axis.POSIXct(side=1,at=seq(temp.timeseries[1], temp.timeseries[length(temp.timeseries)],
                             "1 hour"), labels = "",las=2,tck=1.0,lwd = 1,lty = 3,col = "gray",
               adjustcolor("gray", alpha=0.5))

  # プロット
  for(i_height in 1:length(hm.colname)){
    par(new = T)
    eval(parse(text=paste(
      "plot(",hm.colname[i_height],"~time,xlab=\"\", ylab=\"\",type=\"l\",
            xaxt=\"n\",data = temp.mergedata,col=",i_height,
      ",ylim = c(0,max(temp.mergedata[,hm.colname],na.rm = T)),
       xlim=c(temp.timeseries[1], temp.timeseries[length(temp.timeseries)]))",sep="")))
  }
  
  ### event
  # Time
  timescale <- NULL
  # timescale <- data.frame(time = 1:length(d$Time), value = rep(4,length(d$Time)))
  # 時刻、昼・夜、イベント判定（イベント期間以外が0）
  timescale <- data.frame(time = temp.timeseries,
                          value = rep(2,length(temp.timeseries)),
                          event = rep(0,length(temp.timeseries)))
  ## 昼を4に（赤色）(夜を2)
  timetest<- ymd_hms(temp.timeseries)
  timetest<- hms(paste(hour(timetest),minute(timetest),second(timetest),sep=":"))
  sunrise <- hm("05:42") #TsOのSum centerでの大体4/31頃の日の出
  sunset <- hm("20:09")
  timescale$value[timetest>=sunrise & timetest<sunset] <- 4
  
  for (j in 1:nrow(eventperiod)) {
    timescale$event[timescale$time >= eventperiod$Start[j] & 
                      timescale$time <= eventperiod$End[j]] <- j 
  }

  par(mar = c(5,8,1.5,6))
  plot(value~time,ylim=c(0,3),type = "h",xlab="",ylab="timescale",data = timescale,
       xaxt = "n",col = 6-timescale$value)
  par(new = T)
  plot(timescale$time,rep(1,nrow(timescale)),
       col = colpalette[timescale$event+1],
       xlab = "",ylim=c(0,3), 
       ylab = "", type = "h", xaxt = "n")
  axis.POSIXct(side=1,at=seq(temp.timeseries[1], temp.timeseries[length(temp.timeseries)],
                             "6 hour"), las=2, format="%m/%d %H:%M",tck=1.0,lwd = 1,lty = 2,
               adjustcolor("gray", alpha=0.5))
  axis.POSIXct(side=1,at=seq(temp.timeseries[1], temp.timeseries[length(temp.timeseries)],
                             "1 hour"), labels = "",las=2,tck=1.0,lwd = 1,lty = 3,col = "gray",
               adjustcolor("gray", alpha=0.5))
  par(xpd=T)
  legend(par()$usr[1],par()$usr[4]+0.4,
         paste("ev",eventperiod$Event,sep = ""),lty=1,lwd = 2,
         col =colpalette[2:(nrow(eventperiod)+1)],ncol=nrow(eventperiod),cex = 0.6)
  # abline(v = as.POSIXct("2019/4/28 13:15",tz = "Asia/Ulaanbaatar"))
  par(xpd=F)
  
  dev.copy(cairo_pdf, file=paste("dust_ev_",i_site,".pdf",sep=""), width = 10, height = 15)
  # dev.copy(cairo_pdf, file=paste("dust_ev1_",i_site,".pdf",sep=""), width = 10, height = 15)
  dev.off()
  
}
