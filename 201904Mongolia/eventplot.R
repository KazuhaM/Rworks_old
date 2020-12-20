library(lubridate)
# init --------------------------------------------------------------------
# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
path <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# 
path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
setwd(path)
# 
eventperiod <- read.csv(paste(path2,"EventPeriod.csv",sep="/"))
eventperiod$Start <- as.POSIXct(eventperiod$Start)
eventperiod$End <- as.POSIXct(eventperiod$End)

tempevent <- as.POSIXct(c(
  "2019/5/10 20:30:00", #　イベント4の境界。ここを境に風向が変わる
  "2019/5/3 20:00:00", #　イベント2の境界。ここを境に風向が変わる
  "2019/5/4 2:00:00",  #　気温湿度から推定したイベント2の終わり
  "2019/5/11 1:45:00",
  "2019/5/11 2:15:00"
  ))

sttime<- unclass(eventperiod)$Start[unclass(unclass(eventperiod)$Start)==min(unclass(unclass(eventperiod)$Start))]
entime<- unclass(eventperiod)$End[unclass(unclass(eventperiod)$End)==max(unclass(unclass(eventperiod)$End))]
fultime <- seq(sttime, entime,"1 min")

timescale <- data.frame(time = fultime, value = rep(2,length(fultime)), event = rep(0,length(fultime)))
## 昼を4に（赤色）(夜を2)
timetest<-ymd_hms(fultime)
timetest<- hms(paste(hour(timetest),minute(timetest),second(timetest),sep=":"))
sunrise <- hm("05:42")
sunset <- hm("20:09")
timescale$value[timetest>sunrise & timetest<sunset] <- 4

for (j in 1:nrow(eventperiod)) {
  timescale$event[timescale$time > eventperiod$Start[j] & 
                    timescale$time < eventperiod$End[j]] <- j 
}

par(mar = c(8,4,4,2))

plot(timescale$time,timescale$value,ylim=c(0,3),type = "h",xlab="",ylab="timescale",
     xaxt = "n",col = 6-timescale$value)
par(new = T)
plot(timescale$time,rep(1,nrow(timescale)),col = colpalette[timescale$event+1],xlab = "",ylim=c(0,3), 
     ylab = "", type = "h", xaxt = "n")
abline(v = tempevent,col = cm.colors(1:length(tempevent)*2))
axis.POSIXct(side=1,at=seq(fultime[1], fultime[length(fultime)],
                           "1 hour"), las=2, format="%m/%d %H:%M")
par(mar = c(5,4,4,2))


#################sf_time_event
layout(matrix(c(1,1,2), nrow = 3, ncol = 1, byrow = TRUE))
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
setwd(path3)

data.timeseries <- read.csv("Z0Us_MoM_60_sumdata.csv")
data.timeseries$SiteID <-as.factor(data.timeseries$SiteID)
data.timeseries$Time <- as.POSIXct(data.timeseries$Time)

plotperiod <- as.POSIXct(c(
  "2019/5/3 8:00:00", #　描画範囲開始
  "2019/5/3 20:00:00" #　描画範囲終了
))
data.timeseries <- data.timeseries[data.timeseries$Time>=plotperiod[1]&data.timeseries$Time<=plotperiod[2],]
# data.timeseries <- data.timeseries[data.timeseries$SiteID!="B3",]

plot(data.timeseries$Time,data.timeseries$SF_sl,ylim=c(0,20),type = "h",xlab="",ylab="saltation flux",
     xaxt = "n",col = data.timeseries$SiteID,xlim= c(plotperiod[1],plotperiod[2]),cex = 1.5)
axis.POSIXct(side=1,at=seq(plotperiod[1], plotperiod[2],
                           "1 hour"), las=2, format="%m/%d %H:%M")
abline(v = as.POSIXct("2019/5/3 9:15:00"),lwd = 2)
plot(timescale$time,rep(1,nrow(timescale)),col = colpalette[timescale$event+1],xlab = "",ylim=c(0,1), 
     ylab = "", type = "h", xaxt = "n")

par(xpd=T)
legend(par()$usr[1],par()$usr[3],
       paste("ev",eventperiod$Event,sep = ""),lty=1,lwd = 2,
       col =colpalette[2:(nrow(eventperiod)+1)],ncol=nrow(eventperiod))
par(xpd=F)
