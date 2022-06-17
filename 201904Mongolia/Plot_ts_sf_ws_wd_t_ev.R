path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
setwd(path3)
averate <- c("60","600")
i <- 1

# 自作の色パレット
colpalette <- c("black","gold","seagreen4","coral4","sandybrown","deepskyblue1","mediumorchid3","white",
                "orangered","royalblue3")

# イベンド期間用データフレーム
eventperiod <- read.csv(paste(path2,"EventPeriod.csv",sep="/"))
eventperiod$Start <- as.POSIXct(eventperiod$Start)
eventperiod$End <- as.POSIXct(eventperiod$End)

sttime<- unclass(eventperiod)$Start[unclass(unclass(eventperiod)$Start)==min(unclass(unclass(eventperiod)$Start))]
entime<- unclass(eventperiod)$End[unclass(unclass(eventperiod)$End)==max(unclass(unclass(eventperiod)$End))]
fultime <- seq(sttime, entime,"1 min")

# イベンド期間用データフレーム
timescale <- data.frame(time = fultime, value = rep(10,length(fultime)), event = rep(0,length(fultime)))
## 昼を9に（赤色）(夜を10)
timetest<-ymd_hms(fultime)
timetest<- hms(paste(hour(timetest),minute(timetest),second(timetest),sep=":"))
sunrise <- hm("05:42")
sunset <- hm("20:09")
timescale$value[timetest>sunrise & timetest<sunset] <- 9

for (j in 1:nrow(eventperiod)) {
  timescale$event[timescale$time > eventperiod$Start[j] & 
                    timescale$time < eventperiod$End[j]] <- j 
}
timescale$event[timescale$event == 0] <- 7


ts_filename = paste("Z0Us_MoM_",averate[i],"_sumdata.csv",sep="")
st.d <- read.csv(ts_filename,header=T)
st.d$Time <- as.POSIXct(st.d$Time)


layout(matrix(c(1,1,2,2,3,3,4,4,4,5), nrow = 10, ncol = 1, byrow = TRUE))
par(mar = c(1,5,1,1))
plot(st.d$Time,st.d$SF_gs,ylim=c(0,max(st.d$SF_gs)*1.1),type = "h",xlab="",ylab="saltation flux (g/s m^2)",
     xaxt = "n",cex.axis=1.2, cex.lab=1.5,xlim= c(fultime[1],fultime[length(fultime)]),col = rainbow(24))
axis(side=1, at=seq(fultime[1], fultime[length(fultime)],"8 hour"), labels=F)

plot(st.d$Time,st.d$WS_h,ylim=c(0,max(st.d$WS_h)*1.1),type = "p",xlab="",ylab="wind speed (m/s)",
     xaxt = "n",cex.axis=1.2, cex.lab=1.5,xlim= c(fultime[1],fultime[length(fultime)]),col = 1,
     ps = 10,pch = 20)
axis(side=1, at=seq(fultime[1], fultime[length(fultime)],"8 hour"), labels=F)

plot(st.d$Time,st.d$WD_h,ylim=c(0,max(st.d$WD_h)*1.1),type = "p",xlab="",ylab="wind direction (deg.)",
     xaxt = "n",cex.axis=1.2, cex.lab=1.5,xlim= c(fultime[1],fultime[length(fultime)]),col = 1,
     ps = 10,pch = 20)
axis(side=1, at=seq(fultime[1], fultime[length(fultime)],"8 hour"), labels=F)

par(mar = c(9,5,1,1))
plot(st.d$Time,st.d$T_h,ylim=c(260,max(st.d$T_h)*1.1),type = "p",xlab="",ylab="temperature (K)",
     xaxt = "n",cex.axis=1.2, cex.lab=1.5,xlim= c(fultime[1],fultime[length(fultime)]),col = 1,
     ps = 10,pch = 20)
axis.POSIXct(side=1,at=seq(fultime[1], fultime[length(fultime)],
                           "8 hour"), las=2, format="%m/%d %H:%M",cex.axis = 1.2)
par(mar = c(3,5,0,1))
plot(timescale$time,rep(2,nrow(timescale)),col = colpalette[timescale$event+1],xlab = "",ylim=c(0,2), 
     ylab = "", type = "h", xaxt = "n",yaxt = "n")
par(new =  T)
plot(timescale$time,rep(1,nrow(timescale)),col = colpalette[timescale$value],xlab = "",ylim=c(0,2), 
     ylab = "", type = "h", xaxt = "n",yaxt = "n")

par(xpd=T)
legend(par()$usr[1],par()$usr[3],
       paste("event",eventperiod$Event,sep = ""),lty=1,lwd = 10,
       col =colpalette[2:(nrow(eventperiod)+1)],ncol=nrow(eventperiod))
legend((par()$usr[1]+par()$usr[2])/2,par()$usr[3],
       c("noon","night"),lty=1,lwd = 10,
       col =colpalette[9:10],ncol=nrow(eventperiod))
par(xpd=F)
