path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
path3 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1401春期モンゴル解析4"


setwd(path2)

test.Wpc.sd <- read.csv("SfZ0Us_60_sumdata.csv", header = T)
test.Wpc.sd <- test.Wpc.sd[grep("W.",test.Wpc.sd$SiteID),]
test.Wpc.sd$Event[test.Wpc.sd$Event == 99] <-0
Wnames <- c("W2-3","W3-3","W3-4","W2-5")

for(i in 1: 4){
  temp.test <- test.Wpc.sd[test.Wpc.sd$SiteID==Wnames[i],]
  temp.test$Time <-as.ts(temp.test$Time)
  par(mfrow=c(5,1))
  ts.plot(temp.test$PC_10_1, gpars=list(xlab="days", ylab="Saltation flux",ylim = c(0, max(temp.test[,18:20])), col=1, main = Wnames[i]))
  ts.plot(temp.test$PC_10_2, gpars=list(xlab="days", ylab="Saltation flux",ylim = c(0, max(temp.test[,18:20])), col=2))
  ts.plot(temp.test$PC_10_3, gpars=list(xlab="days", ylab="Saltation flux",ylim = c(0, max(temp.test[,18:20])), col=3))
  
    ts.plot(temp.test$WD_h, gpars=list(xlab="days", ylab="Wind direction",ylim = c(0, 360), col=1))
    par(new = T)
    ts.plot(temp.test$WD_m, gpars=list(xlab="days", ylab="Wind direction",ylim = c(0, 360), col=2))
    par(new = T)
    ts.plot(temp.test$WD_l, gpars=list(xlab="days", ylab="Wind direction",ylim = c(0, 360), col=3))
  
    ts.plot(temp.test$Event, gpars=list(xlab="days", ylab="event",ylim = c(0, 5), col=1))
    dev.copy(pdf, 
             file=paste(path3,"/Wpc_sd_60_", Wnames[i],".pdf",sep=""), 
             width = 10, height = 10)
    dev.off()
}