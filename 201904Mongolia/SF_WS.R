#作業ディレクトリ変更
setwd("E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0501第4回ゼミ発表/解析/Summed")


Filen2 <- c("B2l","N2-2","S1-3","W2-3")
for(q in 1:4){
data <- read.csv(paste(Filen2[q],".csv", sep=""))
data$X <- as.POSIXct(data$X)
time_start <- as.POSIXct("2019-5-5 17:49:00")
time_end <- as.POSIXct("2019-5-10 9:14:00")
height <- as.numeric(sub("X", "",c(colnames(data)[7],colnames(data)[9],colnames(data)[11])))/100


########Summary - Time
par(xaxt="n",mfrow = c(2,1),yaxt = "s")
data3 <- data[data[,1]>time_start & data[,1]<time_end ,]

plot(data3$X,data3[,2],type = "h",xlab="",ylab="DriftingSands(n/s)",
     ylim=c(0,max(c(data3[,2],data3[,3],data3[,4],data3[,5]))),col = 1,
     main = Filen2[q])
par(new=T,yaxt = "n")
plot(data3$X,data3[,3],type = "h",xlab="",ylab="",
     ylim=c(0,max(c(data3[,2],data3[,3],data3[,4],data3[,5]))),col = 2)
par(new=T,yaxt = "n")
plot(data3$X,data3[,4],type = "h",xlab="",ylab="",
     ylim=c(0,max(c(data3[,2],data3[,3],data3[,4],data3[,5]))),col = 4)
par(new=T,yaxt = "n")
plot(data3$X,data3[,5],type = "h",xlab="",ylab="",
     ylim=c(0,max(c(data3[,2],data3[,3],data3[,4],data3[,5]))),col = 3)
par(xaxt="s")
axis.POSIXct(1,at=seq(time_start,time_end,"hour"),format="%m/%d %H")
legend(time_start,max(c(data3[,2],data3[,3],data3[,4],data3[,5]))*0.9,
       paste("PC",sub("X", "",colnames(data)[2:5]),c(rep("_height:10",3),"_height:50"),sep=""),
       lty=1 , col =c(1,2,4,3))

par(xaxt="n",yaxt = "s")
data3 <- data[data[,1]>time_start & data[,1]<time_end ,]
plot(data3$X,data3[,6],type = "l",xlab="Time",ylab="WindSpeed(m/s)",col=1,
     ylim = c(0,15))
par(new=T,yaxt = "n")
data3 <- data[data[,1]>time_start & data[,1]<time_end ,]
plot(data3$X,data3[,8],type = "l",xlab="",ylab="",col=2,
     ylim = c(0,15))
par(new=T,yaxt = "n")
data3 <- data[data[,1]>time_start & data[,1]<time_end ,]
plot(data3$X,data3[,10],type = "l",xlab="",ylab="",col=4,
     ylim = c(0,15))
par(xaxt="s")
axis.POSIXct(1,at=seq(time_start,time_end,"hour"),format="%m/%d %H")
legend(time_start,15*0.95,
       paste("WM",sub("X", "",c(colnames(data)[6],colnames(data)[8],colnames(data)[10])),
             "_height:",height,sep=""),lty=1 , col =c(1,2,4,3))


dev.copy(pdf, file=paste(Filen2[q],"_sum.pdf",sep=""), width = 20, height = 10)
dev.off()
par(xaxt="s",mfrow = c(1,1),yaxt = "s")


########DriftingSands - WindSpeed
plot(data3[,8],data3[,2],xlab=paste("WindSpeed(m/s,",height[2],"m height)",sep=""),
     ylab="DriftingSands(n/s)",xlim = c(0,15),
     ylim=c(0,max(c(data3[,2],data3[,3],data3[,4],data3[,5]))),col=1)
par(new=T,yaxt = "n")
plot(data3[,8],data3[,3],xlab="",ylab="",xlim = c(0,15),
     ylim=c(0,max(c(data3[,2],data3[,3],data3[,4],data3[,5]))),col=2)
par(new=T,yaxt = "n")
plot(data3[,8],data3[,4],xlab="",ylab="",xlim = c(0,15),
     ylim=c(0,max(c(data3[,2],data3[,3],data3[,4],data3[,5]))),col=4)
par(new=T,yaxt = "n")
plot(data3[,8],data3[,5],xlab="",ylab="",xlim = c(0,15),
     ylim=c(0,max(c(data3[,2],data3[,3],data3[,4],data3[,5]))),col=3)
legend(0,max(c(data3[,2],data3[,3],data3[,4],data3[,5]))*0.9,
       paste("PC",sub("X", "",colnames(data)[2:5]),c(rep("_height:10",3),"_height:50"),sep=""),
       pch=1 , col =c(1,2,4,3))
dev.copy(pdf, file=paste(Filen2[q],"_DS_WS.pdf",sep=""), width = 10, height = 10)
dev.off()

}


#Roughness ---------------------------------------------------------------------
#############30分平均
data2 <- data[!is.na(data[,13]) & !is.na(data[,14]) & !is.na(data[,15]),]
p <- 0
for(i in 1:nrow(data2)){
  if (i == 1) {
    tempdata <- cbind(c(data2[i,13],data2[i,14],data2[i,15]),height)
    plot(tempdata,type="b",xlim =c(0,max(max(data2[,13]),max(data2[,14]),max(data2[,15]))),
         ylim = c(0.01,10),log="y",xlab="",ylab="")
    temp.result <- lm(log10(tempdata[,2]) ~ tempdata[,1])
    abline(temp.result) 
  }else if(sum(data2[i,13],data2[i,14],data2[i,15])>0 && 
           max(c(data2[i,13],data2[i,14],data2[i,15])) >= 8 &&
           !(data2[i,13] == data2[i,14] & data2[i,15] == data2[i,14]) ){
    par(new=T)
    tempdata <- cbind(c(data2[i,13],data2[i,14],data2[i,15]),height)
    plot(tempdata,type="b",xlim =c(0,max(max(data2[,13]),max(data2[,14]),max(data2[,15]))),
         ylim = c(0.01,10),log="y",xlab="",ylab="",pch = i%%25)
    temp.result <- lm(log10(tempdata[,2]) ~ tempdata[,1])
    abline(temp.result) 
    p <- p+1
  }
}
#############5分平均
data2 <- data[!is.na(data[,6]) & !is.na(data[,8]) & !is.na(data[,10]),]
p <- 0
for(i in 1:nrow(data2)){
  if (i == 1) {
    tempdata <- cbind(c(data2[i,6],data2[i,8],data2[i,10]),height)
    plot(tempdata,type="b",xlim =c(0,max(max(data2[,6]),max(data2[,8]),max(data2[,10]))),
         ylim = c(0.01,10),log="y",xlab="",ylab="")
    temp.result <- lm(log10(tempdata[,2]) ~ tempdata[,1])
    abline(temp.result) 
  }else if(sum(data2[i,6],data2[i,8],data2[i,10])>0 && 
           max(c(data2[i,6],data2[i,8],data2[i,10])) >= 8 &&
           !(data2[i,6] == data2[i,8] & data2[i,10] == data2[i,8]) ){
    par(new=T)
    tempdata <- cbind(c(data2[i,6],data2[i,8],data2[i,10]),height)
    plot(tempdata,type="b",xlim =c(0,max(max(data2[,6]),max(data2[,8]),max(data2[,10]))),
         ylim = c(0.01,10),log="y",xlab="",ylab="",pch = i%%25)
    temp.result <- lm(log10(tempdata[,2]) ~ tempdata[,1])
    abline(temp.result) 
    p <- p+1
  }
}