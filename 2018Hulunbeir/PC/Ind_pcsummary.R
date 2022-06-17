setwd("E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2018/現行資料/1403生態学会用中国解析3/Ut")


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

for (k in 1:length(lev[[4]])) {
  windows()
  par(mfrow=c(3,1),mar = c(2, 7, 4, 2)) 
  
  d2 <- d[d$P.C.No.==lev[[4]][k], ]
  d2 <- d2[!is.na(d2[,1]),]
  
  plot(d2$Date_Hour, d2$DriftSand,xlab="", ylab="DriftSands
       (n/s)",
       ylim=c(0,ceiling(max(d$DriftSand[!is.na(d$DriftSand)])*1.2)),
       xlim=c(43307.4204861111,43315.5315972222),type = "p",xaxt  = "n", cex.axis = 2,
       cex.lab= 2,main=paste("Site",lev[[4]][k],sep=""))
  
  event.t <- c(43308.3020833333, 43308.7326388889, 43309.6458333333, 43309.8472222222, 43311.4444444444,
               43311.6631944444, 43312.1666666666, 43312.7430555556, 43314.2881944444, 43315.2743055556)
  for (i in 1:5) {
    abline(v = event.t[2*i-1])
    abline(v = event.t[2*i])
  }
  
  
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
  
  dev.copy(pdf, file=paste("IndSummary_Site",lev[[4]][k],".pdf",sep=""), width = 20, height = 10)
  dev.off()
}


