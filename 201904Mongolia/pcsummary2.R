setwd("E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0501第4回ゼミ発表/解析")


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