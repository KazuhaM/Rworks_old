d<-read.csv("SummaryAllData-19_2.csv",header=T)
d <- d[d[,6]!="",]
d <- d[!is.na(d$DriftSand),]
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

k=2
d2 <- d[d$P.C.No.==lev[[4]][k], ]
d2 <- d2[!is.na(d2[,1]),]

plot(d2$Date_Hour, d2$DriftSand,xlab="", ylab="(num)",ylim=c(0,ceiling(max(d$DriftSand[!is.na(d$DriftSand)])*1.2)),xlim=c(43307.4204861111,43315.5315972222),col =as.numeric(lev[[4]][k])+1,pch=as.numeric(lev[[4]][k])%/%8+1,type = "p")

for(k in 2 : length(lev[[4]])){  
  d2 <- d[d$P.C.No.==lev[[4]][k], ]
  d2 <- d2[!is.na(d2[,1]),]
  if(nrow(d2) == 0){
    next
  }
  par(new=T)
  plot(d2$Date_Hour, d2$DriftSand,xlab="", ylab="(num)",ylim=c(0,ceiling(max(d$DriftSand[!is.na(d$DriftSand)])*1.2)),xlim=c(43307.4239583333,43315.5315972222),col =as.numeric(lev[[4]][k])+1,pch=as.numeric(lev[[4]][k])%/%8+1,type = "p")

}
legend(locator(1), lev[[4]], col =as.numeric(lev[[4]])+1,pch=as.numeric(lev[[4]])%/%8+1)
