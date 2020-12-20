library(tcltk2)

path4 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
setwd(path4)
# averate <- c("60","180","300","600","1800")
averate <- c("60","600")
irate <- 1
filename <- paste(path4,"/Z0Us_MoM_",averate[irate],"_sumdata.csv",sep="")

msf.d <- read.csv(filename, header=T)

# サイトリスト
msf.d$SiteID <- as.factor(msf.d$SiteID)
sitelev <- levels(msf.d$SiteID)
# イベントリスト
msf.d$Event <- as.factor(msf.d$Event)
eventlev <- levels(msf.d$Event)[levels(msf.d$Event)!="99"]


result.df5 <- data.frame(matrix(rep(NA, 6), nrow=1))[numeric(0), ]
colnames(result.df5) <- c("SiteID","Event","meanSF","sdSF", "meanWS", "meanWD")

for(isite in sitelev){
for(iev in eventlev){
  # isite <- sitelev[1]
  # iev <- eventlev[1]
 temp.msf.d <- msf.d[msf.d$SiteID==isite & msf.d$Event == iev,] 
 # print(paste(isite,"_",iev,sep=""))
 if(nrow(temp.msf.d) != 0){
   temp.df5 <- data.frame(matrix(rep(NA, 6), nrow=1))[numeric(0), ]
   temp.msf.d <- temp.msf.d[!is.na(temp.msf.d$SF_sl),]
  temp.meanSF <- mean(temp.msf.d$SF_sl) 
  temp.sdSF <- sd(temp.msf.d$SF_sl)
  
  # 平均風向（ベクトル平均）算出
  temp.msf.d$WD_h <- pi*(90-temp.msf.d$WD_h)/180
  temp.msf.d$vx <- temp.msf.d$WS_h * cos(temp.msf.d$WD_h)
  temp.msf.d$vy <- temp.msf.d$WS_h * sin(temp.msf.d$WD_h)
  aveWind <- c(sum(temp.msf.d$vx),sum(temp.msf.d$vy))/nrow(temp.msf.d)
  if (aveWind[1]< 0) {
    aveWind <- c(sqrt(aveWind[1]^2+aveWind[2]^2),
                 180*atan(aveWind[2]/aveWind[1])/pi - 180)
  }else{
    aveWind <- c(sqrt(aveWind[1]^2+aveWind[2]^2),
                 180*atan(aveWind[2]/aveWind[1])/pi)
  }
  aveWind[2] <- 90 - aveWind[2]
  temp.df5 <- data.frame(isite,iev,temp.meanSF,temp.sdSF,aveWind[1],aveWind[2])
  result.df5 <- rbind(result.df5,temp.df5)
 }
}
}

colnames(result.df5) <- c("SiteID","Event","meanSF","sdSF", "meanWS", "meanWD")

write.csv(result.df5, paste(path4,"/meanSF_",averate[rateNo],".csv", sep = ""),row.names=FALSE)


