######イベンドごとの風配図
library("fmsb")
path <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
setwd(path)


# イベント数
nevent <- 4
# 風速区切り
windbr <- c(0,5,10,13,15,20)
# 平均時間
avetime <- 60

# データ読み込み
WRdata <- read.csv(paste("Ev_",avetime,"_sumdata.csv",sep=""),header = T)
WRdata <- WRdata[,c(1,2,3,8,11,14)]
events <- levels(as.factor(WRdata$Event))
events <- events[-length(events)]
sites <- levels(WRdata$SiteID)
aveWD <- data.frame("site"=NA,"event"=NA,"avarageWS"=NA,"avarageWD"=NA)

for (a in 1:length(events)) {
  for (b in 1:length(sites)) {
    temp.WRdata <- WRdata[WRdata$Event == events[a] & WRdata$SiteID == sites[b],]
    if (nrow(temp.WRdata) != 0) {
      
      # 風配図作成
      counttime <- nrow(temp.WRdata)
      wr.break <- seq(0, 360, length = 33)
      Windrose <- rbind(numeric(16),1:16)
      colnames(Windrose) <- c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W"
                              ,"WNW","NW","NNW")
      rownames(Windrose) <- c("du0","du1")
      for (i in (length(windbr)-1):1) {
        temp<- hist(temp.WRdata$WD_h[temp.WRdata$WS_h >= windbr[i] &
                                  temp.WRdata$WS_h <= windbr[i+1] ],
                    breaks=wr.break)
        tempcounts <- NULL
        for (j in 1:16) {
          if (j ==1) {
            tempcounts <- c(tempcounts,temp$counts[1]+temp$counts[16])
          }else{
            tempcounts <- c(tempcounts,temp$counts[2*(j-1)]+temp$counts[2*j-1])
          }
        }
        
        Windrose <- rbind(Windrose,tempcounts*100/counttime)
        dimnames(Windrose) <- list(c(rownames(Windrose)[1:nrow(Windrose)-1],
                                     paste(windbr[i],"~",windbr[i+1],"m/s",sep="")),
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
      dev.copy(pdf, file=paste(path2,"/Windrose/Windrose_",sites[b],"_Ev",a,
                              ".pdf",sep=""), width = 10, height = 10)
      dev.off()
      
      # 平均風向（ベクトル平均）算出
      temp.WRdata$WD_h <- 90 - temp.WRdata$WD_h 
      temp.WRdata$vx <- temp.WRdata$WS_h * cos(pi*temp.WRdata$WD_h/180)
      temp.WRdata$vy <- temp.WRdata$WS_h * sin(pi*temp.WRdata$WD_h/180)
      aveWind <- c(sum(temp.WRdata$vx),sum(temp.WRdata$vy))/nrow(temp.WRdata)
      if (aveWind[1]< 0) {
        aveWind <- c(sqrt(aveWind[1]^2+aveWind[2]^2),
                     180*atan(aveWind[2]/aveWind[1])/pi + 180)
      }else{
        aveWind <- c(sqrt(aveWind[1]^2+aveWind[2]^2),
                     180*atan(aveWind[2]/aveWind[1])/pi)
      }
      aveWind[2] <- 90 - aveWind[2]
      
      aveWD <- rbind(aveWD,c(sites[b],events[a],aveWind))
    }
  }
}


Windrose <- rbind(1:16,Windrose)
Windrose <- cbind(nrow(Windrose):1,Windrose)

Windrose <- Windrose[order(Windrose[,1]),]

Windrose <-Windrose[,2:ncol(Windrose)]

plot.new()
legend(0,0.1, row.names(Windrose[1:5,]),lty=1,col=1:5,cex=1.2,ncol=5,lwd = 2)
dev.copy(pdf, file=paste(path2,"/","WrLegend.pdf",sep=""), width = 10, height = 10)
dev.off()

write.csv(aveWD, paste(path2,"/AveWind_sumdata.csv",sep = ""),row.names=FALSE)

