######イベンドごとの風配図
library("fmsb")
library("RColorBrewer")
# path <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
# path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
# setwd(path)

path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
setwd(path2)

# 風速区切り
windbr <- c(0,5,10,13,15,20,25)
# 平均時間
avetime <- 60

col.dir.name <- c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W"
                  ,"WNW","NW","NNW")
label.dir <- seq(360-22.5, 22.5, by = -22.5)
label.dir <- as.character(c(0,label.dir))

# データ読み込み
WRdata <- read.csv(paste(path2,"/recul_event/Ev_",avetime,"_sumdata.csv",sep=""),header = T)
WRdata <- WRdata[,c(1,2,3,8,11,14)]
events <- levels(as.factor(WRdata$Event))
events <- events[-length(events)]
WRdata$SiteID <- as.factor(WRdata$SiteID)
sites <- levels(WRdata$SiteID)
aveWD <- data.frame("site"=NA,"event"=NA,"avarageWS"=NA,"avarageWD"=NA)

Avsite <- read.csv("AvailableSites.csv",header =T)

for (a in 1:length(events)) {
  for (b in 1:length(sites)) {
    temp.id <- paste(sites[b],"_",events[a],sep ="")
    temp.WRdata <- WRdata[WRdata$Event == events[a] & WRdata$SiteID == sites[b],]
    
    if(sum(Avsite$ID == temp.id) != 0 & nrow(temp.WRdata) != 0){

      # 風配図作成
      counttime <- nrow(temp.WRdata)
      wr.break <- seq(0, 360, length = 33)
      Windrose <- rbind(numeric(16),1:16)
      colnames(Windrose) <- col.dir.name
      rownames(Windrose) <- c("du0","du1")
      for (i in (length(windbr)-1):1) {
        # par(family = "")
        temp<- hist(temp.WRdata$WD_h[temp.WRdata$WS_h >= windbr[i] &
                                  temp.WRdata$WS_h < windbr[i+1] ],
                    breaks=wr.break,plot = FALSE)
        tempcounts <- NULL
        for (j in 1:16) {
          if (j ==1) {
            tempcounts <- c(tempcounts,temp$counts[1]+temp$counts[32])
          }else{
            tempcounts <- c(tempcounts,temp$counts[2*(j-1)]+temp$counts[2*j-1])
          }
        }
        
        Windrose <- rbind(Windrose,tempcounts*100/counttime)
        dimnames(Windrose) <- list(c(rownames(Windrose)[1:nrow(Windrose)-1],
                                     paste("(",windbr[i],", ",windbr[i+1],"]",sep="")),
                                   col.dir.name)
      }
      
      Windrose <- t(Windrose)
      Windrose <- Windrose[order(-Windrose[,2]),]
      Windrose <- t(Windrose)
      Windrose <-cbind(Windrose[,16],Windrose[,1:(ncol(Windrose)-1)])
      colnames(Windrose) <- c("N","NNW","NW","WNW","W","WSW","SW","SSW","S","SSE","SE","ESE",
                              "E","ENE","NE","NNE")
      
      Windrose <-Windrose[3:nrow(Windrose),]
      
      # ラベルの間隔を決定
      if(max(Windrose) / 5 >=5){
        windlab = seq(0,ceiling(max(Windrose) / 10) * 10,by = 10)
        windseg = max(windlab) / 10
      }else{
        windlab = seq(0,ceiling(max(Windrose) / 5) * 5,by = 5)
        windseg = max(windlab) / 5
      }
      
      # 最大最小をデータフレーム一行目、二行目に挿入
      wr.min <- numeric(16)
      wr.max <- rep(max(windlab),length=16)
      
      Windrose = rbind(wr.max,wr.min,Windrose)
      Windrose <- data.frame(Windrose)
      
      # paste(windlab,"%",sep = "")

      # 描画
      if(temp.id == "N1-4_1"){
        # oldpar <- par(no.readonly = TRUE) 
        par(mar = c(2,2,2,2),xpd=F,family = family_serif)
        layout(matrix(c(1,1,5,5,6,6,
                        1,1,5,5,6,6,
                        2,2,7,7,8,8,
                        2,2,7,7,8,8,
                        3,3,9,9,10,10,
                        3,3,9,9,10,10,
                        4,4,11,11,12,12,
                        4,4,11,11,12,12), nrow = 8, ncol = 6, byrow = TRUE))
      }
      radarchart(rbind(Windrose[1:2,],rep(0,16)), axistype = 0, seg = windseg,
                 pty=32,pcol = adjustcolor("white", alpha=0),
                 cglcol = "lightgray", cglty = 1,cglwd = 1.3,
                 vlcex = 1.3, centerzero = TRUE, vlabels = colnames(Windrose),
                 title = temp.id,cex.main = 1.5
                 )
      text(0,-seq(0,1,length = length(windlab)),paste(windlab," %",sep = ""),cex = 1)
      par(new = T)
      radarchart(Windrose, axistype = 0, seg = windseg,  
                 pty=32,plwd=2.5 ,plty= 6:1, pcol = 6:1,
                 cglwd=1.3, cglcol = adjustcolor("white", alpha=0), cglty = 1,
                 vlcex = 1.3,
                 axislabcol = 1,
                 centerzero = TRUE, vlabels = "",
                 title = NULL
      )
      par(new = F)
      # dev.copy(cairo_pdf, file=paste(path2,"/windrose/Windrose_",sites[b],"_Ev",a,
      #                         ".pdf",sep=""), width = 10, height = 10)
      # dev.off()
      # par(oldpar) 
      
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
      
      print(sum(Windrose[3:nrow(Windrose),]))
    }
  }
}


Windrose <- rbind(1:16,Windrose)
Windrose <- cbind(nrow(Windrose):1,Windrose)

Windrose <- Windrose[order(Windrose[,1]),]

Windrose <-Windrose[,2:ncol(Windrose)]


radarchart(rbind(Windrose[1:2,],rep(0,16)), axistype = 0, seg = 0,
           pty=32,pcol = adjustcolor("white", alpha=0),
           cglcol = 1, cglty = 1,cglwd = 1.3,
           vlcex = 1.3, centerzero = TRUE, vlabels = label.dir,
           title = "Azimuth (degree)",cex.main = 1.5
)


plot(NULL, ann = F)
legend(0,0.5,title = expression(paste(" Wind speed (m ",s^-1,")",sep="")),yjust=0.5 ,title.adj = 0,
       row.names(Windrose[1:5,]),col=1:6,cex=1.2,ncol=1,lwd = 2,lty = 1:6,text.width=0.3)

dev.copy(cairo_pdf, file=paste(path2,"/windrose/","WrLegend_2.pdf",sep=""), width = 9, height = 12)
dev.off()

# write.csv(aveWD, paste(path2,"/AveWind_sumdata.csv",sep = ""),row.names=FALSE)

