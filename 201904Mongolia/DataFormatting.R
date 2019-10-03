#作業ディレクトリ変更
setwd("E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0501第4回ゼミ発表/解析")

#パッケージ読み込み
library(tcltk)
library(stringr)
library(zoo)
library(xts)
#ファイル読み込み
filen<-read.csv("files.csv",header=F)
except<- read.csv("ExceptionPeriod.csv",header=T)
SE<-read.csv("PCWMStartEnd.csv",header=T)
timet<-read.csv("timetemp.csv",header=F)

# str(except)
# str(SE)
# levels(SE$SiteID)
# except[10,3]
# SE[3,]
# as.POSIXlt(except[10,3]):as.POSIXlt(except[10,3])
# as.POSIXlt(except[10,3])>as.POSIXlt(SE[3,8])
# min(as.POSIXlt(SE[,8]))
# max(as.POSIXlt(SE[,9]))
# as.POSIXlt(SE[3,8])
# as.POSIXlt(SE[3,9])
# timet <- timet[,7]
# timet<-as.POSIXlt(timet)
# tail(timet[timet > as.POSIXlt(SE[3,8]) & timet<as.POSIXlt(SE[3,9])])


#前準備
##データ型変更
###日時データ化
SE$Start <- as.POSIXlt(SE$Start)
SE$End <- as.POSIXlt(SE$End)
timet<-as.POSIXlt(timet[,7])
###文字列化
filen$V2 <- as.character(filen$V2)
filen$V3 <- as.character(filen$V3)
##サイト名一覧取得
SiteName <- levels(SE$SiteID)




# CalTop ------------------------------------------------------------------

#処理開始

# Site-TimeData -----------------------------------------------------------
##各サイトごとのサイト名、開始終了時刻とその間の10秒間隔時刻データ生成
###リスト準備
EachSite <- as.list(NULL)
###本文
pb <- txtProgressBar(min = 1, max = length(SiteName), style = 3)
for (i in 1:length(SiteName)) {
  MinSt <- min(SE[SE$SiteID == SiteName[i],8])
  MaxEn <- max(SE[SE$SiteID == SiteName[i],9])
  EachSite[[i]] <- list(SiteName[i],timet[timet > MinSt & timet<MaxEn])
  setTxtProgressBar(pb, i)
  gc();gc()
}

##Summaryデータ生成
###結果データフレーム準備
Result <- as.data.frame(NULL)
Rcln<- c("Time","Site","PC1","PC2","PC3","PC4","WS1","WS2","WS3","WD1","WD2",
                      "WD3","PC1n","PC2n","PC3n","PC4n","WM1n","WM2n","WM3n","PC1h","PC2h",
                      "PC3h","PC4h","WM1h","WM2h","WM3h","Event","SiteCover","Richness",
                      "Ave.Height","DominantAve.Height","Sum.Veg.Vol.","Sum.DominantVeg.Vol.",
                      "Veg.Type","DuneNo.")
Result <- rbind(rep(0,length(Rcln)),rep(0,length(Rcln)))
colnames(Result)  <- Rcln
pb <- txtProgressBar(min = 1, max = length(SiteName), style = 3)
###サイトごと
for (i in 1:length(SiteName) ){
  
  IdTime <- SE[SE$SiteID==SiteName[i],c("IDNo","Type","Start","End")] 
  ####結果一時格納データフレーム生成
  tempdata2<- as.data.frame(NULL)
  tempdata2<-data.frame(Time = as.data.frame(EachSite[[i]][[2]]),Site = as.data.frame(rep(i,length(EachSite[[i]][[2]]))))
  tempdata2 <- as.xts(read.zoo(tempdata2))
  ###サイト内機器ごと
  pb <- txtProgressBar(min = 1, max = length(IdTime$IDNo), style = 3)
  for (j in 1 : length(IdTime$IDNo)) {
    
    if (IdTime$Type[j] == "PC") {
      ###PC関連のcsvファイル全操作
      setwd(filen$V2[filen$V1=="PC"][[1]])
      
      for (k in 1 :length(filen$V2[filen$V1=="PC"])) {
        
        tempfn <- as.character(filen$V3[filen$V1=="PC"][[k]])
        tempData <-read.csv(file=tempfn, header=TRUE,na.strings=c("", "NULL"))
        tempcoln <- as.vector(colnames(tempData))
        colnames(tempData) <- c("Time",tempcoln[2:length(tempcoln)])
        tempData$Time <- as.POSIXlt(tempData$Time)
        tempcoln <- tempcoln[which(str_detect(tempcoln, pattern=
                                                paste(".",as.character(IdTime$IDNo[j]),sep=""))==T)]
        if (length(tempcoln) != 0) {
          tempdata3 <-  tempData[tempData[,1] <= IdTime$End[j] &
                                   tempData[,1] >= IdTime$Start[j],c("Time",tempcoln)]
          if (length(tempdata3[,1]) != 0) {
            tempdata3 <- as.xts(read.zoo(tempdata3))
            tempdata2<-merge(tempdata2, tempdata3,join = "left")
            
            
          }
        }
        
        gc();gc()
      }
      gc();gc();gc();gc()
      tempdata2 <- as.data.frame(tempdata2)

      while (ncol(tempdata2)>2) {
        for (k in 1:nrow(tempdata2)) {
          if(is.na(tempdata2[k,2]) ==T){
            tempdata2[k,2]<-tempdata2[k,3]
            
          }
        }
        tempdata2 <- cbind(tempdata2[,1:2],tempdata2[,3:ncol(tempdata2)])
      }

    }
    gc();gc();gc();gc()
    setTxtProgressBar(pb, j)
  }    
  gc();gc();gc();gc()
    
    else if (IdTime$Type == "WM"){
      
      
  }
  
  setwd(filen$V2[i])
  fn <- filen$V3[i]
  
  
  if (filen$V1 = "WM") {
    tempData$Time <- as.POSIXlt(tempData$Time)
    
  }else if (filen$V1 = "PC") {
    
  }

}
setwd("E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0501第4回ゼミ発表/解析")
