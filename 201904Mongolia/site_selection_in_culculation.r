result.site.sum <- list()
# Cul_Z0_Us_MoM.r ---------------------------------------------------------

library(tcltk2)

# path4 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
path4 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
setwd(path4)
# averate <- c("60","180","300","600","1800")
# averate <- c("60","600")
averate <- c("600")
# 風速計の1サイトあたりの数（全サイトで等しい場合）
nJ <- 3



### 粗度、摩擦速度、地面修正量算出
# #粒径データ
# d50 <- read.csv("SiteParticle.csv",header=T)

result.site1 <- data.frame(matrix(rep(NA, 3), nrow=1))[numeric(0), ]
colname.site1 <-c("SiteID","Event","n")
colnames(result.site1) <- colname.site1

result.site2 <- data.frame(matrix(rep(NA, 3), nrow=1))[numeric(0), ]
colname.site2 <-c("SiteID","Event","n")
colnames(result.site2) <- colname.site2

for(i in 1:length(averate)){
  # for(i in 1:5){
  ev_filename = paste(path4,"/Sf_MoM_",averate[i],"_sumdata.csv",sep="")
  ev.d <- read.csv(ev_filename,header=T)
  ev.d["Z0"] <- NaN
  ev.d["Us"] <- NaN
  ev.d["d0"] <- NaN
  ev.d["R"] <- NaN
  ev.d["n"] <- NaN
  
  #粗度、摩擦速度算出
  ev.d$SiteID <- as.factor(ev.d$SiteID)
  SiteIndex <- levels(ev.d$SiteID)
  pbk <- txtProgressBar(min = 1, max = length(SiteIndex), style = 3)
  for(k in 1:length(SiteIndex)){
    for (j in 1:(length(levels(as.factor(ev.d$Event)))-1)){
      # 該当するサイトのデータのみ抽出
      temp.d2 <- ev.d[ev.d$SiteID==SiteIndex[k] & ev.d$Event == j,]
      if (nrow(temp.d2) > 1) {
        
        result.site1 <- rbind(result.site1,c(k,j,nrow(temp.d2)))
        #風向のデータが微妙なので風速のデータのみから中立条件を仮定
        temp.d2 <- temp.d2[
          !(temp.d2$WS_h < temp.d2$WS_m & temp.d2$WS_m < temp.d2$WS_l) & #風速の逆転が起きていない
            temp.d2$WS_l >= 8 & #風速が8m/s以上である
            temp.d2$Event != 99 & #イベント期間中である
            temp.d2$Dh_m < 0.5 & temp.d2$Dm_l < 0.5 & #気温が反転していない
            temp.d2$Dh_m > -1 & temp.d2$Dm_l > -1 #気温差が一定値以下である
          ,] 
        
        nI <- nrow(temp.d2)
        if (nI > 1 ) {
          result.site2 <- rbind(result.site2,c(k,j,nI))
          
          setTxtProgressBar(pbk, k)
        }
      }
    }
  }
  # write.csv(ev.d, paste(path4,"/Z0_MoM_",averate[i],"_sumdata.csv", sep = ""),row.names=FALSE)
  # # write.csv(ev.d, paste(path3,"/Z0Us_MoM_",averate[i],"_sumdata_d_0.csv", sep = ""),row.names=FALSE)
  # print(averate[i]) 
}

colnames(result.site1) <- colname.site1
colnames(result.site2) <- colname.site2
result.site1$SiteID <-SiteIndex[result.site1$SiteID] 
result.site2$SiteID <-SiteIndex[result.site2$SiteID] 

result.site1 # 最初の時点であったもの
result.site2 # 中立条件を一つも満たさなかったサイトを除いたもの

result.site.sum$first <- result.site1
result.site.sum$nomalcondition <- result.site2

# ThresholdFV_MoM.r -------------------------------------------------------

library(tcltk2)
options(scipen=5)
# path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
path3 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
setwd(path3)

averate <- c("60")

rateNo <- 1

############################臨界風速等算出
result.site3 <- data.frame(matrix(rep(NA, 3), nrow=1))[numeric(0), ]
colname.site3 <-c("SiteID","Event","n")
colnames(result.site3) <- colname.site3

# データファイル読み込み
sf_filename = paste(path3,"/Z0Us_MoM_",averate[rateNo],"_sumdata.csv",sep="")

sf.d <- read.csv(sf_filename,header=T)
# データファイル基礎調整wo
# サイトリスト
sf.d$SiteID <- as.factor(sf.d$SiteID)
sitelev <- levels(sf.d$SiteID)
# イベントリスト
sf.d$Event <- as.factor(sf.d$Event)
eventlev <- levels(sf.d$Event)[levels(sf.d$Event)!="99"]

# 結果用データフレーム用意
result.df2 <- data.frame(matrix(rep(NA, 10), nrow=1))[numeric(0), ]
colnames(result.df2) <- c("SiteID","Event","E", "Ut", "c","Z0","d0","R","AveDev","SdDev")
result.df2$SiteID <- as.character(result.df2$SiteID)
result.df2$Event <- as.character(result.df2$Event)
result.df2$E <- as.numeric(result.df2$E)
result.df2$Ut <- as.numeric(result.df2$Ut)
result.df2$c <- as.numeric(result.df2$c)
result.df2$Z0 <- as.numeric(result.df2$Z0)
result.df2$AveDev <- as.numeric(result.df2$AveDev)

# サイト、イベントごとに解析
for(j in 1:length(sitelev)){
  for (k in 1:length(eventlev)) {
    temp.d <- sf.d[sf.d$SiteID == sitelev[j] & sf.d$Event == eventlev[k],] #該当のサイト－イベントのデータを取り出す
    if(nrow(temp.d)!=0 & sum(!is.na(temp.d$Us))!=0){ #該当するデータがなかった場合、U_*が求められなかったサイトの場合を除く
      result.site3 <- rbind(result.site3,c(j,k,nrow(temp.d)))
    }

  }   
}

colnames(result.site3) <- colname.site3
result.site3$SiteID <-SiteIndex[result.site3$SiteID] 

result.site3 # u*算出が1つもなかったサイト

result.site.sum$noustar <- result.site3




# ファイル出力 ------------------------------------------------------------------

# write(result.site.sum, paste(path3,"/SiteSelection_inCul.csv",sep=""), append = T)
