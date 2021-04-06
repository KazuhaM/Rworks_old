library(tcltk2)

# path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
# path4 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
path4 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1301MongoliaAnalysis8/NsiteRecul" #Nsite only


setwd(path4)
# z0,d0算出用の平均単位時間
zdaverate <- c("60","180","300","600","1800")
# zdaverate <- c("60","600")
# zdaverate <- c("600")
averate <- c("60","600")
# averate <- c("60")
# 風速計の1サイトあたりの数（全サイトで等しい場合）
nJ <- 3

# z0,d0算出用の平均単位時間選択用変数
l <- 4



# 各サイト-イベントについてz0,d0のリストを取得
zd_filename = paste(path4,"/Z0_MoM_",zdaverate[l],"_sumdata.csv",sep="")
zd.d <- read.csv(zd_filename,header=T)
zd.d$SiteID <- as.factor(zd.d$SiteID)
SiteIndex <- levels(zd.d$SiteID)
columnlist <- c("SiteID", "Event", "Z0", "d0","R","n") #make columnList
zd.table <- data.frame(matrix(nrow=nrow(zd.d))) #make empty dataframe
for (i in 1:length(columnlist)){
  zd.table <- cbind(zd.table, zd.d[, columnlist[i]]) #bind data by referring columnList
}
zd.table <- zd.table[, -1] 
colnames(zd.table) <- columnlist
zd.table <- cbind(zd.table, paste(zd.table$SiteID,zd.table$Event,sep=""))　# サイト－イベント判別子作成
zd.table <- zd.table[!is.na(zd.table$Z0),]　#z0が算出されていないイベント、イベント外を除く
zd.table <- zd.table[!duplicated(zd.table[,7]),] #重複解除
zd.table <- zd.table[, 1:6] #判別子除外

# Us, Ust算出用の単位時間平均データについて、Us, Ustを再算出
# イベント期間中は全データについて算出。ただし、z0, d0が算出できなかったサイト-イベントを除く
for(i in 1:length(averate)){
  ev_filename = paste(path4,"/Z0_MoM_",averate[i],"_sumdata.csv",sep="")
  ev.d <- read.csv(ev_filename,header=T)
  ev.d$Z0 <- NA 
  ev.d$d0 <- NA 
  ev.d$R <- NA 
  ev.d$n <- NA 
  ev.d$Us <- NA 
  # SiteIndex <- levels(ev.d$SiteID)
  pbk <- txtProgressBar(min = 1, max = nrow(ev.d), style = 3)
  print(averate[i]) 
  for(j in 1:nrow(ev.d)){
    if(ev.d[j,"Event"] != 99 & 
       !(ev.d$WS_h[j] < ev.d$WS_m[j] & ev.d$WS_m[j] < ev.d$WS_l[j])){
      row.ev <- ev.d[j,"Event"]
      row.site <- ev.d[j,"SiteID"]
      if(sum(zd.table$SiteID==row.site & zd.table$Event == row.ev) !=0){
        row.z <- zd.table[zd.table$SiteID==row.site & zd.table$Event == row.ev,"Z0"]
        row.d <- zd.table[zd.table$SiteID==row.site & zd.table$Event == row.ev,"d0"]
        row.r <- zd.table[zd.table$SiteID==row.site & zd.table$Event == row.ev,"R"]
        row.n <- zd.table[zd.table$SiteID==row.site & zd.table$Event == row.ev,"n"]
        ev.d[j,"Z0"] = row.z
        ev.d[j,"d0"] = row.d
        ev.d[j,"R"] = row.r
        ev.d[j,"n"] = row.n
        
        # Usを切片固定の単回帰で算出。高さの値から算出
        # log((z - d0)/z0)を予め計算した上で、Uと切片なしで単回帰。推定された係数で0.4を割ってUs
        iws <- c(ev.d$WS_h[j],ev.d$WS_m[j],ev.d$WS_l[j])
        ih <- c(ev.d$Height_WM_h[j],ev.d$Height_WM_m[j],ev.d$Height_WM_l[j])
        
        ih <- log((ih-row.d)/row.z)

        tmp.result <- lm(ih ~ iws-1)
        ev.d[j,"Us"] = 0.4/as.numeric(tmp.result$coefficients[1])
      }
    }
    setTxtProgressBar(pbk, j)
  }
  write.csv(ev.d, paste(path4,"/Z0Us_MoM_",averate[i],"_sumdata.csv",sep=""),row.names=FALSE)
  
}
      