
# Init --------------------------------------------------------------------
library(tcltk2)
library(tidyverse)
library(furrr)
plan(multisession)
options(scipen=5)

# data --------------------------------------------------------------------


# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"

# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
# path3 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1102春期モンゴル解析3/roughness"

# path3 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1401春期モンゴル解析4"
# path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
# path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1301MongoliaAnalysis8/NsiteRecul" #Nsite only

setwd(path3)
# averate <- c("60","180","300","600","1800")
averate <- c("60")
# averate <- c("60","600")
rateNo <- 1

# 一度に見るDの桁数
reptimes <- 1000
############################臨界風速等算出###################

# データファイル読み込み
# sf_filename = paste(path3,"/Z0Us_MoM_",averate[rateNo],"_sumdata_d_0.csv",sep="")
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
result.df2 <- data.frame(matrix(rep(NA, 11), nrow=1))[numeric(0), ]
df.colnames <- c("SiteID","Event","E", "Ut", "D","Z0","d0","R","AveDev","SdDev","n")
colnames(result.df2) <-df.colnames
result.df2$SiteID <- as.character(result.df2$SiteID)
result.df2$Event <- as.character(result.df2$Event)
result.df2$E <- as.numeric(result.df2$E)
result.df2$Ut <- as.numeric(result.df2$Ut)
result.df2$c <- as.numeric(result.df2$D)
result.df2$Z0 <- as.numeric(result.df2$Z0)
result.df2$AveDev <- as.numeric(result.df2$AveDev)
result.df2$n <- as.numeric(result.df2$n)

par(mfrow=c(1,1))
# 値cのサイトごとの初期値
# cseq <- c(0.000000001,0.00000001,0.0000001,0.000001,0.00001,0.0001,0.001,0.01,0.1,1)
cseq <- 10^(-20:20)
# # icsq <-c(2,3,3,6,6,6,6,6,3,4,3,3,3,0,3,2,3,2,2,3,4,4,3,2,3,3,4)+1
# icsq <- rep(17,30) +c(3,	4,	1,	3,	3,	3,	4,	4,	3,	1,	1,	0,	2,	2,	1,	0,	0,	0,	0,	2,	0,	3,	2,	5,	4,	0,	-1,	5,	2,	3
# 
# 
# 
# 
# )

## for N site only
icsq <- rep(17,5) +c(-1,-1,2,2,0

)

i <- 1
# サイト、イベントごとに解析
for(j in 1:length(sitelev)){
  for (k in 1:length(eventlev)) {
    # sum_sigE <- data.frame(matrix(rep(NA, 3), nrow=1))[numeric(0), ] #一時計算結果格納データフレーム作成
    # colnames(sum_sigE) <- c("E", "iUt", "iD")
    
    temp.d <- sf.d[sf.d$SiteID == sitelev[j] & sf.d$Event == eventlev[k],] #該当のサイト－イベントのデータを取り出す
    if(nrow(temp.d)!=0 & sum(!is.na(temp.d$Us))!=0){ #該当するデータがなかった場合、U_*が求められなかったサイトの場合を除く
      temp.d <- temp.d[order(temp.d$Us),]
      temp.d <- temp.d[!is.na(temp.d$Us),]
      temp.d["dev_n"] <- NaN
      
      qmodel.f <- function(iD, iut, us = temp.d$Us , sf = temp.d$SF_sl){
        qm <- iD*(1-(iut/us)^2)*us^3
        e = (sf - qm)^2
      }
      
      c_iD <- seq(cseq[icsq[i]],reptimes *cseq[icsq[i]],by = cseq[icsq[i]])
      c_iut <- seq(0.01,max(temp.d$Us),by = 0.01)
      c_iD2 <- as.vector(mapply(rep,c_iD,length(c_iut)))
      i_data <- data.frame(iD = c_iD2,iut = c_iut)
        i_data1 <- i_data[1:(nrow(i_data)/2),]
        i_data2 <- i_data[(nrow(i_data)/2 + 1):nrow(i_data),]
      
      c_e1 <- future_pmap(i_data1,qmodel.f, .progress = T)
      c_e2 <- future_pmap(i_data2,qmodel.f, .progress = T)
      c_E1 <- future_map(c_e1,sum)
      c_E2 <- future_map(c_e2,sum)
      c_E1 <- purrr::flatten_dbl(c_E1)
      c_E2 <- purrr::flatten_dbl(c_E2)
      c_E <- c(c_E1,c_E2)
      
      
      sum_sigE <- cbind(c_E,i_data)
      colnames(sum_sigE) <- c("E", "iD","iUt")
      
      min.E <- sum_sigE[which.min(sum_sigE$E),]
      
      plot(E~iD,data = sum_sigE)
      points(min.E$iD,min.E$E,col=2,pch=21,bg = 2)

      # サイト内の飛砂計間のばらつきを算出
      temp.d$dev_n[temp.d$PC_10_1==99999] <- 
          sqrt(((temp.d$PC_10_2[temp.d$PC_10_1==99999] -
            (temp.d$PC_10_2[temp.d$PC_10_1==99999]+temp.d$PC_10_3[temp.d$PC_10_1==99999])/2)^2 +
           (temp.d$PC_10_3[temp.d$PC_10_1==99999] -
              (temp.d$PC_10_2[temp.d$PC_10_1==99999]+temp.d$PC_10_3[temp.d$PC_10_1==99999])/2)^2)/
          (2*(temp.d$PC_10_2[temp.d$PC_10_1==99999]+temp.d$PC_10_3[temp.d$PC_10_1==99999])^2/4))
      
      temp.d$dev_n[temp.d$PC_10_1!=99999] <- 
          sqrt((temp.d$PC_10_1[temp.d$PC_10_1!=99999] -
           (temp.d$PC_10_1[temp.d$PC_10_1!=99999]+temp.d$PC_10_2[temp.d$PC_10_1!=99999]+
              temp.d$PC_10_3[temp.d$PC_10_1!=99999])/3)^2 +
          ((temp.d$PC_10_2[temp.d$PC_10_1!=99999] -
              (temp.d$PC_10_1[temp.d$PC_10_1!=99999]+temp.d$PC_10_2[temp.d$PC_10_1!=99999]+
                 temp.d$PC_10_3[temp.d$PC_10_1!=99999])/3)^2 +
             (temp.d$PC_10_3[temp.d$PC_10_1!=99999] -
                (temp.d$PC_10_1[temp.d$PC_10_1!=99999]+temp.d$PC_10_2[temp.d$PC_10_1!=99999]+
                   temp.d$PC_10_3[temp.d$PC_10_1!=99999])/3)^2)/
          (3*(temp.d$PC_10_1[temp.d$PC_10_1!=99999]+temp.d$PC_10_2[temp.d$PC_10_1!=99999]+
                temp.d$PC_10_3[temp.d$PC_10_1!=99999])^2/9))
      
      t.2 <-data.frame(sitelev[j],eventlev[k],as.numeric(min.E$E),
                       as.numeric(min.E$iUt), 
                       as.numeric(min.E$iD),
                       as.numeric(temp.d$Z0[1]) ,
                       as.numeric(temp.d$d0[1]) ,
                       as.numeric(temp.d$R[1]) ,
                       as.numeric(mean(temp.d$dev_n[!is.na(temp.d$dev_n)])),
                       as.numeric(sd(temp.d$dev_n[!is.na(temp.d$dev_n)])),
                       as.numeric(nrow(temp.d)))
      colnames(t.2) <- df.colnames
      result.df2 <- rbind(result.df2,t.2)
      i <- i +1
    }
    
    print(paste(j,"番目のサイト",k,"番目のイベント",sep=""))
  }   
}
# write.csv(result.df2, paste(path3,"/Ust_MoM_",averate[rateNo],"_d_0.csv", sep = ""),row.names=FALSE)
write.csv(result.df2, paste(path3,"/Ust_MoM_",averate[rateNo],".csv", sep = ""),row.names=FALSE)



#####サイト-イベントごとのusのNA率確認#########
# sf.d
na.rate <- data.frame(matrix(rep(NA, 4), nrow=1))
na.colname <- c("SiteID","Event","n", "NotA")

colnames(na.rate) <- na.colname
na.rate <- na.rate[-1,]
for(isite in sitelev){
  for (iev in eventlev) {
    temp.na <- sf.d[sf.d$SiteID == isite & sf.d$Event == iev,]
    if(nrow(temp.na) != 0){
      na.rate <- rbind(na.rate, c(isite, iev, nrow(temp.na),
                                  sum(is.na(temp.na$Us))))
      
      colnames(na.rate) <- na.colname
    }
  }
}
na.rate$n <- as.integer(na.rate$n)
na.rate$NotA <- as.integer(na.rate$NotA)

na.rate["NArate"] <- 100 * na.rate$NotA / na.rate$n
write.csv(na.rate, paste(path3,"/Us_NArate_check.csv", sep = ""),row.names=FALSE)


