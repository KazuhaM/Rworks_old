library(tcltk2)

# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"

path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
# path3 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1102春期モンゴル解析3/roughness"
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
setwd(path2)
# averate <- c("60","180","300","600","1800")
averate <- c("60","300")
# 風速計の1サイトあたりの数（全サイトで等しい場合）
nJ <- 3

############################臨界風速等算出###################

#粒径データ
d50 <- read.csv("SiteParticle.csv",header=T)

for(i in 1:length(averate)){
# for(i in 1:5){
  ev_filename = paste("Ev_",averate[i],"_sumdata.csv",sep="")
  ev.d <- read.csv(ev_filename,header=T)
  ev.d["SF_gs"] <- NaN
  ev.d["SF_gs_1"] <- NaN
  ev.d["SF_gs_2"] <- NaN
  ev.d["SF_gs_3"] <- NaN
  ev.d["SF_sl"] <- NaN
  ev.d["SF_sl_1"] <- NaN
  ev.d["SF_sl_2"] <- NaN
  ev.d["SF_sl_3"] <- NaN
  ev.d["Z0"] <- NaN
  ev.d["Us"] <- NaN
  ev.d["d0"] <- NaN
  ev.d["R"] <- NaN
  ev.d["n"] <- NaN
  ev.d$Height_WM_h <- ev.d$Height_WM_h/100
  ev.d$Height_WM_m <- ev.d$Height_WM_m/100
  ev.d$Height_WM_l <- ev.d$Height_WM_l/100
  
  pbj <- txtProgressBar(min = 1, max = nrow(ev.d), style = 3)
  
  for(j in 1:nrow(ev.d)){
    #サルテーションフラックス算出
    iSiteD50 <- d50[d50[,1]==ev.d$SiteID[j],]
    
    p1 <- ev.d$PC_10_1[j]
    p2 <- ev.d$PC_10_2[j]
    p3 <- ev.d$PC_10_3[j]
    gs <- iSiteD50[,2]
    slt <- iSiteD50[,3]
    ar <- as.numeric(averate[i])
    if(p1 == 99999){
      
      #Udo et al. 2008 の式
      #中央粒径：ground surface (Ishizuka et al. 2012)←こちらを使用
      #中央粒径：saltation particle(abutaiti et al. 2013)
      ev.d$SF_gs[j] <- (2*2.5*(gs^3)*(p2 + p3))/
        (3000*0.012^2*2*ar)
      ev.d$SF_gs_2[j] <- (2*2.5*(gs^3)*p2)/
        (3000*0.012^2*ar)
      ev.d$SF_gs_3[j] <- (2*2.5*(gs^3)*p3)/
        (3000*0.012^2*ar)
      
      ev.d$SF_sl[j] <- (2*2.5*(slt^3)*(p2 + p3))/
        (3000*0.012^2*2*ar)
      ev.d$SF_sl_2[j] <- (2*2.5*(slt^3)*p2)/
        (3000*0.012^2*ar)
      ev.d$SF_sl_3[j] <- (2*2.5*(slt^3)*p3)/
        (3000*0.012^2*ar)
      
      
    } else{
      ev.d$SF_gs[j] <- (2*2.5*(gs^3)*(p1 + p2 + p3))/
        (3000*0.012^2*3*ar)
      ev.d$SF_gs_1[j] <- (2*2.5*(gs^3)*p1)/
        (3000*0.012^2*ar)
      ev.d$SF_gs_2[j] <- (2*2.5*(gs^3)*p2)/
        (3000*0.012^2*ar)
      ev.d$SF_gs_3[j] <- (2*2.5*(gs^3)*p3)/
        (3000*0.012^2*ar)
      
      ev.d$SF_sl[j] <- (2*2.5*(slt^3)*(p1 + p2 + p3))/
        (3000*0.012^2*3*ar)
      ev.d$SF_sl_1[j] <- (2*2.5*(slt^3)*p1)/
        (3000*0.012^2*ar)
      ev.d$SF_sl_2[j] <- (2*2.5*(slt^3)*p2)/
        (3000*0.012^2*ar)
      ev.d$SF_sl_3[j] <- (2*2.5*(slt^3)*p3)/
        (3000*0.012^2*ar)
      
    }
    setTxtProgressBar(pbj, j) 
  }
  #粗度、摩擦速度算出
  SiteIndex <- levels(ev.d$SiteID)
  pbk <- txtProgressBar(min = 1, max = length(SiteIndex), style = 3)
  for(k in 1:length(SiteIndex)){
    # 該当するサイトのデータのみ抽出
    temp.d2 <- ev.d[ev.d$SiteID==SiteIndex[k],]
    # 計算するデータを選別（イベント、中立条件）
    # if(k == 12){
    #   temp.d2 <- temp.d2[!(temp.d2$WS_h < temp.d2$WS_m & 
    #                          temp.d2$WS_m < temp.d2$WS_l) & 
    #                        temp.d2$WS_l >= 3 &
    #                        lessangle(temp.d2$WD_h,temp.d2$WD_l,45),] #& 
    #                       # temp.d2$Event != 99,]
    # }else{
    #   temp.d2 <- temp.d2[!(temp.d2$WS_h < temp.d2$WS_m & 
    #                          temp.d2$WS_m < temp.d2$WS_l) & 
    #                        temp.d2$WS_l >= 3 &
    #                        (lessangle(temp.d2$WD_h,temp.d2$WD_m,45) &
    #                           lessangle(temp.d2$WD_l,temp.d2$WD_m,45)),] #& 
    #                       # temp.d2$Event != 99,]
    # }
    
    #風向のデータが微妙なので風速のデータのみから中立条件を仮定
    temp.d2 <- temp.d2[!(temp.d2$WS_h < temp.d2$WS_m & temp.d2$WS_m < temp.d2$WS_l) &
      temp.d2$WS_l >= 8 & temp.d2$Event != 99,] 
    
    nI <- nrow(temp.d2)
    if (nI > 1 ) {
      result.df2 <- data.frame(matrix(rep(NA, 4), nrow=1))[numeric(0), ]
      result.df.bi <- data.frame(matrix(rep(NA, nI), nrow=1))[numeric(0), ]
      colnames(result.df2) <- c("R","d0","z0","n")
      colnames(result.df.bi) <- row.names(temp.d2)
      for (a_d0 in seq(0, min(temp.d2$Height_WM_l),by = 0.01)) {
        # a = sigma(i = 1 to I){Ai - Di*Ei/Ci}/(N -sigma(i = 1 to I){Di^2 / Ci})
        # bi = (Ei - Di * a)/Ci
        # jは高度。iは各データ回の番号
        
        # 変数の設定
        Ai <- rep(NaN, length= nI) #sigma(j= 1 to J){ln(z_ij - d0)}, A_i
        Ci <- rep(NaN, length= nI) #sigma(j= 1 to J){u^2_ij}, C_i
        Di <- rep(NaN, length= nI) #sigma(j= 1 to J){u_ij}, D_i
        Ei <- rep(NaN, length= nI) #sigma(j= 1 to J){u_ij * ln(z_ij - d0)}, E_i
        
        # 各変数の計算
        for (l in 1:nI) {
          c.u <- c(temp.d2$WS_h[l],temp.d2$WS_m[l],temp.d2$WS_l[l])
          c.z <- c(temp.d2$Height_WM_h[l],temp.d2$Height_WM_m[l],temp.d2$Height_WM_l[l])
          
          Ai[l] <- sum(log(c.z - a_d0))
          Ci[l] <- sum(c.u^ 2)
          Di[l] <- sum(c.u)
          Ei[l] <- sum(c.u * log(c.z - a_d0))
          
        }
        # aの算出。分母、分子に分けてまず計算
        aupv <- sum(Ai - Di * Ei / Ci) # 分子
        alwv <- nI * nJ - sum(Di ^ 2 / Ci) # 分母 
        
        # a, bの算出
        a <- aupv / alwv
        bi <- (Ei - a * Di) / Ci
        
        # bをz0とu_*iに変換。結果を保存
        usi <- 0.4/bi
        temp_bi <- data.frame(rbind(usi),row.names =a_d0)
        colnames(temp_bi) <- row.names(temp.d2)
        result.df.bi <-rbind(result.df.bi,temp_bi)
        
        # Rの算出
        yij <- NULL
        for (l  in 1:nI) {
          yij <- c(yij,c(temp.d2$WS_h[l],temp.d2$WS_m[l],temp.d2$WS_l[l]) * bi[l])
        }
        
        Yij <- NULL
        for (l  in 1:nI) {
          t.Y.z <- c(temp.d2$Height_WM_h[l],temp.d2$Height_WM_m[l],temp.d2$Height_WM_l[l])
          Yij <- c(Yij,log((t.Y.z - a_d0)/ exp(a)))
        }

        R_v <-mean((yij - mean(yij))*(Yij - mean(Yij))) /
          (sqrt(mean((yij - mean(yij))^2)) * sqrt(mean((Yij - mean(Yij))^2)))
        
        # あるd0に関する結果格納
        temp_result <- data.frame(R_v, a_d0, exp(a),nI)
        colnames(temp_result) <- c("R","d0","z0","n")
        
        result.df2 <- rbind(result.df2,temp_result)
        colnames(result.df2) <- c("R","d0","z0","n")
        result.df2 <- na.omit(result.df2)
      }
      plot(result.df2$d0,result.df2$R)
      dev.copy(pdf, 
               file=paste(path3,"/",averate[i],"_", SiteIndex[k],"_R_d0.pdf",sep=""), 
               width = 10, height = 10)
      dev.off()
      
      # result.df2[result.df2$R==max(result.df2$R),]
      # t(result.df.bi[as.character(result.df2[result.df2$R==max(result.df2$R),"d0"]),])
      
      ev.d[as.character(row.names(temp.d2)),"Z0"] <- result.df2[result.df2$R==max(result.df2$R),"z0"]
      ev.d[as.character(row.names(temp.d2)),"Us"] <- 
        t(result.df.bi[as.character(result.df2[result.df2$R==max(result.df2$R),"d0"]),])
      ev.d[as.character(row.names(temp.d2)),"d0"] <- result.df2[result.df2$R==max(result.df2$R),"d0"]
      ev.d[as.character(row.names(temp.d2)),"R"] <- result.df2[result.df2$R==max(result.df2$R),"R"]
      ev.d[as.character(row.names(temp.d2)),"n"] <- result.df2[result.df2$R==max(result.df2$R),"n"]
      
      setTxtProgressBar(pbk, k)
    }
  }
write.csv(ev.d, paste(path3,"/SfZ0Us_MoM_",averate[i],"_sumdata.csv", sep = ""),row.names=FALSE)
print(averate[i]) 
}

lessangle <- function(ang1,ang2,lesv ){
  distangle <- abs(ang1 - ang2)
  for (i in 1:length(distangle)) {
    if(distangle[i] > 180){
      distangle[i] <- 360 - distangle[i]
    }
  }
  return(distangle < lesv)
}