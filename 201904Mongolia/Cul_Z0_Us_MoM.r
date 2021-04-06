library(tcltk2)

# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"

# path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
# path3 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1102春期モンゴル解析3/roughness"
# path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
# path4 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
path4 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1301MongoliaAnalysis8/NsiteRecul" #Nsite only
setwd(path4)
# averate <- c("60","180","300","600","1800")
averate <- c("60","600")
# averate <- c("60")
# 風速計の1サイトあたりの数（全サイトで等しい場合）
nJ <- 3



############################粗度、摩擦速度、地面修正量算出###################

# #粒径データ
# d50 <- read.csv("SiteParticle.csv",header=T)

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
  eventlev <- levels(as.factor(ev.d$Event))
    eventlev <- as.numeric(eventlev[-length(eventlev)])
  pbk <- txtProgressBar(min = 1, max = length(SiteIndex), style = 3)
  for(k in 1:length(SiteIndex)){
    for (j in eventlev){
      # 該当するサイトのデータのみ抽出
      temp.d2 <- ev.d[ev.d$SiteID==SiteIndex[k] & ev.d$Event == j,]
      if (nrow(temp.d2) > 1) {
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
        temp.d2 <- temp.d2[
          !(temp.d2$WS_h < temp.d2$WS_m & temp.d2$WS_m < temp.d2$WS_l) & #風速の逆転が起きていない
            temp.d2$WS_l >= 8 & #風速が8m/s以上である
            temp.d2$Event != 99 & #イベント期間中である
            temp.d2$Dh_m < 0.5 & temp.d2$Dm_l < 0.5 & #気温が反転していない
            temp.d2$Dh_m > -1 & temp.d2$Dm_l > -1 #気温差が一定値以下である
          ,] 
        
        ### プロファイルを描いて具体的に検証
        # est.us <- c(t(result.df.bi[as.character(result.df2[result.df2$R==max(result.df2$R),"d0"]),]))
        # est.d0 <- result.df2[result.df2$R==max(result.df2$R),"d0"]
        # est.z0 <- result.df2[result.df2$R==max(result.df2$R),"z0"]
        # for(m in 1:nrow(temp.d2)){
        #   if(m != 1){
        #     par(new = T)
        #   }
        #   a.profile <- data.frame(z = c(temp.d2$Height_WM_h[m],
        #                                 temp.d2$Height_WM_m[m],
        #                                 temp.d2$Height_WM_l[m]),
        #                           ws = c(temp.d2$WS_h[m],
        #                                  temp.d2$WS_m[m],
        #                                  temp.d2$WS_l[m]))
        #   if(m > 9){
        #     p.pch <- 22
        #   }else{
        #     p.pch <- 21
        #   }
        #   plot(z~ws,data =  a.profile,ylim = c(0.0001,2.5),xlim = c(0,20),log="y",col=m,yaxt = "n",
        #        xaxt = "n",pch =p.pch,cex = 1.2,bg = m)
        #   if(m == 1){
        #     axis(1)
        #     sLab <- c(expression(10^{-4}),expression(10^{-3}),expression(10^{-2}),
        #               expression(10^{-1}),expression(10^{0}),expression(10^{1}))
        #     
        #     axis(side=2,          #side2:左
        #          at=10^(-4:1), #0から8まで1ずつ
        #          tck=0.03,            #長さ0.03のティック
        #          labels=sLab,
        #          mgp=c(1,0.5,0),
        #          cex.axis = 1.2
        #     )
        #     fPow <- function(x){(2:9)*10^x}
        #     axis(side=2,        #side2:左
        #          at=sapply(-4:1, fPow), #繰り返し(2:9)×10^(iLogL:iLogU)
        #          tck=0.01,          #長さ0.01のティック
        #          labels=FALSE,      #ラベル出力なし
        #          mgp=c(1,0.5,0)
        #     )
        #   }
        #   a.profile$z <- log(a.profile$z)
        #   a.pr.lm <- lm(z~ws,data =  a.profile)
        #   pred.x <- seq(0,20,by = 1)
        #   
        #   # pred.pr <- exp(a.pr.lm$coefficients[1] + a.pr.lm$coefficients[2]*seq(0,20,by = 1))
        #  
        #   pred.pr <- est.d0 + est.z0 * exp(0.4 * pred.x / est.us[m])
        #   lines(pred.x,pred.pr,col=m)
        # }
        
        
        nI <- nrow(temp.d2)
        if (nI > 1 ) {
          result.df2 <- data.frame(matrix(rep(NA, 4), nrow=1))[numeric(0), ]
          result.df.bi <- data.frame(matrix(rep(NA, nI), nrow=1))[numeric(0), ]
          colnames(result.df2) <- c("R","d0","z0","n")
          colnames(result.df.bi) <- row.names(temp.d2)
          # for (a_d0 in seq(0, min(temp.d2$Height_WM_l),by = 0.01)) {
          for (a_d0 in 0:0) { ### Nsite onlyのために変更！
            # a = sigma(i = 1 to I){Ai - Di*Eii/Ci}/(N -sigma(i = 1 to I){Di^2 / Ci})
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
          # par(mar = c(4.5,4.5,2,1),family = family_serif)
          plot(result.df2$d0,result.df2$R,main =paste(SiteIndex[k] ,"_", j,sep=""))
          dev.copy(pdf, 
                   file=paste(path4,"/R_MoM/",averate[i],"_", SiteIndex[k],"_",j,"_R.pdf",sep=""), 
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
    }
  }
write.csv(ev.d, paste(path4,"/Z0_MoM_",averate[i],"_sumdata.csv", sep = ""),row.names=FALSE)
# write.csv(ev.d, paste(path3,"/Z0Us_MoM_",averate[i],"_sumdata_d_0.csv", sep = ""),row.names=FALSE)
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