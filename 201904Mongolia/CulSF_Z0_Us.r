library(tcltk2)

# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"

path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
setwd(path2)
averate <- c("60","180","300","600","1800")

############################臨界風速等算出###################

#粒径データ
d50 <- read.csv("SiteParticle.csv",header=T)

pbi <- txtProgressBar(min = 1, max = length(averate), style = 3)
# for(i in 1:length(averate)){
for(i in 1:1){
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
  
  pbj <- txtProgressBar(min = 1, max = nrow(ev.d), style = 3)
  #サルテーションフラックス算出
  for(j in 1:nrow(ev.d)){
    iSiteD50 <- d50[d50[,1]==ev.d$SiteID[j],]
    if(ev.d$PC_10_1[j] == 99999){
      
      #Udo et al. 2008 の式
      #中央粒径：ground surface (Ishizuka et al. 2012)←こちらを使用
      #中央粒径：saltation particle(abutaiti et al. 2013)
      ev.d$SF_gs[j] <- (2*2.5*(iSiteD50[,2]^3)*(ev.d$PC_10_2[j] + ev.d$PC_10_3[j]))/
        (3000*0.012^2*2*as.numeric(averate[i]))
      ev.d$SF_gs_2[j] <- (2*2.5*(iSiteD50[,2]^3)*ev.d$PC_10_2[j])/
        (3000*0.012^2*as.numeric(averate[i]))
      ev.d$SF_gs_3[j] <- (2*2.5*(iSiteD50[,2]^3)*ev.d$PC_10_3[j])/
        (3000*0.012^2*as.numeric(averate[i]))
      
      ev.d$SF_sl[j] <- (2*2.5*(iSiteD50[,3]^3)*(ev.d$PC_10_2[j] + ev.d$PC_10_3[j]))/
        (3000*0.012^2*2*as.numeric(averate[i]))
      ev.d$SF_sl_2[j] <- (2*2.5*(iSiteD50[,3]^3)*ev.d$PC_10_2[j])/
        (3000*0.012^2*as.numeric(averate[i]))
      ev.d$SF_sl_3[j] <- (2*2.5*(iSiteD50[,3]^3)*ev.d$PC_10_3[j])/
        (3000*0.012^2*as.numeric(averate[i]))
      
      
    } else{
      ev.d$SF_gs[j] <- (2*2.5*(iSiteD50[,2]^3)*(ev.d$PC_10_1[j] + ev.d$PC_10_2[j] + ev.d$PC_10_3[j]))/
        (3000*0.012^2*3*as.numeric(averate[i]))
      ev.d$SF_gs_1[j] <- (2*2.5*(iSiteD50[,2]^3)*ev.d$PC_10_1[j])/
        (3000*0.012^2*as.numeric(averate[i]))
      ev.d$SF_gs_2[j] <- (2*2.5*(iSiteD50[,2]^3)*ev.d$PC_10_3[j])/
        (3000*0.012^2*as.numeric(averate[i]))
      ev.d$SF_gs_3[j] <- (2*2.5*(iSiteD50[,2]^3)*ev.d$PC_10_3[j])/
        (3000*0.012^2*as.numeric(averate[i]))
      
      ev.d$SF_sl[j] <- (2*2.5*(iSiteD50[,3]^3)*(ev.d$PC_10_1[j] + ev.d$PC_10_2[j] + ev.d$PC_10_3[j]))/
        (3000*0.012^2*3*as.numeric(averate[i]))
      ev.d$SF_sl_1[j] <- (2*2.5*(iSiteD50[,3]^3)*ev.d$PC_10_1[j])/
        (3000*0.012^2*as.numeric(averate[i]))
      ev.d$SF_sl_2[j] <- (2*2.5*(iSiteD50[,3]^3)*ev.d$PC_10_2[j])/
        (3000*0.012^2*as.numeric(averate[i]))
      ev.d$SF_sl_3[j] <- (2*2.5*(iSiteD50[,3]^3)*ev.d$PC_10_3[j])/
        (3000*0.012^2*as.numeric(averate[i]))
      
    }
    #粗度、摩擦速度算出
    if(ev.d$WS_h[j] > ev.d$WS_m[j] && ev.d$WS_m[j] > ev.d$WS_l[j] && ev.d$WS_h[j] >= 8 &&  ev.d$Event != 99){
    # if(!(ev.d$WS_h[j] <= ev.d$WS_m[j] && ev.d$WS_m[j] <= ev.d$WS_l[j]) && ev.d$WS_h[j] >= 8 &&  ev.d$Event != 99){
      iws <- c(ev.d$WS_h[j],ev.d$WS_m[j],ev.d$WS_l[j])
      ih <- c(ev.d$Height_WM_h[j],ev.d$Height_WM_m[j],ev.d$Height_WM_l[j])/100
      
      # if(j == 1){
      #   plot(iws, ih, log = "y",xlim = c(0,max(ev.d$WS_h)),ylim =c(0.01,10))
      # }else{
      #   par(new = T)
      #   plot(iws, ih, log = "y",xlim = c(0,max(ev.d$WS_h)),ylim =c(0.01,10))
      #   
      # }
      ih <- log(ih)
      
      tmp.result <- lm(ih ~ iws)
      ev.d$Z0[j] <- exp(as.numeric(tmp.result$coefficients[1]))
      ev.d$Us[j] <- 0.4/as.numeric(tmp.result$coefficients[2])
      # abline(tmp.result)
      
    }
    setTxtProgressBar(pbj, j) 
  }
  SiteIndex <- levels(ev.d$SiteID)
  for(k in 1:length(SiteIndex)){
    temp.d2 <- ev.d[ev.d$SiteID==SiteIndex[k],]
    aveZ0 <- mean(temp.d2$Z0[!is.na(temp.d2$Z0)])
    ev.d$AveZ0[ev.d$SiteID==SiteIndex[k]] <- aveZ0
  }
  ev.d["AveUs"] <- NaN
  for(j in 1:nrow(ev.d)){
    iSiteD50 <- d50[d50[,1]==ev.d$SiteID[j],]
    #粗度、摩擦速度算出
    if(ev.d$WS_h[j] > ev.d$WS_m[j] && ev.d$WS_m[j] > ev.d$WS_l[j]){
      # if(!(ev.d$WS_h[j] <= ev.d$WS_m[j] && ev.d$WS_m[j] <= ev.d$WS_l[j]) && ev.d$WS_h[j] >= 8 &&  ev.d$Event != 99){
      iws <- c(ev.d$WS_h[j],ev.d$WS_m[j],ev.d$WS_l[j])
      ih <- c(ev.d$Height_WM_h[j],ev.d$Height_WM_m[j],ev.d$Height_WM_l[j])/100
      javeZ0 <- ev.d$AveZ0[j]
      # if(j == 1){
      #   plot(iws, ih, log = "y",xlim = c(0,max(ev.d$WS_h)),ylim =c(0.01,10))
      # }else{
      #   par(new = T)
      #   plot(iws, ih, log = "y",xlim = c(0,max(ev.d$WS_h)),ylim =c(0.01,10))
      #   
      # }
      ih <- log(ih)
      
      tmp.result <- lm((ih- javeZ0) ~0+ iws)
      ev.d$AveUs[j] <- 0.4/as.numeric(tmp.result$coefficients[1])
      # abline(tmp.result)
      
    }
  }
  write.csv(ev.d, paste("SfZ0Us_",averate[i],"_sumdata.csv", sep = ""),row.names=FALSE)
  print(averate[i]) 
}