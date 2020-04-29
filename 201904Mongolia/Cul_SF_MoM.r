library(tcltk2)

# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"

# path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
# path3 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1102春期モンゴル解析3/roughness"
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
setwd(path3)
# averate <- c("60","180","300","600","1800")
averate <- c("60")
# 風速計の1サイトあたりの数（全サイトで等しい場合）
nJ <- 3

############################臨界風速等算出###################

#粒径データ
d50 <- read.csv("SiteParticle.csv",header=T)

for(i in 1:length(averate)){
  # for(i in 1:5){
  ev_filename = paste("Ev_sumdata_",averate[i],"_T_P.csv",sep="")
  ev.d <- read.csv(ev_filename,header=T)
  ev.d["SF_gs"] <- NaN
  ev.d["SF_gs_1"] <- NaN
  ev.d["SF_gs_2"] <- NaN
  ev.d["SF_gs_3"] <- NaN
  ev.d["SF_sl"] <- NaN
  ev.d["SF_sl_1"] <- NaN
  ev.d["SF_sl_2"] <- NaN
  ev.d["SF_sl_3"] <- NaN
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
  write.csv(ev.d, paste(path3,"/Sf_MoM_",averate[i],"_sumdata.csv", sep = ""),row.names=FALSE)
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