path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
path4 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
setwd(path3)

averate <- c("60","600")
rateNo <- 1
sf_filename = paste("Ev_sumdata_",averate[rateNo],"_T_P.csv",sep="")
sf.d <- read.csv(sf_filename,header=T)

# サイトリスト
sf.d$SiteID <- as.factor(sf.d$SiteID)
sitelev <- levels(sf.d$SiteID)
# イベントリスト
sf.d$Event <- as.factor(sf.d$Event)
eventlev <- levels(sf.d$Event)[levels(sf.d$Event)!="99"]

result.df3 <-data.frame(matrix(rep(NA, 4), nrow=1))[numeric(0), ]
colnames(result.df3) <- c("Event", "Site", "U_min","U_max")
for(jev in eventlev){
for(ksite in sitelev){
  temp.us <- sf.d$WS_h[sf.d$Event==jev&sf.d$SiteID==ksite] 
  temp.us <- temp.us[!is.na(temp.us)]
  if(length(temp.us) != 0){
    temp.result <- data.frame("Event"= jev,
                              "Site" = ksite,
                              "U_min" = min(temp.us),
                              "U_max" = max(temp.us))
    result.df3 <- rbind(result.df3,temp.result)
  }
}
}
write.csv(result.df3, paste(path4,"/U_ragne_",averate[rateNo],".csv", sep = ""),row.names=FALSE)

