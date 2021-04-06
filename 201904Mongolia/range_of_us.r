path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
setwd(path3)
averate <- c("60","600")
rateNo <- 1
sf_filename = paste(path3,"/Z0Us_MoM_",averate[rateNo],"_sumdata.csv",sep="")
sf.d <- read.csv(sf_filename,header=T)

# サイトリスト
sf.d$SiteID <- as.factor(sf.d$SiteID)
sitelev <- levels(sf.d$SiteID)
# イベントリスト
sf.d$Event <- as.factor(sf.d$Event)
eventlev <- levels(sf.d$Event)[levels(sf.d$Event)!="99"]

result.df3 <-data.frame(matrix(rep(NA, 4), nrow=1))[numeric(0), ]
colnames(result.df3) <- c("Event", "Site", "Us_min","Us_max")
for(jev in eventlev){
for(ksite in sitelev){
  temp.us <- sf.d$Us[sf.d$Event==jev&sf.d$SiteID==ksite] 
  temp.us <- temp.us[!is.na(temp.us)]
  if(length(temp.us) != 0){
    temp.result <- data.frame("Event"= jev,
                              "Site" = ksite,
                              "Us_min" = min(temp.us),
                              "Us_max" = max(temp.us))
    result.df3 <- rbind(result.df3,temp.result)
  }
}
}
write.csv(result.df3, paste(path3,"/Us_ragne_",averate[rateNo],".csv", sep = ""),row.names=FALSE)

