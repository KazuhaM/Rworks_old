path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
setwd(path2)

data.pc <- read.csv("Ev_60_sumdata.csv")
data.temp <- read.csv("Ev_temp60_sumdata.csv")

data.pc$ID <- paste(data.pc$Time,data.pc$SiteID,data.pc$Event,sep="")
data.temp$ID <- paste(data.temp$Time,data.temp$SiteID,data.temp$Event,sep="")

data.merge <- merge(data.pc,data.temp, by.x = "ID", by.y = "ID")
nrow(data.merge)

write.csv(data.merge, paste(path3,"/Ev_sumdata_",averate[i],"_T_P.csv", sep = ""),row.names=FALSE)
