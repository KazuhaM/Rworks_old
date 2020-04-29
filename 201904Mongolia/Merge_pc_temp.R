path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
setwd(path2)

data.pc <- read.csv("Ev_60_sumdata.csv")
data.temp <- read.csv("Ev_temp60_sumdata.csv")

data.pc$ID <- paste(data.pc$Time,data.pc$SiteID,data.pc$Event,sep="")
data.temp$ID <- paste(data.temp$Time,data.temp$SiteID,data.temp$Event,sep="")
same.check <- data.pc$ID ==data.temp$ID
if(length(same.check[same.check ==TRUE]) == length(same.check)){
  data.temp.temp <- data.frame("T_h" = data.temp$T_h,
                               "T_m" = data.temp$T_m,
                               "T_l" = data.temp$T_l,
                               "H_h" = data.temp$H_h,
                               "H_m" = data.temp$H_m,
                               "H_l" = data.temp$H_l,
                               "Dh_m" = data.temp$T_h-data.temp$T_m, #D1
                               "Dm_l" = data.temp$T_m-data.temp$T_l) #D2
  data.merge <- cbind(data.pc[,1:(ncol(data.pc)-1)],data.temp.temp)
}else{
  print("ERROR IDs are not same between PC data and Temp data")
}

# data.merge <- merge(data.pc,data.temp, by.x = "ID", by.y = "ID")
# nrow(data.merge)

write.csv(data.merge, paste(path3,"/Ev_sumdata_","60","_T_P.csv", sep = ""),row.names=FALSE)
