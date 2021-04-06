path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1102春期モンゴル解析3/distribution"
setwd(path)

WsiteD <- read.csv("WsiteShrubDistribution.csv",header = T)
site_name <- as.factor(WsiteD$Site)
site_name <- levels(site_name)

for (i in 1:length(site_name)) {
  temp_WsiteD <- WsiteD[WsiteD$Site == site_name[i],]
  wr_filename <- paste(site_name[i],"_shdist.csv", sep = "")
  d <- read.csv(wr_filename, header = T)
  d2 <- d[,3:ncol(d)]
  d2 <- as.matrix(d2)
  map_len <- matrix(NA, nrow=21, ncol=21) 
  p_len <- cbind(d[,1:2],rowSums(d2))
  for (xj in 1:21) {
    for (yk in 1:21) {
      map_len[yk,xj] <- p_len[(xj - 1) * 21 + yk,3]
    }
  }
  
  image(map_len)
  dev.copy(pdf, file=paste("Map_len",site_name[i],".pdf",sep=""), width = 10, height = 10)
  dev.off()
}