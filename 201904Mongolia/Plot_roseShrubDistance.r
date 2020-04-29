library(plotrix)

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
  
  est_len <- apply(d2, 2, mean)
  # result <- cbind(as.numeric(sub("X","",colnames(d)[3:ncol(d)])),est_len)
  result <- est_len
  
  radial.plot(result,lwd = 2,
              labels=c("","","","","","","",""),
              start=0,clockwise=FALSE, rp.type="p",radial.lim =c(0,trunc(max(result))+1),
              show.radial.grid = TRUE,grid.left = TRUE,show.grid.labels = 3)
  arrows(0, 0, cos(-temp_WsiteD$SiteDir.E0_left.[1] * pi / 180),
         sin(temp_WsiteD$SiteDir.E0_left.[1] * pi / 180)  ,angle = 15, length = 0.15,lwd = 2)
  mtext(paste("Elongation: ",signif(max(result) / min(result),3),sep = ""), side = 1, line = 0, at = NA)
  
  dev.copy(pdf, file=paste("rosePlot_",site_name[i],".pdf",sep=""), width = 10, height = 10)
  dev.off()
}