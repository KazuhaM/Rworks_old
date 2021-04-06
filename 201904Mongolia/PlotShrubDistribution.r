# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1102春期モンゴル解析3/distribution"
path <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1102春期モンゴル解析3/distribution"
setwd(path)

WsiteD <- read.csv("WsiteShrubDistribution.csv",header = T)
# WsiteD_index <- setdiff(colnames(WsiteD), c("SiteDir.E0_left.","shrub_dir.E0_left."))
# WsiteD <- WsiteD[WsiteD_index]
site_name <- as.factor(WsiteD$Site)
site_name <- levels(site_name)

max_h <- max(WsiteD$height.cm.[!is.na(WsiteD$height.cm.)])
min_h <- min(WsiteD$height.cm.[!is.na(WsiteD$height.cm.)])

for (i in 1:length(site_name)) {
  temp_WsiteD <- WsiteD[WsiteD$Site == site_name[i],]
  plot.new()
  plot.window(xlim=c(-2, 22), ylim=c(-2, 22))
  for (j in 1:4) {
    axis(side = j, at=seq(0, 20, by = 5))
  }
  for (j in 0:20) {
    segments(j, 0, j, 20)
    segments(0, j, 20, j)
  }
  for (j in 0:4) {
    abline(h = j*5, lty=3)
    abline(v = j*5, lty=3)
  }
  
  base_x <- seq(-pi, pi, length=100)
  for(j in 1:nrow(temp_WsiteD)){
    if(temp_WsiteD$pc.wm[j] == ""){
      # 長軸（半分）
      t_a <- temp_WsiteD$long_axis.cm.[j]/200
      # 短軸（半分）
      t_b <- temp_WsiteD$short_axis.cm.[j]/200
      # 回転角
      t_theta <- temp_WsiteD$shrub_dir.E0_left.[j] * pi / 180
      # 中心のx座標
      t_cenx <- temp_WsiteD$x.m.[j]
      # 中心のy座標
      t_ceny <- temp_WsiteD$y.m.[j]
      
      # 楕円周上の点の座標
      t_x <- t_a * cos(base_x) * cos(t_theta) - t_b * sin(base_x) * sin(t_theta) + t_cenx
      t_y <- t_a * cos(base_x) * sin(t_theta) + t_b * sin(base_x) * cos(t_theta) + t_ceny
      
      # 楕円を描画
      polygon(t_x, t_y, col =gray(1 - (temp_WsiteD$height.cm.[j] - min_h) / max_h))
    }else if(temp_WsiteD$pc.wm[j] %in% c("l", "m", "h") ){
      # 中心のx座標
      t_cenx <- temp_WsiteD$x.m.[j]
      # 中心のy座標
      t_ceny <- temp_WsiteD$y.m.[j]
      
      points(t_cenx,t_ceny,pch = 14, cex=2)
      text(t_cenx,t_ceny + 0.6, paste("wm_", temp_WsiteD$pc.wm[j],sep = ""), cex=1.1)
    }else{
      # 中心のx座標
      t_cenx <- temp_WsiteD$x.m.[j]
      # 中心のy座標
      t_ceny <- temp_WsiteD$y.m.[j]
      
      points(t_cenx,t_ceny,pch = 8, cex=2)
      text(t_cenx,t_ceny + 0.6, paste("pc_", temp_WsiteD$pc.wm[j],sep = ""), cex=1.1)
    }
    
  }
  arrows(-1.5, 21.5, cos(-temp_WsiteD$SiteDir.E0_left.[1] * pi / 180) - 1.5,
         sin(temp_WsiteD$SiteDir.E0_left.[1] * pi / 180) + 21.5 ,angle = 15, length = 0.15)
  text(1.3* cos(-temp_WsiteD$SiteDir.E0_left.[1] * pi / 180) - 1.5,
       1.3 * sin(temp_WsiteD$SiteDir.E0_left.[1] * pi / 180) + 21.5, "N")
  #dev.copy(pdf, file=paste("DistributionPlot_",site_name[i],".pdf",sep=""), width = 10, height = 10)
  #dev.off()
}

