library(sp)
library(tcltk2)
# 内外の判定 0 : 領域外の点 1 : 領域内の点 2 : 境界上の点(辺) 3 :境界上の点(頂点)
# res <- point.in.polygon(x, y, pol.x, pol.y)

path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1102春期モンゴル解析3/distribution"
setwd(path)

WsiteD <- read.csv("WsiteShrubDistribution.csv",header = T)
# WsiteD_index <- setdiff(colnames(WsiteD), c("SiteDir.E0_left.","shrub_dir.E0_left."))
# WsiteD <- WsiteD[WsiteD_index]
site_name <- as.factor(WsiteD$Site)
site_name <- levels(site_name)

max_h <- max(WsiteD$height.cm.[!is.na(WsiteD$height.cm.)])
min_h <- min(WsiteD$height.cm.[!is.na(WsiteD$height.cm.)])

# グリッドポイントの座標行列を生成
p_grid_x <- 0:20
p_grid_y <- 0:20

septime <- 32
linepointx <- flinepointx(septime,1002)
linepointy <- flinepointy(septime,1002)

pori_sm <- 100
base_x <- seq(-pi, pi, length=pori_sm)


for (i in 1:length(site_name)) {
  temp_WsiteD <- WsiteD[WsiteD$Site == site_name[i],]
  # plot.new()
  # plot.window(xlim=c(-2, 22), ylim=c(22, -2))
  # for (j in 1:4) {
  #   axis(side = j, at=seq(0, 20, by = 5))
  # }
  # for (j in 0:20) {
  #   segments(j, 0, j, 20)
  #   segments(0, j, 20, j)
  # }
  # for (j in 0:4) {
  #   abline(h = j*5, lty=3)
  #   abline(v = j*5, lty=3)
  # }
  # 
  shrb_px <- data.frame(matrix(rep(NA, pori_sm), nrow=1))[numeric(0), ]
  shrb_py <- data.frame(matrix(rep(NA, pori_sm), nrow=1))[numeric(0), ]
  
  for(j in 1:nrow(temp_WsiteD)){
    # 灌木なら
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
      
      t_x <- data.frame(rbind(t_x))
      row.names(t_x) <- j
      t_y <- data.frame(rbind(t_y))
      row.names(t_y) <- j
      
      shrb_px <- rbind(shrb_px,t_x)
      shrb_py <- rbind(shrb_py,t_y)
      # 楕円を描画
      # polygon(t_x, t_y, col =gray(1 - (temp_WsiteD$height.cm.[j] - min_h) / max_h))
    # 風速計なら
    }#else if(temp_WsiteD$pc.wm[j] %in% c("l", "m", "h") ){
    #   # 中心のx座標
    #   t_cenx <- temp_WsiteD$x.m.[j]
    #   # 中心のy座標
    #   t_ceny <- temp_WsiteD$y.m.[j]
    #   
    #   points(t_cenx,t_ceny,pch = 14, cex=2)
    #   text(t_cenx,t_ceny + 0.6, paste("wm_", temp_WsiteD$pc.wm[j],sep = ""), cex=1.1)
    # # 飛砂計なら
    # }else{
    #   # 中心のx座標
    #   t_cenx <- temp_WsiteD$x.m.[j]
    #   # 中心のy座標
    #   t_ceny <- temp_WsiteD$y.m.[j]
    #   
    #   points(t_cenx,t_ceny,pch = 8, cex=2)
    #   text(t_cenx,t_ceny + 0.6, paste("pc_", temp_WsiteD$pc.wm[j],sep = ""), cex=1.1)
    # }
    
  }
  
  result.shrb.distr <- data.frame(matrix(rep(NA, septime + 2), nrow=1))[numeric(0), ]
  colnames(result.shrb.distr) <- c("grid_x","grid_y",seq(0, 360-360/septime, by = 360/septime))
  pb <- txtProgressBar(min = 1, max = 21^2, style = 3)
  for (xi in 1:21) {
    for (yi in 1:21) {
      i_linepointx <- linepointx + p_grid_x[xi]
      i_linepointy <- linepointy + p_grid_y[yi]
      
      temp_rs <- data.frame(rbind(c(p_grid_x[xi], p_grid_y[yi], rep(NA,septime))))
      colnames(temp_rs) <- c("grid_x","grid_y",seq(0, 360-360/septime, by = 360/septime))
      result.shrb.distr <- rbind(result.shrb.distr,temp_rs)
      for (iang in 1:septime) {
        lestsh_dist　<- c()
        for (ishrb in 1:nrow(temp_WsiteD)) {
          lineang <- as.numeric(colnames(i_linepointx))[iang]
          res <- point.in.polygon(i_linepointx[,iang], i_linepointy[,iang], 
                                  shrb_px[ishrb,], shrb_py[ishrb,])
          lim_p_x <- length(i_linepointx[i_linepointx[,iang] <= 20 & i_linepointx[,iang] >= 0 ,iang])
          lim_p_y <- length(i_linepointy[i_linepointy[,iang] <= 20 & i_linepointy[,iang] >= 0 ,iang])
          res[min(lim_p_x,lim_p_y)] <- 4
          end_p <- which(res != 0)[1]
          lestsh_dist[ishrb] <- linepointx[end_p,iang] / cos(lineang * pi / 180)
        }
        result.shrb.distr[(xi - 1) * 21 + yi, 2 + iang] <- min(lestsh_dist)
      }
      setTxtProgressBar(pb, (xi - 1) * 21 + yi) 
    }
  }
  # arrows(-1.5, -1.5, cos(-temp_WsiteD$SiteDir.E0_left.[1] * pi / 180) - 1.5,
  #        sin(-temp_WsiteD$SiteDir.E0_left.[1] * pi / 180) - 1.5 ,angle = 15, length = 0.15)
  # text(1.3* cos(-temp_WsiteD$SiteDir.E0_left.[1] * pi / 180) - 1.5,
  # #      1.3 * sin(-temp_WsiteD$SiteDir.E0_left.[1] * pi / 180) - 1.5, "N")
  # dev.copy(pdf, file=paste("DistributionPlot_",site_name[i],".pdf",sep=""), width = 10, height = 10)
  # dev.off()
  write.csv(result.shrb.distr, paste(site_name[i],"_shdist.csv", sep = ""),row.names=FALSE)
  print(pasti(i,"/",length(site_name),"まで終了",sep ="" ))
}

flinepointx <- function(seppi,count){
  sepang <- 360/seppi
  reprang <- seq(0, 360-sepang, by = sepang)
  output <- data.frame(matrix(NA, nrow=count, ncol = seppi))
  colnames(output) <- reprang
  for(tiangx in reprang){
    output[,as.character(tiangx)] <- seq(0,length = count, by = sqrt(2)*cos(tiangx * pi / 180) / 50)
  }
  return(output)
}
flinepointy <- function(seppi,count){
  sepang <- 360/seppi
  reprang <- seq(0, 360-sepang, by = sepang)
  output <- data.frame(matrix(NA, nrow=count, ncol = seppi))
  colnames(output) <- reprang
  for(tiangy in reprang){
    output[,as.character(tiangy)] <- (seq(0,length = count, by = sqrt(2)*sin(tiangy * pi / 180) / 50))
  }
  return(output)
}