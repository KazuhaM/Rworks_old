library(sp)
library(tcltk2)

path3 <- "D:\SpAr"
setwd(path3)

# サイト一覧取得
site_id <- read.csv("Sites_var.csv",header =T)


# グリッドポイントの座標行列を生成
p_grid_x <- 0:20
p_grid_y <- 0:20

septime <- 32
linepointx <- flinepointx(septime,1002)
linepointy <- flinepointy(septime,1002)

pori_sm <- 100
base_x <- seq(-pi, pi, length=pori_sm)


for(i_site in 1:nrow(site_id)){
    site.name <- site_id[i_site,1]
    sp_shp<-sf::st_read(paste("21forVariogram/var_", site.name, ".shp", sep = ""))

    st_geometry(sp_shp) %>%
    plot()

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
            for (ishrb in 1:nrow(temp_WsiteD)) { #地物の数
                lineang <- as.numeric(colnames(i_linepointx))[iang]
                res <- point.in.polygon(i_linepointx[,iang], i_linepointy[,iang], 
                                        shrb_px[ishrb,], shrb_py[ishrb,]) ###ここにポリゴンの頂点のx,y座標をベクトルデータで入力
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

    write.csv(result.shrb.distr, paste(site_name[i],"_shdist.csv", sep = ""),row.names=FALSE)
    print(pasti(i,"/",length(site_name),"まで終了",sep ="" ))
}

###################### functions ###############################
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