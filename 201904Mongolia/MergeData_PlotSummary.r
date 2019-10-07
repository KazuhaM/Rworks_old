# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
path <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"

path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
setwd(path)

averate <- c("60","180","300","600","1800")

d.flist <- list.files(path, pattern="csv")
d.flist300 <- d.flist[grep(averate,d.flist)]

pc.col = 4
wm.col = 6

n <- 3 + pc.col + wm.col*1.5 


######全サイトデータ結合########################################
for(k in 1 : length(averate)){
  d.flist300 <- d.flist[grep(paste(averate[k],"_",sep=""),d.flist)]
  result.df <- data.frame(matrix(rep(NA, n), nrow=1))[numeric(0), ]
  colnames(result.df) <- c("Time", "SiteID", "Event", "PC_10_1", "PC_10_2", "PC_10_3", "PC_50_1",
                           "WS_h", "WS_m", "WS_l", "WD_h", "WD_m", "WD_l",
                           "Height_WM_h", "Height_WM_m", "Height_WM_l")
  for(i in 1 : length(d.flist300)){
    d <- read.csv(d.flist300[i],header=T)
    dname <- strsplit(d.flist300[i], "_")
    dname <- sub(".csv", "", dname[[1]][2])
    d.pc <- d[,c(1, grep("PC",colnames(d)))]
    d.pc <- d.pc[, colnames(d.pc) != "PC18_50"]
    d.ws <- d[,c(1, grep("WS",colnames(d)))]
    d.wd <- d[,c(1, grep("WD",colnames(d)))]
    
    height.wm <- as.numeric(sub("WS_","",colnames(d.ws)[2:length(colnames(d.ws))]))
    height.wm.mat <- matrix(0,nrow(d),length(height.wm))
    for(j in 1 : nrow(d)){
      height.wm.mat[j,] <- height.wm
    }
    
    
    if(ncol(d.pc) == 4){
      temp.df <- cbind(d[,1], rep(dname,nrow(d)),rep(1,nrow(d)),rep(99999,nrow(d)),
                       d.pc[2:ncol(d.pc)],d.ws[2:ncol(d.ws)],d.wd[2:ncol(d.wd)],height.wm.mat)
    }else if(ncol(d.pc) == 5){
      temp.df <- cbind(d[,1], rep(dname,nrow(d)),rep(1,nrow(d)),
                       d.pc[2:ncol(d.pc)],d.ws[2:ncol(d.ws)],d.wd[2:ncol(d.wd)],height.wm.mat)
    }
    colnames(temp.df) <- colnames(result.df)
    result.df = rbind(result.df,temp.df)
    
    par(mfrow=c(3,1))
    #drifting sands
    ts.plot(d.pc[2],gpars=list(xlab="Time", ylab="Drifting Sands(n/s)", main = paste(averate[k],"_", dname,sep="")),
            ylim = c(0,max(d.pc[,2:ncol(d.pc)])),col=1) 
    for(j in 3:ncol(d.pc)){
      par(new = T)
      ts.plot(d.pc[j],gpars=list(xlab="", ylab=""),
              ylim = c(0,max(d.pc[,2:ncol(d.pc)])),col=j-1)
    }
    legend(max(as.ts(d[,1]))*0.9,max(d.pc[,2:ncol(d.pc)]),colnames(d.pc)[2:ncol(d.pc)],lty=1, 
           col =1:(ncol(d.pc)-1))
    
    #wind speed
    ts.plot(d.ws[2],gpars=list(xlab="Time", ylab="Wind Speed(m/s)"),
            ylim = c(0,max(d.ws[,2:ncol(d.ws)])),col=1)
    
    for(j in 3:ncol(d.ws)){
      par(new = T)
      ts.plot(d.ws[j],gpars=list(xlab="", ylab=""),
              ylim = c(0,max(d.ws[,2:ncol(d.ws)])),col=j -1)
    }
    legend(max(as.ts(d[,1]))*0.9,max(d.ws[,2:ncol(d.ws)]),colnames(d.ws)[2:ncol(d.ws)],lty=1,
           col =1:(ncol(d.ws)-1))
    
    #wind dir.
    ts.plot(d.wd[2],gpars=list(xlab="Time", ylab="Wind Direction(degree)"),
            ylim = c(0,max(d.wd[,2:ncol(d.wd)])),col=1)
    
    for(j in 3:ncol(d.wd)){
      par(new = T)
      ts.plot(d.wd[j],gpars=list(xlab="", ylab=""),
              ylim = c(0,max(d.wd[,2:ncol(d.wd)])),col=j -1)
    }
    legend(max(as.ts(d[,1]))*0.9,max(d.wd[,2:ncol(d.wd)]),colnames(d.wd)[2:ncol(d.wd)],lty=1,
           col =1:(ncol(d.wd)-1))
    abline(h = 120)
    abline(h = 240)
    
    dev.copy(pdf, file=paste(averate[k],"_", dname,".pdf",sep=""), width = 10, height = 10)
    dev.off()
  }
  
  write.csv(result.df, paste(sub("/avebyn","",path),"/", averate[k],"_sumdata.csv", sep = ""),row.names=FALSE)
  
}
