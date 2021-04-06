library(tcltk2)

# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"

path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
setwd(path2)
averate <- c("60","180","300","600","1800")

############################臨界風速等算出###################
i <- 1
sf_filename = paste("SfZ0Us_",averate[i],"_sumdata.csv",sep="")
sf.d <- read.csv(sf_filename,header=T)
sitelev <- levels(sf.d$SiteID)
sf.d$Event <- as.factor(sf.d$Event)
eventlev <- levels(sf.d$Event)[levels(sf.d$Event)!="99"]
par(mfrow=c(1,1))
pbj <- txtProgressBar(min = 1, max = length(sitelev), style = 3)
for(j in 1:length(sitelev)){
  for (k in 1:length(eventlev)) {
    boolplot <- TRUE
    for(i in 1:1){
      sf_filename = paste("SfZ0Us_",averate[i],"_sumdata.csv",sep="")
      sf.d <- read.csv(sf_filename,header=T)
      sf.d$Event <- as.factor(sf.d$Event)
      
      temp.d <- sf.d[sf.d$SiteID == sitelev[j] & sf.d$Event == eventlev[k],]

      temp.d <-temp.d[!is.na(temp.d$SF_gs) & !is.na(temp.d$Us),]
      if(nrow(temp.d) == 0){
        boolplot <- FALSE
        next
      }
      if(i == 1){
        xmax <- max(temp.d$AveUs)
        ymax <- max(temp.d$SF_gs)
        plot(temp.d$AveUs,temp.d$SF_gs, main = paste(sitelev[j],"_",eventlev[k],sep="") ,
             xlab = "friction velocity(m/s)", ylab = "saltation flux (g/s m^2)",
             xlim = c(0,xmax), ylim = c(0,ymax))

      }else{
        par(new = T)
        plot(temp.d$AveUs,temp.d$SF_gs, xlab = "", ylab = "",col = i,
             xlim = c(0,xmax), ylim = c(0,ymax))
      }
    }
    if(boolplot){
      dev.copy(pdf, file=paste(sitelev[j],"_",eventlev[k],".pdf",sep=""), width = 10, height = 10)
      dev.off()
    }
  }
  setTxtProgressBar(pbj, j) 
}