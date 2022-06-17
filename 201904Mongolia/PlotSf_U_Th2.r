library(tcltk2)

# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1401春期モンゴル解析4"

# path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
# path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
# path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1301MongoliaAnalysis8/NsiteRecul" #Nsite only


setwd(path2)
# averate <- c("60","180","300","600","1800")
# averate <- c("60")
averate <- c("60","600")
reteNO <- 1
############################臨界風速等算出###################
i<-1
# sf_filename = paste("Z0Us_MoM_",averate[i],"_sumdata_d_0.csv",sep="")
sf_filename = paste("Z0Us_MoM_",averate[i],"_sumdata.csv",sep="")

sf.d <- read.csv(sf_filename,header=T)
# ust.d <- read.csv(paste("Ust_MoM_",averate[rateNo],"_d_0.csv", sep = ""),header =T)
ust.d <- read.csv(paste("Ust_MoM_",averate[rateNo],".csv", sep = ""),header =T)

sf.d$SiteID <- as.factor(sf.d$SiteID)
sitelev <- levels(sf.d$SiteID)
sf.d$Event <- as.factor(sf.d$Event)
eventlev <- levels(sf.d$Event)[levels(sf.d$Event)!="99"]
par(mfrow=c(1,1))
pbj <- txtProgressBar(min = 1, max = length(sitelev), style = 3)
for(j in 1:length(sitelev)){
  for (k in 1:length(eventlev)) {
    boolplot <- TRUE
      
    temp.d <- sf.d[sf.d$SiteID == sitelev[j] & sf.d$Event == eventlev[k],]

    temp.d <-temp.d[!is.na(temp.d$SF_sl) & !is.na(temp.d$Us),]
    if(nrow(temp.d) == 0){
      boolplot <- FALSE
      next
    }
    
    th.d <- ust.d[ust.d$SiteID == sitelev[j] & ust.d$Event == eventlev[k],]
    
    xmax <- max(temp.d$WS_h)
    ymax <- max(temp.d$SF_sl)
    plot(temp.d$WS_h,temp.d$SF_sl, main = paste(sitelev[j],"_",eventlev[k],sep="") ,
         xlab = "wind speed(m/s)", ylab = "saltation flux (g/s m^2)",
         xlim = c(0,xmax), ylim = c(0,ymax),cex.axis=1.2, cex.lab=1.5,
         cex.main = 1.5)
    
    if(boolplot){
      # dev.copy(pdf, file=paste(path2,"/Us_SF",sitelev[j],"_",eventlev[k],"_d0.pdf",sep=""), width = 10, height = 10)
      dev.copy(pdf, file=paste(path2,"/Us_SF/U_SF_",averate[i],"_",sitelev[j],"_",eventlev[k],".pdf",sep=""), width = 10, height = 10)
      dev.off()
    }

  }
  setTxtProgressBar(pbj, j) 
}
