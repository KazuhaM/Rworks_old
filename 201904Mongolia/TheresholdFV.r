library(tcltk2)
options(scipen=5)
# path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData/avebyn"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"

path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
setwd(path2)
averate <- c("60","180","300","600","1800")
rateNo <- 1

############################臨界風速等算出###################
sf_filename = paste("SfZ0Us_",averate[rateNo],"_sumdata.csv",sep="")
sf.d <- read.csv(sf_filename,header=T)
sitelev <- levels(sf.d$SiteID)
sf.d$Event <- as.factor(sf.d$Event)
eventlev <- levels(sf.d$Event)[levels(sf.d$Event)!="99"]

result.df2 <- data.frame(matrix(rep(NA, 7), nrow=1))[numeric(0), ]
colnames(result.df2) <- c("SiteID","Event","E", "Ut", "c","AveZ0","AveDev")
result.df2$SiteID <- as.character(result.df2$SiteID)
result.df2$Event <- as.character(result.df2$Event)
result.df2$E <- as.numeric(result.df2$E)
result.df2$Ut <- as.numeric(result.df2$Ut)
result.df2$c <- as.numeric(result.df2$c)
result.df2$AveZ0 <- as.numeric(result.df2$AveZ0)
result.df2$AveDev <- as.numeric(result.df2$AveDev)

par(mfrow=c(1,1))

cseq <- c(0.000000001,0.00000001,0.0000001,0.000001,0.00001,0.0001,0.001,0.01,0.1)
icsq <-c(2,3,3,6,6,6,6,6,3,4,3,3,3,0,3,2,3,2,2,3,4,4,3,2,3,3,4)+1
i <- 1
for(j in 1:length(sitelev)){
  for (k in 1:length(eventlev)) {
    sum_sigE <- data.frame(matrix(rep(NA, 3), nrow=1))[numeric(0), ]
    colnames(sum_sigE) <- c("E", "iUt", "ic")
    
    temp.d <- sf.d[sf.d$SiteID == sitelev[j] & sf.d$Event == eventlev[k],]
    if(nrow(temp.d)!=0){
      temp.d <- temp.d[order(temp.d$AveUs),]
      temp.d["dev_n"] <- NaN
      
      counta <- 1
      pbj <- txtProgressBar(min = 1, max = length(seq(0.1,max(temp.d$AveUs[!is.na(temp.d$AveUs)]),by = 0.1))*
                              length(seq(cseq[icsq[i]],100*cseq[icsq[i]],by = cseq[icsq[i]])), style = 3)
      for(iut in seq(0.1,max(temp.d$AveUs[!is.na(temp.d$AveUs)]),by = 0.1)){
        for (ic in seq(cseq[icsq[i]],100*cseq[icsq[i]],by = cseq[icsq[i]])) {
          qmodel <- ic*(1-(iut/temp.d$AveUs[!is.na(temp.d$AveUs)])^2)*temp.d$AveUs[!is.na(temp.d$AveUs)]^3
          e = (temp.d$SF_gs[!is.na(temp.d$AveUs)] - qmodel)^2
          sum_sigE <- rbind(sum_sigE, c(sum(e[!is.na(e)]), iut, ic)) 
          setTxtProgressBar(pbj, counta) 
          counta <- counta +1
        }
      }
      colnames(sum_sigE) <- c("E", "iUt", "ic")
      
      plot(sum_sigE$iUt, sum_sigE$E)
      temp.d$dev_n[temp.d$PC_10_1==99999] <- sqrt(((temp.d$PC_10_2[temp.d$PC_10_1==99999] -
                                                      (temp.d$PC_10_2[temp.d$PC_10_1==99999]+temp.d$PC_10_3[temp.d$PC_10_1==99999])/2)^2 +
                                                     (temp.d$PC_10_3[temp.d$PC_10_1==99999] -
                                                        (temp.d$PC_10_2[temp.d$PC_10_1==99999]+temp.d$PC_10_3[temp.d$PC_10_1==99999])/2)^2)/
                                                    (2*(temp.d$PC_10_2[temp.d$PC_10_1==99999]+temp.d$PC_10_3[temp.d$PC_10_1==99999])^2/4))
      temp.d$dev_n[temp.d$PC_10_1!=99999] <- sqrt((temp.d$PC_10_1[temp.d$PC_10_1!=99999] -
                                                     (temp.d$PC_10_1[temp.d$PC_10_1!=99999]+temp.d$PC_10_2[temp.d$PC_10_1!=99999]+temp.d$PC_10_3[temp.d$PC_10_1!=99999])/3)^2 +
                                                    ((temp.d$PC_10_2[temp.d$PC_10_1!=99999] -
                                                        (temp.d$PC_10_1[temp.d$PC_10_1!=99999]+temp.d$PC_10_2[temp.d$PC_10_1!=99999]+temp.d$PC_10_3[temp.d$PC_10_1!=99999])/3)^2 +
                                                       (temp.d$PC_10_3[temp.d$PC_10_1!=99999] -
                                                          (temp.d$PC_10_1[temp.d$PC_10_1!=99999]+temp.d$PC_10_2[temp.d$PC_10_1!=99999]+temp.d$PC_10_3[temp.d$PC_10_1!=99999])/3)^2)/
                                                    (3*(temp.d$PC_10_1[temp.d$PC_10_1!=99999]+temp.d$PC_10_2[temp.d$PC_10_1!=99999]+temp.d$PC_10_3[temp.d$PC_10_1!=99999])^2/9))
      t.2 <-data.frame(sitelev[j],eventlev[k],as.numeric(min(sum_sigE$E)),
                       as.numeric(sum_sigE$iUt[which.min(sum_sigE$E)]), 
                       as.numeric(sum_sigE$ic[which.min(sum_sigE$E)]),
                       as.numeric(temp.d$AveZ0[1]) ,
                       as.numeric(mean(temp.d$dev_n[!is.na(temp.d$dev_n)])))
      colnames(t.2) <- c("SiteID","Event","E", "Ut", "c","AveZ0","AveDev")
      result.df2 <- rbind(result.df2,t.2)
      i <- i +1
    }
    
    print(paste(j,"番目のサイト",k,"番目のイベント",sep=""))
  }   
}
write.csv(result.df2, paste("Ust_",averate[rateNo],".csv", sep = ""),row.names=FALSE)