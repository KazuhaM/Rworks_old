setwd("E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2018/現行資料/1401生態学会用中国解析2/PC")

d <- read.csv("SummaryAllData-19.csv",header = T)
d <- d[,c(2:18)]
d <- d[d[,6]!="",]
d <- d[!is.na(d$DriftSand),]
for(i in 1:4){
  d[,i] <- factor(d[,i])  
}
str(d)
lev <- list()
for(i in 1:4){
  lev[[i]] <- levels(d[,i])  
}
lev

###臨界風速算出
ut <- cbind(c(0,0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0,0))
rownames(ut) <- c("Ut","D","P.C.No.","Event","Date","Cov","Richness",
                  "Ave.Height","DominantAve.Height","Sum.Veg.Vol.",
                  "Sum.DominantVeg.Vol.","E.Veg.Type2","DCA3")
colnames(ut) <- c("du","du")
for(k in 1 : length(lev[[3]])){  
  for(j in 1 : length(lev[[4]])){
      cal <-matrix(0, nrow=2, ncol=2)
      d2 <- d[d$Event==lev[[4]][j] & d$P.C.No.==lev[[3]][k], ]
      d2 <- d2[!is.na(d2[,1]),]
        if(nrow(d2) == 0){
          next
        }
      e1 <- c(sum(d2$WindSpeed^2),sum(d2$WindSpeed^4))
      e2 <- c(sum(d2$WindSpeed^4),sum(d2$WindSpeed^6))
      cal <- rbind(e1,e2)
      r  <- c(sum(d2$WindSpeed   * d2$DriftSand), 
              sum(d2$WindSpeed^3 * d2$DriftSand))
      if(r[1] == 0 & r[2] == 0){
        next
      }
      est <- solve(cal,r)
      if(is.na(sqrt(-est[1]/est[2]))==TRUE){
        print(as.character(d2[1,2]))
        print(paste(j,l))
        next
      }
      ut <- cbind(ut,c(sqrt(-est[1]/est[2]),est[2]
                        ,as.numeric(as.character(d2$P.C.No.[1])),d2$Event[1]
                       ,as.numeric(as.character(d2$Date[1]))
                       ,d2$SiteCover[1],d2$Richness[1]
                       ,d2$Ave.Height[1],d2$DominantAve.Height[1]
                       ,d2$Sum.Veg.Vol.[1],d2$Sum.DominantVeg.Vol.[1]
                       ,as.numeric(d2$E.Veg.Type2[1]),d2$DCA3[1] ))
      dimnames(ut) <- list(c("Ut","D","P.C.No.","Event","Date","Cov","Richness",
                             "Ave.Height","DominantAve.Height","Sum.Veg.Vol.",
                             "Sum.DominantVeg.Vol.","E.Veg.Type2","DCA3"),
                              c(colnames(ut)[1:ncol(ut)-1],
                                          paste(lev[[4]][j],"_",
                                                lev[[3]][k],sep="")))
    }
  }
ut <-ut[,3:ncol(ut)]

ut2 <- t(ut)
pairs(ut2)
plot(ut2[ut2,8],ut2[,1], col=ut2[,3], pch=ut2[,4] )
plot(ut2[,8],ut2[,1], col=ut2[,3])
plot(ut2[ut2[,3]==2,8],ut2[ut2[,3]==2,1])
write.table(ut2, file="ut_Event.txt")


###移動開始限界風速

mut2 <- cbind(c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0))
rownames(mut2) <- c("mUt","P.C.No.","Event","Date","Cov","Richness",
                   "Ave.Height","DominantAve.Height","Sum.Veg.Vol.",
                   "Sum.DominantVeg.Vol.","E.Veg.Type2","DCA3")
colnames(mut2) <- c("du","du")
for(j in 1 : 5){
  d4 <- d[d$Event==lev[[4]][j], ]
  d4 <- d4[!is.na(d4[,1]),]
  if(nrow(d4) == 0){
    next
  }
  
  for(l in 1 : 28){
    if(length(which((d4[,3] == lev[[3]][l])==TRUE))==0){
      next
    }else{
    minW<-min(d4[d4[,5]>= 1 & d4[,3] == lev[[3]][l] ,6])
    d4.t <- d4[d4[,5]>= 1 & d4[,3] == lev[[3]][l] ,]
    #移動開始限界風速の配列作成
    mut2 <- cbind(mut2,c(minW
                       ,as.numeric(as.character(d4.t$P.C.No.[1])),d4.t$Event[1]
                       ,as.numeric(as.character(d4.t$Date[1]))
                       ,d4.t$SiteCover[1],d4.t$Richness[1]
                       ,d4.t$Ave.Height[1],d4.t$DominantAve.Height[1]
                       ,d4.t$Sum.Veg.Vol.[1],d4.t$Sum.DominantVeg.Vol.[1]
                       ,as.numeric(d4.t$E.Veg.Type2[1]),d4.t$DCA3[1] ))
    dimnames(mut2) <- list(c("mUt","P.C.No.","Event","Date","Cov","Richness",
                            "Ave.Height","DominantAve.Height","Sum.Veg.Vol.",
                            "Sum.DominantVeg.Vol.","E.Veg.Type2","DCA3"),
                          c(colnames(mut2)[1:ncol(mut2)-1],
                            paste(lev[[4]][j],"_",
                                  lev[[3]][l],sep="")))
    }
  }
}

mut2 <-mut2[,3:ncol(mut2)]
mut2 <- t(mut2)
mut2 <- mut2[!is.na(mut2[,2]),]
write.table(mut2, file="mut2_EventSite.txt")


str(ut2)

plot(ut2[,10],ut2[,1],col=ut2[,12],pch=ut2[,4],
    xlim = c(0,max(ut2[,10]*1.1)),ylim=c(0,max(ut2[,1])*1.1),xlab="Sum.Veg.Vol.",ylab="Ut")

for(i in 1 : 5){
for(j in 1 : 5){
  ut3 <- ut2[ut2[,12]==i & ut2[,4]==j,]
  result <- lm(ut3[,1]~ut3[,10])
  
  par(new=T)
  plot(ut3[,10],ut3[,1],col=ut3[,12],pch=ut3[,4],xlim = c(0,max(ut2[,10]*1.1)),ylim=c(0,max(ut2[,1])*1.1),xlab="Sum.DominantVeg.Vol.",ylab="Ut")
  abline(result,col=ut3[,12],lty=ut3[,4],xlim = c(0,max(ut2[,10]*1.1)),ylim=c(0,max(ut2[,1])*1.1),xlab="Sum.DominantVeg.Vol.",ylab="Ut")

}}
legend(locator(1),legend=paste("Veg.Type",c(1,2,3,4,5)),col=c(1,2,3,4,5),pch=1)
legend(locator(1),legend=paste("Event",c(1,2,3,4,5)),pch=c(1,2,3,4,5))

###イベント、植生タイプごとの風速-飛砂数　臨界風速と移動開始限界風速
d <- d[d[,6]!="",]
d <- d[!is.na(d$DriftSand),]
  for(j in 1 : 5){
    d3 <- d[d$Event==lev[[4]][j], ]
    d3 <- d3[!is.na(d3[,1]),]
    if(nrow(d3) == 0){
      next
    }
    plot(d3[,6],d3[,5],col=d3[,15]+1,xlim=c(0,max(c(d3[,6],ut2[ut2[,4]==j,1]))),
         main=paste("event",lev[[4]][j]),xlab="WindSpeed",ylab="DriftedSands")
    legend(0,max(d3[,5])*0.9,
           legend=c("Shift","Annual","Grass","Caragana","Caragana_Grass"),col =1:5 ,pch=1)
    mh<-max(d3[,5])*0.9
    for(k in 1 : 29){
      if(length(ut2[ut2[,3]==lev[[3]][k] & ut2[,4]==j,])==0){

      }else{
      ut3 <- ut2[ut2[,3]==lev[[3]][k] & ut2[,4]==j,]
      abline(v = ut3[1],col=ut3[12]+1)
      text(ut3[1],mh,paste(signif(ut3[1],2),"_",ut3[11],sep="")
           ,adj = 0,col=ut3[12]+1)
      mh <- mh-10
    }}

    dev.copy(pdf, file=paste("Event",j,"ThW.pdf",sep=""), width = 10, height = 10)
    dev.off()
  }


mut <- cbind(c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0))
rownames(mut) <- c("mUt","P.C.No.","Event","Date","Cov","Richness",
                  "Ave.Height","DominantAve.Height","Sum.Veg.Vol.",
                  "Sum.DominantVeg.Vol.","E.Veg.Type2","DCA3")
colnames(mut) <- c("du","du")

for(j in 1 : 5){
  d3 <- d[d$Event==lev[[4]][j], ]
  d3 <- d3[!is.na(d3[,1]),]
  if(nrow(d3) == 0){
    next
  }
  plot(d3[,6],d3[,5],col=d3[,15]+1,xlim=c(0,max(d3[,6])),
       main=paste("event",lev[[4]][j]),xlab="WindSpeed",ylab="DriftedSands")
  legend(0,max(d3[,5])*0.9,
         legend=c("Shift","Annual","Grass","Caragana","Caragana_Grass"),col =1:5 ,pch=1)
  mh<-max(d3[,5])*0.9

  for(l in 1 : 5){
    minW<-min(d3[d3[,5]>= 1 & d3[,15]+1 == l ,6])
    abline(v = minW,lty=4,col = l )
    mtext(minW, side = 1, line = 0, at = minW,col = l)
    
    d3.t <- d3[d3[,5]>= 1 & d3[,15]+1 == l ,]
#移動開始限界風速の配列作成
    mut <- cbind(mut,c(minW
                     ,as.numeric(as.character(d3.t$P.C.No.[1])),d3.t$Event[1]
                     ,as.numeric(as.character(d3.t$Date[1]))
                     ,d3.t$SiteCover[1],d3.t$Richness[1]
                     ,d3.t$Ave.Height[1],d3.t$DominantAve.Height[1]
                     ,d3.t$Sum.Veg.Vol.[1],d3.t$Sum.DominantVeg.Vol.[1]
                     ,as.numeric(d3.t$E.Veg.Type2[1]),d3.t$DCA3[1] ))
    dimnames(mut) <- list(c("mUt","P.C.No.","Event","Date","Cov","Richness",
                           "Ave.Height","DominantAve.Height","Sum.Veg.Vol.",
                           "Sum.DominantVeg.Vol.","E.Veg.Type2","DCA3"),
                         c(colnames(mut)[1:ncol(mut)-1],
                           paste(lev[[4]][j],"_",
                                 l,sep="")))
  }
  
  dev.copy(pdf, file=paste("Event",j,"MSThW.pdf",sep=""), width = 10, height = 10)
  dev.off()
}

mut <-mut[,3:ncol(mut)]
mut <- t(mut)
################イベント、植生タイプでまとめて臨界風速
###臨界風速算出

#d[,15] <- factor(d[,15])  

lev2 <- list()
  lev2[[1]] <- levels(factor(d[,15]))  

lev2
ut4 <- cbind(c(0,0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0,0))
rownames(ut4) <- c("Ut","D","P.C.No.","Event","Date","Cov","Richness",
                  "Ave.Height","DominantAve.Height","Sum.Veg.Vol.",
                  "Sum.DominantVeg.Vol.","E.Veg.Type2","DCA3")
colnames(ut4) <- c("du","du")

for(k in 1 : length(lev2[[1]])){  
for(j in 1 : length(lev[[4]])){
    cal <-matrix(0, nrow=2, ncol=2)
    d4 <- d[d$Event==lev[[4]][j] & d$E.Veg.Type2==lev2[[1]][k], ]
    d4 <- d4[!is.na(d4[,1]),]
    if(nrow(d4) == 0){
      next
    }
    e1 <- c(sum(d4$WindSpeed^2),sum(d4$WindSpeed^4))
    e2 <- c(sum(d4$WindSpeed^4),sum(d4$WindSpeed^6))
    cal <- rbind(e1,e2)
    r  <- c(sum(d4$WindSpeed   * d4$DriftSand), 
            sum(d4$WindSpeed^3 * d4$DriftSand))
    if(r[1] == 0 & r[2] == 0){
      next
    }
    est <- solve(cal,r)
    if(is.na(sqrt(-est[1]/est[2]))==TRUE){
      print(as.character(d4[1,2]))
      print(paste(j,l))
      next
    }
    ut4 <- cbind(ut4,c(sqrt(-est[1]/est[2]),est[2]
                     ,as.numeric(as.character(d4$P.C.No.[1])),d4$Event[1]
                     ,as.numeric(as.character(d4$Date[1]))
                     ,d4$SiteCover[1],d4$Richness[1]
                     ,d4$Ave.Height[1],d4$DominantAve.Height[1]
                     ,d4$Sum.Veg.Vol.[1],d4$Sum.DominantVeg.Vol.[1]
                     ,as.numeric(d4$E.Veg.Type2[1]),d4$DCA3[1] ))
    dimnames(ut4) <- list(c("Ut","D","P.C.No.","Event","Date","Cov","Richness",
                           "Ave.Height","DominantAve.Height","Sum.Veg.Vol.",
                           "Sum.DominantVeg.Vol.","E.Veg.Type2","DCA3"),
                         c(colnames(ut4)[1:ncol(ut4)-1],
                           paste(lev[[4]][j],"_",
                                 lev2[[1]][k],sep="")))
  }
}
ut4 <-ut4[,3:ncol(ut4)]

ut4 <- t(ut4)
write.table(ut4, file="ut_Event_VegType.txt")

###イベント、植生タイプごとの臨界風速
d <- d[d[,6]!="",]
d <- d[!is.na(d$DriftSand),]
for(j in 1 : 5){
  d3 <- d[d$Event==lev[[4]][j], ]
  d3 <- d3[!is.na(d3[,1]),]
  if(nrow(d3) == 0){
    next
  }
  plot(d3[,6],d3[,5],col=d3[,15]+1,xlim=c(0,max(c(d3[,6],ut4[ut4[,4]==j,1]))),
       main=paste("event",lev[[4]][j]),xlab="WindSpeed",ylab="DriftedSands")
  legend(0,max(d3[,5])*0.9,
         legend=c("Shift","Annual","Grass","Caragana","Caragana_Grass"),col =1:5 ,pch=1)
  mh<-max(d3[,5])*0.9
  for(k in 0 : 4){
    if(length(ut4[ut4[,4]==j & ut4[,12]==k,])==0){
      
    }else{
      ut4.t <- ut4[ut4[,4]==j & ut4[,12]==k,]
      abline(v = ut4.t[1],col=ut4.t[12]+1)
      text(ut4.t[1],mh,paste(signif(ut4.t[1],2),"_",ut4.t[11],sep="")
           ,adj = 0,col=ut4.t[12]+1)
      mh <- mh-10
    }}
  
  dev.copy(pdf, file=paste("EventNegType",j,"ThW.pdf",sep=""), width = 10, height = 10)
  dev.off()
}

###GLMM


ut2.df <- data.frame(ut2)
mut2.df <- data.frame(mut2)
ut.result <- lm(Ut ~ Cov + DominantAve.Height + DCA3 , ut2.df)
mut2.result <- lm(mUt ~ Cov + DominantAve.Height , mut2.df)
summary(ut.result)
summary(mut2.result)



plot(ut2.df$Cov ,ut2.df$Ut)
plot(ut2.df$Cov ,ut2.df$DominantAve.Height)
lm(Ut ~ Cov * DominantAve.Height, ut4.df)



##ut sumdominantVegVol
utveg.lm <- list()
for(i in 1 : 5 ){
  ut2.t2 <- ut2[ut2[,12]==i-1,]
  ut2.t2.df <- data.frame(ut2.t2)
  utveg.lm[[i]] <- summary(lm(Ut~Sum.DominantVeg.Vol.,ut2.t2.df))
}




#####################################each site#########################
d <- read.csv("SummaryAllData.csv",header = T)
d <- d[,c(2:17)]
d <- d[d[,5]!="",]
d <- d[!is.na(d$DriftSand),]
for(i in 1:3){
  d[,i] <- factor(d[,i])  
}
str(d)
lev <- list()
for(i in 1:3){
  lev[[i]] <- levels(d[,i])  
}
lev
ut <- data.frame(c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0))
rownames(ut) <- c("Ut","D","P.C.No.","Date","Cov","Richness",
                  "Ave.Height","DominantAve.Height","Sum.Veg.Vol.",
                  "Sum.DominantVeg.Vol.","E.Veg.Type","DCA3")
colnames(ut) <- c("du","du")
for(k in 1 : length(lev[[3]])){  
    cal <-matrix(0, nrow=2, ncol=2)
    d2 <- d[d$P.C.No.==lev[[3]][k], ]
    d2 <- d2[!is.na(d2[,1]),]
    if(nrow(d2) == 0){
      next
    }
    e1 <- c(sum(d2$WindSpeed^2),sum(d2$WindSpeed^4))
    e2 <- c(sum(d2$WindSpeed^4),sum(d2$WindSpeed^6))
    cal <- rbind(e1,e2)
    r  <- c(sum(d2$WindSpeed   * d2$DriftSand), 
            sum(d2$WindSpeed^3 * d2$DriftSand))
    if(r[1] == 0 & r[2] == 0){
      next
    }
    est <- solve(cal,r)
    if(is.na(sqrt(-est[1]/est[2]))==TRUE){
      print(as.character(d2[1,2]))
      print(paste(j,l))
      next
    }
    ut <- cbind(ut,data.frame(c(sqrt(-est[1]/est[2]),est[2]
                                ,as.numeric(as.character(d2$P.C.No.[1]))
                                ,as.numeric(as.character(d2$Date[1]))
                                ,d2$SiteCover[1],d2$Richness[1]
                                ,d2$Ave.Height[1],d2$DominantAve.Height[1]
                                ,d2$Sum.Veg.Vol.[1],d2$Sum.DominantVeg.Vol.[1]
                                ,as.character(d2$E.Veg.Type[1]),d2$DCA3[1] )))
    dimnames(ut) <- list(c("Ut","D","P.C.No.","Date","Cov","Richness",
                           "Ave.Height","DominantAve.Height","Sum.Veg.Vol.",
                           "Sum.DominantVeg.Vol.","E.Veg.Type","DCA3"),
                         c(colnames(ut)[1:ncol(ut)-1],
                           lev[[3]][k]))
  }
ut <-ut[,3:ncol(ut)]

ut2 <- t(ut)
pairs(ut2)
plot(ut2[ut2,8],ut2[,1], col=ut2[,3], pch=ut2[,4] )
plot(ut2[,8],ut2[,1], col=ut2[,3])
plot(ut2[ut2[,3]==2,8],ut2[ut2[,3]==2,1])
write.table(ut2, file="ut3.txt")
