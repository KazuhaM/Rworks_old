setwd("E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/論文投稿/修論投稿/解析/ParticleCounter")

d <- read.csv("erosion6.csv",header = T)
d <- d[,c(1,3:10)]
for(i in 1:5){
  d[,i] <- factor(d[,i])  
}
str(d)
lev <- list()
for(i in 1:5){
  lev[[i]] <- levels(d[,i])  
}
lev
l5 <- matrix(c(1,4))
ut <- cbind(c(0,0,0),c(0,0,0))
rownames(ut) <- c("Ut","D","Cov")
colnames(ut) <- c("du","du")
for(i in 1 : length(lev[[1]])){
  for(j in 1 : length(lev[[3]])){
    for(k in 1 : length(lev[[4]])){
      for(l in l5){
      cal <-matrix(0, nrow=2, ncol=2)
      d2 <- d[d$year==lev[[1]][i] & d$try==lev[[3]][j] & d$dune==lev[[4]][k] &
                d$height==lev[[5]][l], ]
        if(nrow(d2) == 0){
          next
        }
      e1 <- c(sum(d2$Wind_Speed_5min^2),sum(d2$Wind_Speed_5min^4))
      e2 <- c(sum(d2$Wind_Speed_5min^4),sum(d2$Wind_Speed_5min^6))
      cal <- rbind(e1,e2)
      r  <- c(sum(d2$Wind_Speed_5min   * d2$count_5min), 
              sum(d2$Wind_Speed_5min^3 * d2$count_5min))
      est <- solve(cal,r)
      if(is.na(sqrt(-est[1]/est[2]))==TRUE){
        print(as.character(d2[1,2]))
        print(paste(i,j,k,l))
        next
      }
      ut <- cbind(ut,c(sqrt(-est[1]/est[2]),est[2],d2$cover[1]))
      dimnames(ut) <- list(c("Ut","D","Cov"), c(colnames(ut)[1:ncol(ut)-1],
                                          paste(lev[[1]][i],"_",
                                                as.character(d2[1,2]),sep="")))
      }
    }
  }
}



