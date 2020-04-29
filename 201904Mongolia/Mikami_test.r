path <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1102春期モンゴル解析3"
setwd(path)

mtest.d <- read.csv("Mikami_et_al_test.csv", header = T)
mtest.d <- mtest.d[mtest.d$RUN <= 4 | mtest.d$RUN == 5 | mtest.d$RUN == 13,]
mtest.d <- mtest.d[(mtest.d$RUN <= 4 & mtest.d$Level >= 3 & mtest.d$Level <= 7) |
                     (mtest.d$RUN == 5 & mtest.d$Level >= 3 & mtest.d$Level <= 6) |
                     (mtest.d$RUN == 13 & mtest.d$Level >= 3 & mtest.d$Level <= 6),]
nI <- length(levels(factor(mtest.d$RUN)))
nJ <- rep(NaN, length= nI)
for (i in 1:nI) {
  nJ[i] <- length(levels(factor(mtest.d[mtest.d$RUN == levels(factor(mtest.d$RUN))[i] ,"Level"])))
}

result.df2 <- data.frame(matrix(rep(NA, 9), nrow=1))[numeric(0), ]
colnames(result.df2) <- c("R","d0","z0","us1","us2","us3","us4","us5","us6")
for (a_d0 in seq(0, 15,by = 0.1)) {
  Ui <- rep(NaN, length= nI) #sigma(j= 1 to J){u_ij}, A
  U2i <- rep(NaN, length= nI) #sigma(j= 1 to J){u^2_ij}, B
  uZi <- rep(NaN, length= nI) #sigma(j= 1 to J){u_ij * ln(z_ij - d0)}, C
  Zi <- rep(NaN, length= nI) #sigma(j= 1 to J){ln(z_ij - d0)}, D
  
  for (i in 1:nI) {
    temp.mtest.d <- mtest.d[mtest.d$RUN == levels(factor(mtest.d$RUN))[i],]
    
    Ui[i] <- sum(temp.mtest.d$u)
    U2i[i] <- sum(temp.mtest.d$u^ 2)
    uZi[i] <- sum(temp.mtest.d$u * log(temp.mtest.d$z - a_d0))
    Zi[i] <- sum(log(temp.mtest.d$z - a_d0))
  }
  
  aupv <- sum(Zi - Ui * uZi / U2i)
  alwv <- nrow(mtest.d) - sum(Ui ^ 2 / U2i) 
  
  a <- aupv / alwv
  bi <- (uZi - a * Ui) / U2i
  usi <- 0.4/bi
  bi <- rep(bi,  nJ)
  # usi <- 0.4/bi
  yij <- mtest.d$u * bi
  
  p_Uij <- log((mtest.d$z - a_d0)/ exp(a))
  Yij <- p_Uij #- log((mtest.d$z - a_d0)/ exp(a))
  
  R_v <-mean((yij - mean(yij))*(Yij - mean(Yij))) /
    (sqrt(mean((yij - mean(yij))^2)) * sqrt(mean((Yij - mean(Yij))^2)))
  
  temp_result <- data.frame(rbind(c(R_v, a_d0, exp(a),usi)))
  colnames(temp_result) <- c("R","d0","z0","us1","us2","us3","us4","us5","us6")
  
  result.df2 <- rbind(result.df2,temp_result)
  colnames(result.df2) <- c("R","d0","z0","us1","us2","us3","us4","us5","us6")
}
plot(result.df2$d0,result.df2$R)
plot(yij,Yij)
result.df2[result.df2$R==max(result.df2$R),]

plot(mtest.d$u, mtest.d$z-result.df2$d0[133], col = rep(1:nI,nJ), xlim = c(0.001,5),ylim = c(0.1,150),log = "y")
legend(4,1,levels(factor(mtest.d$RUN)),col = 1:nI,pch = 1)
for (i in 1:length(usi)) {
  lines(rng <- seq(0,4,by =0.1),exp(logicalrule(result.df2$z0[133],result.df2[133,i+3])) ,col = i)
}

logicalrule <- function(z0,us){
  rng <- seq(0,4,by =0.1)
  return(log(z0)+rng * 0.4/us ) 
}

par(new = T)
plot(p_Uij,mtest.d$z, col = rep(1:nI,nJ), pch = 2, xlim = c(0.001,5),ylim = c(1,150),log = "y")
plot(p_Uij,mtest.d$z, col = rep(1:nI,nJ), pch = 2,log = "y")


temp_P <- 1/ nrow(mtest.d)
for (i in 1:nI) {
  for (j in 1:nJ[i]) {
    temp.mtest.d <- mtest.d[mtest.d$RUN == levels(factor(mtest.d$RUN))[i],]
    (log(temp.mtest.d$z - a_d0) - 0.4 * temp.mtest.d$u/ usi)^2
  }
}