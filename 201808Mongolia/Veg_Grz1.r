setwd("E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2018/現行資料/1101第三回ゼミ発表/解析/VegGrz")

sc <- read.csv("Cover.csv",row.names=1)
sd <- read.csv("Dung.csv",row.names=1)
sh <- read.csv("Height.csv",row.names=1)
pd <- read.csv("PlotData.csv",row.names=1)

###SA群落
pd.sa <- pd[,pd[1,]==1]
pd.sa1 <- t(pd.sa[5:14,])
sd.sa <- sd[,sd[1,]==1]
sd.sa1 <- t(sd.sa[5:8,])
sa1 <- cbind(pd.sa1,sd.sa1) 
pairs(sa1)

sa1.2 <- cbind(sa1[,1], sa1[,3:5], sa1[,11:14])
colnames(sa1.2) <- c(colnames(sa1)[1],colnames(sa1)[3:5],colnames(sa1)[11:14])
pairs(sa1.2)
hist(sa1.2[,1])

sa1.c <- cbind(sa1[,1], sa1[,11:13])
colnames(sa1.c) <- c(colnames(sa1)[1],colnames(sa1)[11:13])
sa1.c <- as.data.frame(sa1.c)
sa1.Cglm <- lm(AllCover~.,sa1.c)
summary(sa1.Cglm)

###KN群落
pd.kn <- pd[,pd[1,]==0]
pd.kn1 <- t(pd.kn[5:14,])
sd.kn <- sd[,sd[1,]==0]
sd.kn1 <- t(sd.kn[5:8,])
kn1 <- cbind(pd.kn1,sd.kn1) 
pairs(kn1)

kn1.2 <- cbind(kn1[,1], kn1[,3:5], kn1[,11:14])
colnames(kn1.2) <- c(colnames(kn1)[1],colnames(kn1)[3:5],colnames(kn1)[11:14])
pairs(kn1.2)
hist(kn1.2[,1])

kn1.c <- cbind(kn1[,1], kn1[,11:13])
colnames(kn1.c) <- c(colnames(kn1)[1],colnames(kn1)[11:13])
kn1.c <- as.data.frame(kn1.c)
kn1.Cglm <- lm(AllCover~.,kn1.c)
summary(kn1.Cglm)
