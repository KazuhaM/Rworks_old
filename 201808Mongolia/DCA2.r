### ライブラリ読み込み　###
library(vegan)
library(labdsv) #indval用
library (cluster)


### データ読み込み　###
sc1 <- sc[,sc[1,]==0]
d <- sc1[5:nrow(sc1),]
ncol(d)

d[is.na(d)]<-0
d<-t(d)
#d<-t(d)
col<-ncol(d)
row<-nrow(d)
is.numeric(d)
d

# DCAの場合
d.mds<-decorana(d)
d.mds
pn <- 1
summary(d.mds)

#PCAの場合
d.mds<-prcomp(d)
d.mds
plot(d.mds)
summary(d.mds)
biplot(d.mds,choices=1:2)


#MDSの場合
d.mds<-metaMDS(d,zerodist="add") ##### zerodist="add" を追加
stressplot(d.mds) # Shepard diagram を描く。
d.mds
pn <- 2

# DCA_MDS共通
summary(d.mds)
plot(d.mds, type="t")
switch(pn,               
       flname1 <- "DCA",
       flname1 <- "NMDS"
)
dev.copy(pdf, file=paste(flname1,".pdf",sep=""), width = 10, height = 10)
dev.off()


#SA群落
sa.pca <- d.mds$x[,1]
sa.pca <- cbind(sa.pca, sa1[,11:13])
#colnames(sa.pca) <- c(colnames(sa1)[1],colnames(sa1)[11:13])
sa.pca <- as.data.frame(sa.pca)
pairs(sa.pca)
sa.pcalm <- lm(sa.pca~.,sa.pca)
summary(sa.pcalm)

#KN群落
kn.pca <- d.mds$x[,1]
kn.pca <- cbind(kn.pca, kn1[,11:13])
#colnames(kn.pca) <- c(colnames(kn1)[1],colnames(kn1)[11:13])
kn.pca <- as.data.frame(kn.pca)
pairs(kn.pca)
kn.pcalm <- lm(kn.pca~.,kn.pca)
summary(kn.pcalm)

#KN群落DCA
kn.pca <- d.mds$rproj[,1]
kn.pca <- cbind(kn.pca, kn1[,11:13])
#colnames(kn.pca) <- c(colnames(kn1)[1],colnames(kn1)[11:13])
kn.pca <- as.data.frame(kn.pca)
pairs(kn.pca)
kn.pcalm <- lm(kn.pca~.,kn.pca)
summary(kn.pcalm)
