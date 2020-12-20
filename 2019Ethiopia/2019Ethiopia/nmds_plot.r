d2<-read.csv("score.csv",row.names=1)
plot(d2$MDS1,d2$MDS2,col=d2$site,pch=15)
legend(0.5,0.6,c("F1","F2","F3","S1","S2","S3"),col=1:6,pch=15)
