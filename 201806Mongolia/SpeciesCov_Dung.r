d<-read.csv("SpCom.csv",row.names=1)
d[is.na(d)]<-0
d.As<-t(d)
colnames(d.As)

sp<-21
com.no2<-3

d.As[com[,com.no2],sp]

#家畜選択
a<-4
switch(a,               
       "1" = lvst <- 13, #ウマ
       "2" = lvst <- 14, #ラクダ
       "3" = lvst <- 15, #ヒツジ
       "4" = lvst <- 16  #全種
)

#環境データ
e<-read.csv("VegCom.csv",header = T)
e
dungs.all <-e$DngH + e$DngC + e$DngS
e<-cbind(e,dungs.all)

#群落タイプごとの該当場所
com<-NULL
for (i in 1 : 5){
  com<-cbind(com,e[,2]==i)
}

plot(e[com[,com.no2],lvst],d.As[com[,com.no2],sp], xlab="Dung Pellet",ylab="A. splendens Coverage")

