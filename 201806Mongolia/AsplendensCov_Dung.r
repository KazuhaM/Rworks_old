d<-read.csv("SpCom.csv",row.names=1)
d[is.na(d)]<-0
d.As<-t(d)
colnames(d.As)
d.As[,1]

d.As[com[,4],1]

#家畜選択
a<-1
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

plot(e[com[,4],lvst],d.As[com[,4],1], xlab="Dung Pellet",ylab="A. splendens Coverage")

