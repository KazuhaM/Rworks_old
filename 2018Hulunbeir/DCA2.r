setwd("E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2018/現行資料/1401生態学会用中国解析2")

### ライブラリ読み込み　###
library(vegan)
library(labdsv) #indval用
library (cluster)

### 分割数設定　任意の数に変更してください　###
gnum_st<-4##地点
gnum_sp<-4　##種

### データ読み込み　###

d<-read.csv("Vegetation.csv",row.names=1)
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


### クラスタリング ###
sco<-scores(d.mds,display="sites") #地点の序列化スコア
clus<-hclust(dist(sco),"ward.D2") #ウォード法改で
#clus<-hclust(dist(sco),"average") #群平均法で
#par(ps=5.5)
plot(clus) #グラフ表示
dev.copy(pdf, file=paste("clus",".pdf",sep=""), width = 10, height = 10)
dev.off()

rect.hclust(clus,gnum_st)
dev.copy(pdf, file=paste("clus_",gnum_st,".pdf",sep=""), width = 10, height = 10)
dev.off()
clus
summary(clus)


### グループ分けした序列を　グラフ表示 ###
k<-cutree(clus,gnum_st)
ordipointlabel(d.mds,display="sites",col = c(1:(gnum_st+1))[k],pch = c(1:gnum_st+1)[k])
ordiellipse(d.mds,k, display="sites",kind="sd", conf=0.9,lwd=1,lty=2, col="black")
dev.copy(pdf, file=paste(flname1,"_",gnum_st,".pdf",sep=""), width = 10, height = 10)
dev.off()

### サイトの所属グループとグルーピングの妥当性MRPP
#サイトの所属
#gnum_st<-6
#k<-cutree(clus,gnum_st)
k

write.table(k,paste("group",gnum_st,".csv",sep=","),sep=",",row.names=T,col.names=T)

clus.ade<-mrpp(d,k)
clus.ade
clus.ade$Pvalue


#クラスタ数の妥当性の検討（シルエット法）
dis <- dist(sco)
sil = silhouette (k,dis)
sil.means <- mean(sil[,3])
plot(sil)
abline(v = sil.means,col = 2, lty=3)
dev.copy(pdf, file="silhouette.pdf", width = 10, height = 10)
dev.off()

### 地点の分割数ごとにリストの並び替えと指標種を計算 ###
#変数初期化
li<-list(NULL)
me<-list(NULL)
ordtxt<-NULL
d2<-d #出力のためのデータ

#最大数に分割したときの順番を取得
tmp<-cbind(clus$order,c(1:row))
tmp<-tmp[order(tmp[,1]),]
tmp<-tmp[,2]

#分割数分だけループ
for( i in 1:gnum_st) {
  ifelse(i==gnum_st,me[[i]]<-tmp,me[[i]]<-cutree(clus,i+1))
  ordtxt[i]<-paste("me[[",i,"]]")#orderのための文字列
  d2<-cbind(d2,me[[i]])
  colnames(d2)[col+i]<-paste("group",i+1,sep="")
  
  dul<-indval(d,me[[i]]) #指標種分析 
  dul<-data.frame(numeric.sp=names(dul$maxcls), community=dul$maxcls, indval=round(dul$indcls,3), pvalue=round(dul$pval,4))
  dul<- dul[order(dul$community, dul$indval, dul$pvalue, decreasing=T),][,-1]
  li[[i]]<-dul #指標種分析結果
}
ord<-eval(parse(text=paste("order(",paste(ordtxt,collapse=","),")")))
d2<-d2[ord,]#並び替え

d2<-t(d2) ###地点、種入れ替え

### 種も同様に並び替え　###
sco.s<-scores(d.mds,display="species")
clus.s<-hclust(dist(sco.s),"average")
me<-list(NULL)
ordtxt<-NULL
tmp<-cbind(clus.s$order,c(1:col)) #クラスターの順番を表の並び替えに利用するため
tmp<-tmp[order(tmp[,1]),]
tmp<-tmp[,2]

for( i in 1:gnum_sp) {
  ifelse(i==gnum_sp,me[[i]]<-tmp,me[[i]]<-cutree(clus.s,i+1))
  ordtxt[i]<-paste("me[[",i,"]]")#orderのための文字列
  d2<-cbind(d2,me[[i]])
  colnames(d2)[row+i]<-paste("group",i+1,sep="")
}

# 並び替えの前に　必要ない行と列にNA代入
for(i in 1:gnum_st){
  for(j in 1:gnum_sp){
    d2[col+i,row+j]<-NA
  }
}
ord<-eval(parse(text=paste("order(",paste(ordtxt,collapse=","),")")))
d2<-rbind(d2[ord,],d2[(col+1):(col+gnum_st),])#並び替え

d2<-d2[,-(row+gnum_sp)]　#並び替えのための最終列を削除
d2<-d2[-(col+gnum_st),] #並び替えのための最終行を削除

### 並び替えた結果と指標種を書き出し
write.table(d2,"結果.csv",sep=",",row.names=T,col.names=T)
for(i in 1:gnum_st) write.table(li[[i]],paste("indval",i,".csv",sep=""),sep=",",row.names=T,col.names=T)
