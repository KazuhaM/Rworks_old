### ライブラリ読み込み　###
library(vegan)
library(labdsv) #indval用

### 分割数設定　任意の数に変更してください　###
gnum_st<-5 ##地点
gnum_sp<-4　##種

### データ読み込み　###

d<-read.csv("SpCom.csv",row.names=1)
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

#MDSの場合
stress.value<-NA
for(i in 1:100){
  set.seed(i)
  nmds.test<-metaMDS(d,k=2,trymax=50)
  stress.value[i]<-nmds.test$stress
}
r.seed<-grep(min(stress.value),stress.value)
set.seed(r.seed)
d.mds<-metaMDS(d,zerodist="add") ##### zerodist="add" を追加
stressplot(d.mds) # Shepard diagram を描く。
d.mds

# DCA_MDS共通
summary(d.mds)
plot(d.mds, type="t")
#biplot(d.mds, scaling=1)
d.mds$rproj

### クラスタリング ###
sco<-scores(d.mds,display="sites") #地点の序列化スコア
clus<-hclust(dist(sco),"ward.D2") #ウォード法改で
#clus<-hclust(dist(sco),"average") #群平均法で
#par(ps=5.5)
plot(clus) #グラフ表示
rect.hclust(clus,gnum_st)
clus
summary(clus)

### グループ分けした序列を　グラフ表示 ###
k<-cutree(clus,gnum_st)
ordipointlabel(d.mds,display="sites",col = c(1:(gnum_st+1))[k],pch = c(1:gnum_st+1)[k])
ordiellipse(d.mds,k, display="sites",kind="sd", conf=0.9,lwd=1,lty=2, col="black")


### サイトの所属グループとグルーピングの妥当性MRPP
#サイトの所属
#gnum_st<-6
#k<-cutree(clus,gnum_st)

#環境データ
e<-read.csv("VegCom.csv",header = T)
e
e.dung <-e$DngH + e$DngC + e$DngS
plot(e.dung, e$AllCov)
result.lm<-lm(e$AllCov~e.dung)
summary(result.lm)
abline(result.lm)     
e<-cbind(e,e.dung)
pairs(e[,2:16])
d.mds$rproj
e.dung

com<-NULL
for (i in 1 : 5){
  com<-cbind(com,e[,2]==i)
}
com
com.no<-5
plot(e.dung[com[,com.no]],d.mds$rproj[com[,com.no],1])
e.dung()

com.no<-5
plot(e.dung[com[,com.no]],d.mds$points[com[,com.no],1])
e.dung()
plot(e.dung,d.mds$points[,1])
