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
#plot(e.dung, e$AllCov)
#result.lm<-lm(e$AllCov~e.dung)
#summary(result.lm)
#abline(result.lm)     
e<-cbind(e,dungs.all)
#pairs(e[,2:16])
#d.mds$rproj
#e.dung

com<-NULL
for (i in 1 : 5){
  com<-cbind(com,e[,2]==i)
}
com

#選択した家畜ごとの平均と標準誤差
dung.ave<-NULL
dung.se<-NULL
#com.num<-NULL
for(i in 1 :5){
  dung.ave[i] <- mean(e[com[,i],lvst])
  dung.se[i] <- sd(e[com[,i],lvst])/sqrt(length(which(com[,i] == TRUE)))
  #dung.sum[i] <- sum(e$dungs.all[com[,i]]) 
  #com.num[i]<-length(which(com[,i] == TRUE))
}
dung.ave
dung.se

#SteelDwass用並べ替え
y<-e[order(e$community),]

#Steel.Dwassの式の出力変更
Steel.Dwass <- function(data,                                                # データベクトル
                        group)                                          # 群変数ベクトル
{
  OK <- complete.cases(data, group)                            # 欠損値を持つケースを除く
  data <- data[OK]
  group <- group[OK]
  n.i <- table(group)                                          # 各群のデータ数
  ng <- length(n.i)                                            # 群の数
  t <- combn(ng, 2, function(ij) {
    i <- ij[1]
    j <- ij[2]
    r <- rank(c(data[group == i], data[group == j]))     # 群 i, j をまとめてランク付け
    R <- sum(r[1:n.i[i]])                                        # 検定統計量
    N <- n.i[i]+n.i[j]                                   # 二群のデータ数の合計
    E <- n.i[i]*(N+1)/2                                  # 検定統計量の期待値
    V <- n.i[i]*n.i[j]/(N*(N-1))*(sum(r^2)-N*(N+1)^2/4)  # 検定統計量の分散
    return(abs(R-E)/sqrt(V))                                # t 値を返す
  })
  p <- ptukey(t*sqrt(2), ng, Inf, lower.tail=FALSE)            # P 値を計算
  #result <- cbind(t, p)                                                # 結果をまとめる
  #rownames(result) <- combn(ng, 2, paste, collapse=":")
  return(p)
  #return(result)
}
#Steel.Dwass(y,hmb)
SD <- Steel.Dwass(y[,lvst] ,y$community)
SD

x.mean<-NULL
x.se<-NULL
x.mean <- dung.ave
x.se <- dung.se
xh<-max(x.mean + x.se)*1.2
px <- barplot(x.mean, ylab = "Dung Pellets", xlab = "Community Type", ylim = c(0,xh))
#誤差バーを描く、lengthは誤差バーのバーの長さを示す、angleを鋭角にするとバーは矢印状となる
arrows(px, x.mean - x.se, px, x.mean + x.se, angle = 90, length = 0.1)
arrows(px, x.mean + x.se, px, x.mean - x.se, angle = 90, length = 0.1)

#調整用
ms<-2		#グラフの上限の倍率、
mxs<-1		#msに加える値、グラフの上端
sas<-0.3		#かっこ間の距離
ts<-0.1		#かっこの左右の長さ
cs<-0			#p値の位置
ft<-0.8   #

#検定結果描画
enna<-5
ha<-NULL
ennb<-enna-1		
ha<-rbind(c(0,1,2,3,4),c(5,6,7,8,0),c(9,10,11,0,0),c(12,13,0,0,0),c(14,0,0,0,0))
for (i in 1:ennb){
  ja<-enna-i	
  for (j in 1:ja){
    h<-sas *ha[i,j]
    segments(x0 = px[i,]-0.2, y0 = xh - h - ts, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
    segments(x0 = px[i,]-0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 = xh- h, lwd=0.5)
    segments(x0 = px[i+j,]+0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 =xh - h -ts, lwd=0.5)
    print(ha[i,j])
    hb<- ha[i,j]+1
    sd <- SD[hb]
    print(sd)
    if (sd < 0.001 ){
      text(x = px[i,] , y = xh-h+cs, labels =paste("***" ,signif(sd,digits=4)) , cex = ft , font = 1, adj = 0)
    }else if(sd < 0.01 ){
      text(x = px[i,] , y = xh-h+cs, labels =paste("**" ,signif(sd,digits=4)), cex = ft , font = 1, adj = 0)
    }else if(sd < 0.05 ){
      text(x = px[i,] , y = xh-h+cs, labels =paste("*" ,signif(sd,digits=4)), cex = ft , font = 1, adj = 0)
    }else{
      text(x = px[i,] , y = xh-h+cs, labels =signif(sd,digits=4) , cex = ft , font = 1, adj = 0)
    } 	
  }
}
