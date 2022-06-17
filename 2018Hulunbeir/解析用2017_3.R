setwd("E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/論文投稿/修論投稿/解析/植生")
for(l in 1 : 2){
for(n in 1 : 4){
m <-2
a<-l
b<-m
c<-n

#a<-1
#b<-1
#c<-2

#調整用
ms<-1.3		#グラフの上限の倍率、
mxs<-1		#msに加える値、グラフの上端
sas<-0.7		#かっこ間の距離
ts<-0.2		#かっこの左右の長さ
cs<-0.3			#p値の位置
ft<-0.8


#データ読み込み
A<-c("C","R")
B<-c("d","t")
C<-c("CT","SC","14","13")

if (a==1){
	yl<-"All Cover"
}else 
if (a == 2){
	yl<-"Richness"
}

switch(b,               
   "1" = xl<-"Direction",
   "2" = xl<-"Treatment/Year"   
)

switch(c,               
   "1" = cy<-"CT",
   "2" = cy<-"SC",
   "3" = cy<-"14",
   "4" = cy<-"13"
)

flname<-paste(C[c],A[a],"-",B[b], sep ="")

x<-read.csv(paste(flname,".csv",sep=""),header=TRUE)
enn<-ncol(x)


 
x.mean<-apply(x[1:enn-1], 2 , mean , na.rm=T)
x.se<-c()
hm<-c()
sda<-c()
hma<-c()
enna<-enn-1
for (i in 1 : enna){
	#if(is.na(names(table(is.na(x[,i])==FALSE)["TRUE"]))){
	#	hm[i]<-0
	#	break
	#	}else
		print(i)
		if(table(is.na(x[,i])==FALSE)["TRUE"]==length(x[,i])){
			hm[i]<-length(x[,i])
		}else {
			hm[i]<-table(is.na(x[,i])==FALSE)["TRUE"]
		}
	sda[i]<-sd(x[,i], na.rm=T)
	hma[i]<-sqrt(hm[i])
	x.se[i]<-sda [i]/ hma[i]
}

x.sd <- apply(x[1:enn-1], 2, sd, na.rm = T)

#Steel.Dwass用配列
y<-x[!is.na(x[,1:enn-1])]
hmb<-x[,enn]
y<-y[1:length(hmb)]


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
SD <- Steel.Dwass(y,hmb)

#棒グラフの記述
xh<-max(x.mean + x.se)*ms
par(mar = c(5.1, 4.5, 4.1, 2.1))
px <- barplot(x.mean, ylab = yl, xlab = xl, ylim = c(0,xh+mxs), ps = ft,
	main="",
	cex=1.2,
	cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     	cex.main = 1.8)      #  メインタイトルの字の大きさを設定する

#誤差バーを描く、lengthは誤差バーのバーの長さを示す、angleを鋭角にするとバーは矢印状となる
arrows(px, x.mean - x.se, px, x.mean + x.se, angle = 90, length = 0.1)
arrows(px, x.mean + x.se, px, x.mean - x.se, angle = 90, length = 0.1)

#n数を表示
echn<-c()
for(o in 1 : enna){
	echn[o]<-length(x[,o][!is.na(x[,o])])
}
if(enna==4){
	mtext(paste("n=",echn,sep=""),cex=1.2, 
	side=1,line=0,at=seq(0.7, enna*1.1, length=enna))
}else if(enna==3){
	mtext(paste("n=",echn,sep=""),cex=1.2, 
	side=1,line=0,at=seq(0.7, enna*1.05, length=enna))
}

		dev.copy(pdf, file=paste(flname,"a.pdf",sep=""), width = 10, height = 10)
		dev.off()

if (enn == 5) {
	ha<-NULL
	ennb<-enna-1
	ha<-rbind(c(0,1,2),c(3,4,0),c(5,0,0))#
	for (i in 1:ennb){
		ja<-enna-i
	for (j in 1:ja){
		h<-sas *ha[i,j]
		segments(x0 = px[i,]-0.2　, y0 = xh- h -ts, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
		segments(x0 = px[i,]-0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 = xh- h, lwd=0.5)
		segments(x0 = px[i+j,]+0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 =xh- h -ts, lwd=0.5)
		print(ha[i,j])
		hb<- ha[i,j]+1
		sd <- SD[hb]
		print(sd)
		if (sd < 0.001 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("***" ,signif(sd,digits=4))  , font = 1, cex = ft, adj = 0)
		}else if(sd < 0.01 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("**" ,signif(sd,digits=4)), font = 1,cex = ft, adj = 0)
		}else if(sd < 0.05 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("*" ,signif(sd,digits=4)) , font = 1,cex = ft, adj = 0)
		}else{
			text(x = px[i,] , y = xh-h+cs, labels =signif(sd,digits=4) , font = ft, cex = ft, adj = 0)
		} 	
	}
	}
}else if (enn == 4){
	ha<-NULL
	ennb<-enna-1
	ha<-rbind(c(0,1),c(2,0))#
	for (i in 1:ennb){
		ja<-enna-i
	for (j in 1:ja){
		h<-sas *ha[i,j]
		segments(x0 = px[i,]-0.2, y0 = xh - h　-ts, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
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
}else if (enn == 7){
	ha<-NULL
	ennb<-enna-1		#項目間の数
	ha<-rbind(c(0,1,2,3,4),c(5,6,7,8,0),c(9,10,11,0,0),c(12,13,0,0,0),c(14,0,0,0,0))#
	for (i in 1:ennb){
		ja<-enna-i	#ja:
	for (j in 1:ja){
		h<-sas *ha[i,j]
		segments(x0 = px[i,]-0.2, y0 = xh - h　-ts, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
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
}	
		mtext(flname, side = 3, line = 1, at = NA)
		dev.copy(pdf, file=paste(flname,".pdf",sep=""), width = 10, height = 10)
		dev.off()


}}

