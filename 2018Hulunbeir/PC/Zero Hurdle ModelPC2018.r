setwd("E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2018/現行資料/1401生態学会用中国解析2/PC")
library(pscl)
d<-read.csv("SummaryAllData.csv",header=T)
d <- d[,c(2:17)]
d <- d[d[,5]!="",]
d <- d[!is.na(d$DriftSand),]

#2016年データと2017年データを分離
d7<-d[d$year=="2017",]
d6<-d[d$year=="2016",]

#2016年データ解析
for (a in 1 : 3){
for (b in 1 : 3){ 
#a=1
#b=1
if(a==3 && b==3){
	next
}else{

#Mound Name
if (a　==　1){
	switch(b,               
	   "1" = mn6<-"13.1",
	   "2" = mn6<-"14.1",
	   "3" = mn6<-"CT"
	)
}else 
if (a == 2){
	switch(b,               
	   "1" = mn6<-"13.2",
	   "2" = mn6<-"14.2",
	   "3" = mn6<-"CT"
	)
}else 
if (a == 3){
	switch(b,               
	   "1" = mn6<-"13.1",
	   "2" = mn6<-"CT"
	)
}
#Slope Direction & File Name & Making data file
if (a==3){
	fln1 <- paste(a, "_", mn6,"N_f",sep="" )
	fln2 <- paste(a, "_", mn6,"SE_f",sep="" )
	fln3 <- paste(a, "_", mn6,"SW_f",sep="" )
	d6.a<-rbind(d6[d6$try_dune_height==fln1,],d6[d6$try_dune_height==fln2,],d6[d6$try_dune_height==fln3 ,])

}else{
	fln1 <- paste(a, "_", mn6,"W_f",sep="" )
	fln2 <- paste(a, "_", mn6,"E_f",sep="" )
	d6.a<-rbind(d6[d6$try_dune_height==fln1,],d6[d6$try_dune_height==fln2,])
}

#ゼロ切断モデル回帰
d6.a<-d6.a[order(d6.a[,3]),]

#as.integerは整数部分のみ取ってくる。1/sを超えた場合のみ回帰
if(max(d6.a$count_5min)>=1){

#ゼロ切断モデル回帰
d6.h<-hurdle(round(d6.a$count_5min) ~ d6.a$Wind_Speed_5min)
#d6.h.nb<-hurdle(round(d6.a$count_5min) ~ d6.a$Wind_Speed_5min,dist="negbin")

sink(paste("6_",a, "_",mn6,".txt",sep=""),append = TRUE)  
#print(summary(d6.h.p))
#print(summary(d6.h.nb))
sink()  
#if(AIC(d6.h.p)<AIC(d6.h.nb)){
#	d6.h<-d6.h.p
#	md<-"poisson"
#}else{
#	d6.h<-d6.h.nb
#	md<-"negative binomial"
#}
d6.p<-predict(d6.h)

#プロット
windows( width = 10, height = 10)
par(mar = c(5.2, 5, 6, 2.1),pty = "s")
plot(d6.a$Wind_Speed_5min,d6.a$count_5min,
	main="", 
	xlab="Wind Speed (m/s)",
	ylab="Drifting Sands (n/s)",
	xlim=c(0,max(d6.a$Wind_Speed_5min)*1.2),
	ylim=c(0,max(d6.a$count_5min)*1.1),
	cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     	cex.main = 1.8     #  メインタイトルの字の大きさを設定する
)

par(new=T)
plot(d6.a$Wind_Speed_5min, d6.p, type="l", xlab="",ylab="",main="",
	xlim=c(0,max(d6.a$Wind_Speed_5min)*1.2),
	ylim=c(0,max(d6.a$count_5min)*1.1),
	ann = F,axes = F)
#text(1,max(d6.a$count_5min),md)

minW<-min(d6.a$Wind_Speed_5min[d6.a$count_5min >= 1])
abline(v = minW )
mtext(minW, side = 1, line = 0, at = minW)


		dev.copy(pdf, file=paste("6_",a, "_",mn6,".pdf",sep=""), width = 10, height = 10)
		dev.off()
}else{

#プロット
windows( width = 10, height = 10)
par(mar = c(5.2, 5, 6, 2.1),pty = "s")
plot(d6.a$Wind_Speed_5min,d6.a$count_5min,
	main="", 
	xlab="Wind Speed (m/s)",
	ylab="Drifting Sands (n/s)",
	xlim=c(0,max(d6.a$Wind_Speed_5min)*1.2),
	ylim=c(0,max(d6.a$count_5min)*1.1),
	cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     	cex.main = 1.8     #  メインタイトルの字の大きさを設定する
)
		dev.copy(pdf, file=paste("6_",a, "_",mn6,".pdf",sep=""), width = 10, height = 10)
		dev.off()
}
}
}
}

#2017年解析

for (a in 1 : 3){
for (b in 1 : 6){ 
#a=3
#b=1
if(a<3 && b>=4){
	next
}else{

#Mound Name
if (a　==　1){
	switch(b,               
	   "1" = mn7<-"13.1",
	   "2" = mn7<-"14.1",
	   "3" = mn7<-"CT"
	)
}else if (a == 2){
	switch(b,               
	   "1" = mn7<-"13.2",
	   "2" = mn7<-"14.2",
	   "3" = mn7<-"CT"
	)
}else if (a == 3){
	switch(b,               
	   "1" = mn7<-"13.1",
	   "2" = mn7<-"14.1",
	   "3" = mn7<-"14.2",
	   "4" = mn7<-"13.2",
	   "5" = mn7<-"CT",
	   "6" = mn7<-"SC"

	)
}
}
		fln1 <- paste(a, "_", mn7,"_10",sep="" )
		d7.a1<-d7[d7$try_dune_height==fln1,]
		d7.a1<-d7.a1[order(d7.a1[,3]),]

	if(max(d7.a1$count_5min)>=1){
		d7.a1.h<-hurdle(round(d7.a1$count_5min) ~ d7.a1$Wind_Speed_5min)
		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(d7.a1.h))
		sink()  
		d7.a1.p<-predict(d7.a1.h)

		#プロット
		windows( width = 10, height = 10)
		par(mar = c(5.1, 4.5, 4.1, 2.1),pty = "s")
		plot(d7.a1$Wind_Speed_5min,d7.a1$count_5min,
			main="", 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(d7.a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(d7.a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(d7.a1$Wind_Speed_5min, d7.a1.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(d7.a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(d7.a1$count_5min)*1.1),
			ann = F,axes = F,col=1)
		legend(1,max(d7.a1$count_5min),
			as.character(d7.a1$try_dune_height[1]),pch=1,col=1)
		minW<-min(d7.a1$Wind_Speed_5min[d7.a1$count_5min >= 1])
		abline(v = minW ,col=1)
		mtext(minW, side = 1, line = 0, at = minW, co=1)

				dev.copy(pdf, file=paste("7_",a, "_",mn7,"_10.pdf",sep=""), width = 10, height = 10)
				dev.off()
	}


#Slope Direction & File Name & Making data file
if (a==3){
#################################################
	if(b==5){
		fln1 <- paste(a, "_", mn7,"_10",sep="" )
		fln2 <- paste(a, "_", mn7,"_20",sep="" )
		fln3 <- paste(a, "_", mn7,"_f",sep="" )
		d7.a1<-d7[d7$try_dune_height==fln1,]
		d7.a2<-d7[d7$try_dune_height==fln2,]
		d7.a3<-d7[d7$try_dune_height==fln3,]
		d7.a1<-d7.a1[order(d7.a1[,3]),]
		d7.a2<-d7.a2[order(d7.a2[,3]),]
		d7.a3<-d7.a3[order(d7.a3[,3]),]
		PL(d7.a1,d7.a2,d7.a3)
#################################################
	}else if(b==6){
		fln1 <- paste(a, "_", mn7,"_10",sep="" )
		fln2 <- paste(a, "_", mn7,"_20",sep="" )
		fln3 <- paste(a, "_", mn7,"_40",sep="" )
		d7.a1<-d7[d7$try_dune_height==fln1,]
		d7.a2<-d7[d7$try_dune_height==fln2,]
		d7.a3<-d7[d7$try_dune_height==fln3,]
		d7.a1<-d7.a1[order(d7.a1[,3]),]
		d7.a2<-d7.a2[order(d7.a2[,3]),]
		d7.a3<-d7.a3[order(d7.a3[,3]),]
		PL(d7.a1,d7.a2,d7.a3)
	}else{
#################################################
		fln1 <- paste(a, "_", mn7,"_10",sep="" )
		d7.a<-d7[d7$try_dune_height==fln1,]
		d7.a<-d7.a[order(d7.a[,3]),]
		PL(d7.a)
	}
}else{
	if(b==3){
		fln1 <- paste(a, "_", mn7,"_10",sep="" )
		fln2 <- paste(a, "_", mn7,"_20",sep="" )
		fln3 <- paste(a, "_", mn7,"_40",sep="" )
		fln4 <- paste(a, "_", mn7,"_f",sep="" )
		d7.a1<-d7[d7$try_dune_height==fln1,]
		d7.a2<-d7[d7$try_dune_height==fln2,]
		d7.a3<-d7[d7$try_dune_height==fln3,]
		d7.a4<-d7[d7$try_dune_height==fln4,]
		d7.a1<-d7.a1[order(d7.a1[,3]),]
		d7.a2<-d7.a2[order(d7.a2[,3]),]
		d7.a3<-d7.a3[order(d7.a3[,3]),]
		d7.a4<-d7.a4[order(d7.a4[,3]),]
		PL(d7.a1,d7.a2,d7.a3,d7.a4)
	}else{
		fln1 <- paste(a, "_", mn7,"_10",sep="" )
		fln2 <- paste(a, "_", mn7,"_20",sep="" )
		fln3 <- paste(a, "_", mn7,"_40",sep="" )
		d7.a1<-d7[d7$try_dune_height==fln1,]
		d7.a2<-d7[d7$try_dune_height==fln2,]
		d7.a3<-d7[d7$try_dune_height==fln3,]
		d7.a1<-d7.a1[order(d7.a1[,3]),]
		d7.a2<-d7.a2[order(d7.a2[,3]),]
		d7.a3<-d7.a3[order(d7.a3[,3]),]
		PL(d7.a1,d7.a2,d7.a3)
	}
}
	
}
}

##以下テスト
plot(d7$Wind_Speed_5min,d7$count_5min)
plot(d7$Wind_Speed_5min,d7$count_5min)

d7.a<-d7[d$try_dune_height=="3_CT_10",]
d7.a<-d7.a[order(d7.a[,3]),]
plot(d7.a$Wind_Speed_5min,
	d7.a$count_5min,xlim=c(0,7), ylim=c(0,85))


test<-hurdle(as.integer(d7.a$count_5min) ~ d7.a$Wind_Speed_5min)
summary(test)
test.p<-predict(test)
par(new=T)
plot(d7.a$Wind_Speed_5min, test.p, type="l", xlim=c(0,7), ylim=c(0,85))

d7.a<-d7[d7$try_dune_height=="3_CTN_f",]
d7.a<-d7.a[order(d7.a[,3]),]
plot(d7.a$Wind_Speed_5min,
	d7.a$count_5min,xlim=c(0,max(d7.a$Wind_Speed_5min)+1),
	 ylim=c(0,max(d7.a$count_5min)+10))


test2<-hurdle(as.integer(d7.a$count_5min) ~ d7.a$Wind_Speed_5min)
summary(test2)
test2.p<-predict(test2)
par(new=T)
plot(d7.a$Wind_Speed_5min, test2.p, type="l", xlim=c(0,max(d7.a$Wind_Speed_5min)+1),
	 ylim=c(0,max(d7.a$count_5min)+10))




#ゼロ切断モデル回帰
d7.h.nb<-hurdle(round(d7.a$count_5min) ~ d7.a$Wind_Speed_5min, dist = "negbin")

d7.p<-predict(d7.h)
par(new=T)
HM<-function(x){
	-1/(1+exp(d7.h$optim$zero$par[2]*x + d7.h$optim$zero$par[1]))
	+exp(d7.h$optim$count$par[2] *x + d7.h$optim$count$par[1])}
plot(HM,0,12,xlim=c(0,max(d7.a$Wind_Speed_5min)*1.2),
	ylim=c(0,max(d7.a$count_5min)*1.1),
	ann = F,axes = F)
plot(residuals(d7.h) ~ fitted(d7.h))
coef(d7.h)
coef(d7.h, model = "zero")
summary(d7.h.nb)
logLik(d7.h)

# Need to download countreg from https://r-forge.r-project.org/R/?group_id=522
# Install syntax for Windows-based computer
# install.packages("countreg_0.1-5.zip", repos = NULL, type = "win.binary")
library(countreg)
rootogram(d7.h, max = 80) # fit up to count 80
par(new=T)
plot(d7.a$Wind_Speed_5min, d7.p, type="l", xlab="",ylab="",main="",
	xlim=c(0,max(d7.a$Wind_Speed_5min)*1.2),
	ylim=c(0,max(d7.a$count_5min)*1.1),
	ann = F,axes = F)
AIC(d7.h)
AIC(d7.h.nb)



PL<-function(a1,a2 = NULL, a3 = NULL,a4=NULL){
	a1.h<-hurdle(as.integer(a1$count_5min) ~ a1$Wind_Speed_5min)
	print(head(a2))
	if(is.null(a2)==TRUE){
		summary(a1.h)
	}else{
	a2.h<-hurdle(as.integer(a2$count_5min) ~ a2$Wind_Speed_5min)
	return(paste(summary(a1.h),summary(a2.h)))	
	}
}

		minW1<-min(d7.a1$Wind_Speed_5min[d7.a1$count_5min >= 1])
		minW2<-min(d7.a2$Wind_Speed_5min[d7.a2$count_5min >= 1])
		minW3<-min(d7.a3$Wind_Speed_5min[d7.a3$count_5min >= 1])
		minW4<-min(d7.a4$Wind_Speed_5min[d7.a4$count_5min >= 1])
		abline(v = minW1 ,col=1)
		abline(v = minW2 ,col=2)
		abline(v = minW3 ,col=3)
		abline(v = minW4 ,col=4)
		mtext(minW1, side = 1, line = 0, at = minW1, co=1)
		mtext(minW2, side = 1, line = 0, at = minW2, co=2)
		mtext(minW3, side = 1, line = 0, at = minW3, co=3)
		mtext(minW4, side = 1, line = 0, at = minW4, co=4)
