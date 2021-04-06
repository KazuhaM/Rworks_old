setwd("E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2018/現行資料/1401生態学会用中国解析2/PC")
#パッケージインストール
pckname<-"segmented"
if(require(pckname)){
    print(paste(pckname," is loaded correctly"))
} else {
    print(paste("trying to install ",pckname,"..."))
    install.packages(pckname)
    if(require(pckname)){
        print(paste(pckname,"installed and loaded"))
    } else {
        stop("could not install")
    }
}
pckname<-"ggplot2"
if(require(pckname)){
    print(paste(pckname," is loaded correctly"))
} else {
    print(paste("trying to install ",pckname,"..."))
    install.packages(pckname)
    if(require(pckname)){
        print(paste(pckname,"installed and loaded"))
    } else {
        stop("could not install")
    }
}
require(ggplot2)
require(segmented)
#2016年2017年でデータ分け
data<-read.csv("ForRegression.csv",header=T)
#data[,3] <- abs(data[,3])*10
data <- data[!is.na(data[,3]),]
xx <-data$Cover
yy <-data$ErosionPin
x6 <-data$Cover[data$Year==2016]
y6 <-data$ErosionPin[data$Year==2016]
x7 <-data$Cover[data$Year==2017]
y7 <-data$ErosionPin[data$Year==2017]
x8 <-data$Cover[data$Year==2018]
y8 <-data$ErosionPin[data$Year==2018]

dat <- data.frame(x=xx, y=yy)
dat6 <- data.frame(x=x6, y=y6)
dat7 <- data.frame(x=x7, y=y7)
dat8 <- data.frame(x=x8, y=y8)
plot(xx,yy)
#par(mar = c(5.1, 4.5, 4.1, 2.1))
plot(x6,y6,
	main="Relationship between 
	Erosion and Vegetation Coverage",
	xlab="Vegetation Coverage(%)",ylab="ErosionPin(abs,mm)",
	cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
	col=4,pch=1,
	xlim=c(0,max(xx[!is.na(xx)])+5),ylim=c(0,max(yy[!is.na(yy)])+10)
	)
par(new=T)
plot(x7,y7,
	col=2,pch=4,
	xlim=c(0,max(xx[!is.na(xx)])+5),ylim=c(0,max(yy[!is.na(yy)])+10),
	ann = F,axes = F
)
par(new=T)
plot(x8,y8,
     col=3,pch=6,
     xlim=c(0,max(xx[!is.na(xx)])+5),ylim=c(0,max(yy[!is.na(yy)])+10),
     ann = F,axes = F
)
legend(locator(1), legend=c("2016","2017","2018"),col=c(4,2,3),pch=c(1,4,6))


#piecewise regression
##2016
out.lm16 <- lm(y ~ x, data = dat6)
o16 <- segmented(out.lm16, seg.Z = ~x
)
dat216 = data.frame(x = x6, y = broken.line(o16)$fit)
o16.sum<-summary(o16)
confint(o16)
o16.sum$r.squared
##2017
out.lm17 <- lm(y ~ x, data = dat7)
o17 <- segmented(out.lm17, seg.Z = ~x,
  control = seg.control(display = FALSE)
) 
dat217 = data.frame(x = x7, y = broken.line(o17)$fit)
o17.sum<-summary(o17)
confint(o17)
o17.sum$r.squared
##2018
out.lm18 <- lm(y ~ x, data = dat8)
o18 <- segmented(out.lm18, seg.Z = ~x,
                 control = seg.control(display = FALSE)
) 
dat218 = data.frame(x = x8, y = broken.line(o18)$fit)
o18.sum<-summary(o18)
confint(o18)
o18.sum$r.squared

##all
out.lm <- lm(y ~ x, data = dat)
o <- segmented(out.lm, seg.Z = ~x,
  control = seg.control(display = FALSE)
)
dat2 = data.frame(x = xx, y = broken.line(o)$fit)
o.sum<-summary(o)
confint(o)
o.sum$r.squared
#プロット
#windows()
#par(mar = c(5.1, 4.5, 4.1, 2.1))
plot(x6,y6,
	xlab="Vegetation Coverage (%)",ylab="Erosion Pin (abs,mm)",
	cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
	col=1,pch=1,
	xlim=c(0,max(xx)+5),ylim=c(0,max(yy)+10),family="sans" 
	)
par(new=T)
plot(x7,y7,
	col=1,pch=4,
	xlim=c(0,max(xx)+5),ylim=c(0,max(yy)+10),
	ann = F,axes = F
)
par(new=T)
plot(x8,y8,
     col=1,pch=2,
     xlim=c(0,max(xx)+5),ylim=c(0,max(yy)+10),
     ann = F,axes = F
)

plot(o16,add=T,col=gray(0.6),lwd=2,lty=1)
lines(o16,col=gray(0.6))
mtext(signif(o16$psi[2],2), side = 1, line = 0, at = o16$psi[2],col=gray(0.5))

plot(o17, add=T,col=gray(0.3),lwd=2,lty=1)
lines(o17,col=gray(0.3))
mtext(signif(o17$psi[2],2), side = 1, line = 0, at = o17$psi[2],col=gray(0.3))

plot(o18, add=T,col=gray(0),lwd=2,lty=1)
lines(o18,col=gray(0))
mtext(signif(o18$psi[2],2), side = 1, line = 0, at = o18$psi[2],col=gray(0))

plot(o,add=T,col='black',lwd=2,lty=4)
lines(o,col='black',lty=4)
mtext(signif(o$psi[2],2), side = 1, line = 1, at = o$psi[2],col=1)

legend(locator(1), legend=c("2016","2017","2018"),col=gray(c(0.6,0.3,0)),
       pch=c(1,4,2),cex  = 1.5)
legend(locator(1), legend=c("3 years","2016","2017","2018"),col=gray(c(0,0.6,0.3,0)), lty=c(4,1,1,1),lwd=2,cex  = 1.5)


#windows()
par(mar = c(5.1, 4.5, 4.1, 2.1))
plot(x6,y6,
	xlab="Vegetation Coverage (%)",ylab="Erosion Pin (abs,mm)",
	cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
	col=1,pch=1,
	xlim=c(0,max(xx)+5),ylim=c(0,max(yy)+10)
	)
plot(o16,conf.level=0.95,add=T,col=1)
lines(o16,col=1)
mtext(signif(o16$psi[2],2), side = 1, line = 0, at = o16$psi[2],col=1)
text(30,500, paste("Break Point:",signif(o16$psi[2],3),
	"±",signif(o16$psi[3],3)),adj = 0,cex  = 1.5)
text(30,480, paste("R^2 =",signif(o16.sum$r.squared,3)),adj = 0,cex  = 1.5)

#windows()
par(mar = c(5.1, 4.5, 4.1, 2.1))
plot(x7,y7,
	xlab="Vegetation Coverage (%)",ylab="Erosion Pin (abs,mm)",
	cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
	col=1,pch=1,
	xlim=c(0,max(xx)+5),ylim=c(0,max(yy)+10)
	)
plot(o17,conf.level=0.95,add=T,col=1)
lines(o17,col=1)
mtext(signif(o17$psi[2],2), side = 1, line = 0, at = o17$psi[2],col=1)
text(30,500, paste("Break Point:",signif(o17$psi[2],3),
	"±",signif(o17$psi[3],3)),adj = 0,cex  = 1.5)
text(30,480, paste("R^2 =",signif(o17.sum$r.squared,3)),adj = 0,cex  = 1.5)

par(mar = c(5.1, 4.5, 4.1, 2.1))
plot(x8,y8,
     xlab="Vegetation Coverage (%)",ylab="Erosion Pin (abs,mm)",
     cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
     col=1,pch=1,
     xlim=c(0,max(xx)+5),ylim=c(0,max(yy)+10)
)
plot(o18,conf.level=0.95,add=T,col=1)
lines(o18,col=1)
mtext(signif(o18$psi[2],2), side = 1, line = 0, at = o18$psi[2],col=1)
text(30,500, paste("Break Point:",signif(o18$psi[2],3),
                   "±",signif(o17$psi[3],3)),adj = 0,cex  = 1.5)
text(30,480, paste("R^2 =",signif(o18.sum$r.squared,3)),adj = 0,cex  = 1.5)

#windows()
par(mar = c(5.1, 4.5, 4.1, 2.1))
plot(xx,yy,
	xlab="Vegetation Coverage (%)",ylab="Erosion Pin (abs,mm)",
	cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
	col=1,pch=1,
	xlim=c(0,max(xx)+5),ylim=c(0,max(yy)+10)
	)
plot(o,conf.level=0.95,add=T,col=1)
lines(o,col=1)
mtext(signif(o$psi[2],2), side = 1, line = 0, at = o$psi[2],col=1)
text(30,500, paste("Break Point:",signif(o$psi[2],3),
	"±",signif(o$psi[3],3)),adj = 0,cex  = 1.5)
text(30,480, paste("R^2 =",signif(o.sum$r.squared,3)),adj = 0,cex  = 1.5)
###########################################################################

#exponential
resid <- function(par)	# 関数名は何でも良い。引数は，パラメータのベクトル。x, y は，大域変数として参照する。
{
	yhat <- par[1]+par[2]*exp(-par[3]*x6)
	sum((y6-yhat)^2)	# 残差平方和を返す
}
st<-c(1,1,1)
optim(st, resid)
ea16<-list(a=optim(st, resid)$par[1],
	b = optim(st, resid)$par[2],
	c = optim(st, resid)$par[3])
e16.model <- nls(y6 ~ a+b*exp(-c*x6), dat6, start=ea16)

resid <- function(par)	# 関数名は何でも良い。引数は，パラメータのベクトル。x, y は，大域変数として参照する。
{
	yhat <- par[1]+par[2]*exp(-par[3]*x17)
	sum((y17-yhat)^2)	# 残差平方和を返す
}
st<-c(20,20,20)
optim(st, resid)
ea17<-list(a=optim(st, resid)$par[1],
	b = optim(st, resid)$par[2],
	c = optim(st, resid)$par[3])
#ea17<-list(a=100,b = 100,c = 1)
e17.model <- nls(y17 ~ a+b*exp(-c*x17), dat17, start=ea17)

resid <- function(par)	# 関数名は何でも良い。引数は，パラメータのベクトル。x, y は，大域変数として参照する。
{
	yhat <- par[1]+par[2]*exp(-par[3]*xx)
	sum((yy-yhat)^2)	# 残差平方和を返す
}
st<-c(1,1,1)
eax<-list(a=optim(st, resid)$par[1],
	b = optim(st, resid)$par[2],
	c = optim(st, resid)$par[3])
e.model <- nls(yy ~ a+b*exp(-c*xx), dat, start=eax)

predict.e16 <- predict(e16.model)
predict.e17 <- predict(e17.model)
predict.e <- predict(e.model)
 plot(x6, y6, ann=F, xlim=c(0,40), ylim=c(0,10000));   par(new=T)
 plot(x6,predict.e16, type="l", xlim=c(0,40), ylim=c(0,10000))

#inverse
e16.model <- nls(y6 ~ a+b/x6, dat6, start=list(a=1, b=1))
i17.model <- nls(y ~ a+b/x, dat17, start=list(a=1, b=1))
i.model <- nls(y ~ a+b/x, dat, start=list(a=1, b=1))

#logit
l16.model <- nls(y ~ a-b*exp(-exp(c+d*log(x))), dat16, start=list(a=1, b=1, c=1, d=1))
l17.model <- nls(y ~ a-b*exp(-exp(c+d*log(x))), dat17, start=list(a=1, b=1, c=1, d=1))
l.model <- nls(y ~ a-b*exp(-exp(c+d*log(x))), dat, start=list(a=1, b=1, c=1, d=1))




