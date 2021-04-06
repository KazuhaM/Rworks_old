
# library import ----------------------------------------------------------
library(MuMIn) # AICのためのdredge関数
library(beeswarm)
library(car) # 多重共線性 virの算出
library(pwr) # 検出力計算　重回帰はpwr.f2.test
# Init setting ------------------------------------------------------------
# work directory
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
# path3 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
setwd(path3)
path4 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1202PhDthesis_submission/Fig"
# path4 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1202PhDthesis_submission/Fig"
# data source
d <- read.csv("Veg_EroAnalysis14.csv", header =T)
d.b <-read.csv("Veg_EroAnalysis14_B.csv", header =T)
d.soil <- read.csv("Veg_EroAnalysis14_soil.csv", header =T)

# 型変換
d$SiteID <- as.factor(d$SiteID)
d$Event <- as.factor(d$Event)
d$ID <- as.factor(d$ID)
d$Type <- as.factor(d$Type)
d$Type2 <- as.factor(d$Type2)
d$Z0_scale <- as.factor(d$Z0_scale)
# str(d)

# soil data
d.soil$ID <- as.factor(d.soil$ID)
d.soil$Type <- as.factor(d.soil$Type)

### Nitrariaサイトを除いてSAのみに
d <- d[d$Type2 !="Nitraria",]
d.soil <- d.soil[d.soil$Type !="Nitraria",]

### D to c
# c = D* \rho / g
# \rho = 1293g m^-3 = , g = 9.8 m s^-2
d$D <- d$D * 1293 / 9.8




# クラスターによるグループ分け ----------------------------------------------------------

# 植被率、群落高、rangeのみのデータフレームを作る
veg.cond <- cbind(d$VegCover,d$VegComHgt,d$AsioV5range)
colnames(veg.cond) <- c("VegCover","VegComHgt","AsioV5range")
row.names(veg.cond) <- d$ID

# クラスター分類J
gnum_st <- 3
clus<-hclust(dist(veg.cond),"ward.D2") #ウォード法改で
par(mar = c(5,6,3,2),family = family_serif)
plot(clus,cex = 1.7,cex.axis=1.5,cex.lab = 1.7, main = "",
     ylab = "Ward's cluster distance",xlab = "SiteID _ EventNo.",
     sub = "") #グラフ表示
# dev.copy(cairo_pdf, file=paste(path4,"/cluster.pdf",sep=""), width = 10, height = 7)
# dev.off()

# text(1.7,15,"1")
# clus.g<- rect.hclust(clus,gnum_st)

# クラスター結果のグループ分けを取得
clus.group <- rep(0,length= nrow(veg.cond))
for(i in 1:nrow(veg.cond)){
  for(j in 1 : gnum_st){
    if(sum(names(clus.g[[j]]) == row.names(veg.cond)[i])==1){
      break
    }
  }
  clus.group[i] <- j
}

# 植被率の小さい順に並べ替え
clus.group[clus.group == 1] <- 4
clus.group <- clus.group - 1

# 全情報の入ったデータフレームにグループ情報を付加
veg.cond <- cbind(veg.cond, clus.group)
colnames(veg.cond)[length(colnames(veg.cond))] <- "Group"
d2 <- cbind(d,clus.group )
d2$clus.group <- as.factor(d2$clus.group)


# Analysis about "D" ------------------------------------------------------

# d2$D <- 1/d2$D
# d2 <- d2[1:(nrow(d2)-1),]
result.lmD <- glm(D ~ VegCover + AsioV5range + Z0 + VegComHgt, data = d2,
                  family = Gamma(link = log))
summary(result.lmD)
result.lmD <- glm(D ~ (VegComHgt +  AsioV5range)^2,
                  data = d2,family = Gamma(link = log))
summary(result.lmD)
result.lmD <- glm(D ~ VegComHgt, data = d2,
                  family = Gamma(link = log))
summary(result.lmD)

# result.lmD2 <- lm(D ~ VegCover*AsioV5range, data = d2)
# summary(result.lmD)

# GLM決定係数算出
library(performance)
r2(result.lmD)

# library(MuMIn)
# options(na.action = "na.fail")
# dredge(result.lmD,rank="AIC")
# 
# cor.res <- cor(d2[,c("VegCover","AsioV5range","Z0",
#                     "VegComHgt")])
# vif.res <- 1/(1-cor.res^2)
# vif.res
# library(rgl)
# plot3d(d2$D,d2$VegComHgt,d2$AsioV5range)
par(mar = c(5,6,3,2),family = family_serif)
plot(D ~ VegComHgt, data = d2, xlim = c(0,16),
     col =rainbow(12)[round(d2$AsioV5range,0)],cex = 2,pch = 15,
     cex.lab = 1.7, cex.axis = 1.5,ylab = expression(paste("c (" , m^{-1},")")),
     xlab = "community height (cm)")
legend(0,10,title  ="range",1:max(round(d2$AsioV5range,0)),col = rainbow(12)[1:max(round(d2$AsioV5range,0))],pch=15,
       cex = 1.2)
x <- seq(2,16,by = 0.1)
eta.pred <- result.lmD$coefficients[1] + result.lmD$coefficients[2]*x
lines(x,exp(eta.pred))
# abline(result.lmD)
# plot(D ~ VegCover, data = d2,log = "y")
# plot(D ~ Z0, data = d2)
# plot(VegComHgt ~ VegCover , data = d2)
# plot(D ~ AsioV5range, data = d2,col = rainbow(9)[round(d2$VegComHgt,0)-7],cex = 2,pch = 15)
# plot(D ~ VegComHgt, data = d2)
# plot(AsioV5range~ VegComHgt,data = d2)
# hist(d2$D)


# グループ間比較
# layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))
# par(mar = c(5,6,3,2))


# library("rgl")
# plot3d(d2$D, d2$VegCover,d2$AsioV5range)
# test.cr <- d2$VegComHgt * d2$AsioV5range
# plot(d2$D~test.cr)
# d2[d2$clus.group ==2,"AsioV5range" ]

# plot(D ~ VegCover, data = d2,log = "y")
# plot(D ~ Z0, data = d2)
# plot(VegComHgt ~ VegCover , data = d2)


# 回帰やプロットで色々試すところ ---------------------------------------------------------

# 回帰系でいろいろ試しているところ
# 係数cについて
plot(D ~ VegCover, data = d2,col = rainbow(8)[round(d2$AsioV5range,0)],cex = 2,pch = 16)
legend(30,12,title  ="range",1:max(round(d2$AsioV5range,0)),col = rainbow(8)[1:max(round(d2$AsioV5range,0))],pch=15,
       cex = 1.2)

range.t <- 4
plot(D ~ VegCover, data = d2[d2$AsioV5range<=range.t,],col = 2,cex = 2,pch = 15
     ,ylim = c(0,14),xlim = c(0,47))
par(new =T)
plot(D ~ VegCover, data = d2[d2$AsioV5range>range.t,],col = 3,cex = 2,pch = 16
     ,ylim = c(0,14),xlim = c(0,47))
legend(30,12,title  =range.t,1:max(round(d2$AsioV5range,0)),col = rainbow(8)[1:max(round(d2$AsioV5range,0))],pch=15,
       cex = 1.2)

range.t <- 4
plot(D ~ VegComHgt, data = d2[d2$AsioV5range<=range.t,],col = 2,cex = 2,pch = 15
     ,ylim = c(0,14),xlim = c(0,18))
par(new =T)
plot(D ~ VegComHgt, data = d2[d2$AsioV5range>range.t,],col = 3,cex = 2,pch = 16
     ,ylim = c(0,14),xlim = c(0,18))

plot(VegCover~AsioV5range, data =d2,cex = 2,pch = 15) 

plot(D~AsioV5range, data =d2,cex = 2,pch = 15) 
plot(D~AsioV5range, data =d2[d2$D<4,],cex = 2,pch = 15) 

plot(D ~ VegComHgt, data = d2[d2$D<4,],col = 2,cex = 2,pch = 15
     ,ylim = c(0,4),xlim = c(0,18))

plot(D ~ VegComHgt, data = d2,col = d2$clus.group,
     cex = 2,pch = 15,xlim = c(0,20))
legend(14,12,title  ="group",levels(d2$clus.group),
       col = as.integer(levels(d2$clus.group)),pch=15,
       cex = 1.2)

plot(AsioV5range ~ VegCover, data = d2)

# 粗度について
plot(Z0 ~ VegCover, data = d2,col = rainbow(8)[round(d2$AsioV5range,0)],
     cex = 2,pch = 16,log="y")
legend(30,0.01,title  ="range",1:max(round(d2$AsioV5range,0)),
       col = rainbow(8)[1:max(round(d2$AsioV5range,0))],pch=15,
       cex = 1.2)

range.t <- 5
plot(Z0 ~ VegCover, data = d2[d2$AsioV5range<=range.t,],col = 2,cex = 2,pch = 15
     ,ylim = c(0,0.012),xlim = c(0,47))
par(new =T)
plot(Z0 ~ VegCover, data = d2[d2$AsioV5range>range.t,],col = 3,cex = 2,pch = 16
     ,ylim = c(0,0.012),xlim = c(0,47))
legend(30,12,title  =range.t,1:max(round(d2$AsioV5range,0)),col = rainbow(8)[1:max(round(d2$AsioV5range,0))],pch=15,
       cex = 1.2)

range.t <- c(3,5)
plot(Z0 ~ VegComHgt, data = d2[d2$AsioV5range<=range.t[1],],col = 2,cex = 2,pch = 15
     ,ylim = c(0.0001,0.012),xlim = c(0,18),log="y")
par(new =T)
plot(Z0 ~ VegComHgt, data = d2[d2$AsioV5range<=range.t[2]&d2$AsioV5range>range.t[1],],
     col = 3,cex = 2,pch = 16
     ,ylim = c(0.0001,0.012),xlim = c(0,18),log="y")
par(new =T)
plot(Z0 ~ VegComHgt, data = d2[d2$AsioV5range>range.t[2],],col = 4,cex = 2,pch = 16
     ,ylim = c(0.0001,0.012),xlim = c(0,18),log="y")
legend(0,0.001,title  ="range",c("<=3","3<,<=5","5<"),col = 2:4,pch=15,
       cex = 1.2)


plot(Z0 ~ VegComHgt, data = d2[d2$clus.group==1,],col = 2,cex = 2,pch = 15
     ,ylim = c(0,0.012),xlim = c(0,18))
par(new =T)
plot(Z0 ~ VegComHgt, data = d2[d2$clus.group==2,],
     col = 3,cex = 2,pch = 16
     ,ylim = c(0,0.012),xlim = c(0,18))
par(new =T)
plot(Z0 ~ VegComHgt, data = d2[d2$clus.group==3,],col = 4,cex = 2,pch = 16
     ,ylim = c(0,0.012),xlim = c(0,18))

plot(Z0~AsioV5range, data =d2,cex = 2,pch = 15,log ="y") 
plot(Z0~AsioV5range, data =d2[d2$D<4,],cex = 2,pch = 15) 

plot(Z0 ~ VegComHgt, data = d2[d2$D<4,],col = 2,cex = 2,pch = 15
     ,ylim = c(0,4),xlim = c(0,18))

plot(Z0 ~ VegComHgt, data = d2,col = d2$clus.group,
     cex = 2,pch = 15,xlim = c(0,20))
legend(14,12,title  ="group",levels(d2$clus.group),
       col = as.integer(levels(d2$clus.group)),pch=15,
       cex = 1.2)

result.lmZ <- glm(Z0 ~ (VegComHgt+VegCover+AsioV5range)^2, data = d2,
                  family = Gamma)
summary(result.lmZ)

result.lmZ <- lm(Z0 ~ (VegComHgt+VegCover+AsioV5range), data = d2)
summary(result.lmZ)

test.z <-  d2$AsioV5range / (d2$VegComHgt/100)

test.z2 <- d2$Z0 
result.lmZ2 <- lm(test.z2 ~ test.z)
summary(result.lmZ2)
plot(test.z2~test.z)
plot(test.z~d2$clus.group,ylim = c(0,80 ))
result.lm.c <-  glm(d2$D ~ test.z,
                    family = Gamma(link = log))

# plot(D ~ VegComHgt, data = d2)
# plot(AsioV5range~ VegComHgt,data = d2)
# hist(d2$D)



# Analysis about others of D ----------------------------------------------

# ust~ z0
d2.logz <- d2
d2.logz$Z0 <- log(d2.logz$Z0)
result.lmUst <- lm(Ut ~ VegCover + AsioV5range  + VegComHgt + Z0, data = d2.logz)
summary(result.lmUst)

d2.logz <- d2
d2.logz$Z0 <- log(d2.logz$Z0)
result.lmUst <- lm(Ut ~ Z0, data = d2.logz)
summary(result.lmUst)

# plot ust ~ z0
par(mar = c(5,6,3,2))
plot(Ut ~ Z0, data = d2, xlim = c(10^(-4),max(d2$Z0)*1.1),
     ylim = c(0, max(d2$Ut)*1.1),
     cex = 2,pch = 16, cex.lab = 1.7, cex.axis = 1.5,
     ylab = "thrishold friction velocity (m/s)",
     xlab = "log(z_0) (m)",log = "x",xaxt = "n")
sLab <- c(expression(10^{-4}),expression(10^{-3}),
          expression(10^{-2}),expression(10^{-1}))
axis(side=1,          #side2:左
     at=10^(-4:-1), #0から8まで1ずつ
     tck=0.03,            #長さ0.03のティック
     labels=sLab,
     mgp=c(1,1,0),
     cex.axis = 1.7
)

fPow <- function(x){(2:9)*10^x}
axis(side=1,        #side2:左
     at=sapply(-4:-1, fPow), #繰り返し(2:9)×10^(iLogL:iLogU)
     tck=0.01,          #長さ0.01のティック
     labels=FALSE,      #ラベル出力なし
     mgp=c(1,0.5,0)
)
x <- seq(min(d2$Z0),max(d2$Z0),by = 0.01)
eta.pred <- result.lmUst$coefficients[1] + result.lmUst$coefficients[2]*log(x)
lines(x,eta.pred)

# グループ間比較
# 正式作図用
# par("mar")
# layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))
# par(mar = c(5,6,3,2))
# plot(d2$clus.group,d2$Ut,xlab = "",ylab = "threshold friction velocity (m/s)",
#      ylim = c(0, max(d2$Ut)*1.1),
#      cex.axis=1.7,cex.lab =2)
# plot(d2$clus.group,d2$D,xlab = "group",ylab = "D (gsm^-2)",
#      ylim = c(0, max(d2$D)*1.1),
#      cex.axis=1.7,cex.lab =2)
# # plot(d2$clus.group,d2$meanSF,xlab = "",ylab = "saltation flux (g/s m^2)",
# #      ylim = c(0, max(d2$meanSF)*1.1),
# #      cex.axis=1.7,cex.lab =2)
# #########
# plot(d2$clus.group,d2$Z0,log = "y",xlab = "",ylab = "roughness (m)",
#      ylim = c(10^(-4),0.15),
#      cex.axis=1.7,cex.lab =2
#      ,yaxt = "n",)
# 
# sLab <- c(expression(10^{-4}),expression(10^{-3}),
#           expression(10^{-2}),expression(10^{-1}))
# 
# axis(side=2,          #side2:左
#      at=10^(-4:-1), #0から8まで1ずつ
#      tck=0.03,            #長さ0.03のティック
#      labels=sLab,
#      mgp=c(1,0.5,0),
#      cex.axis = 1.7
# )
# fPow <- function(x){(2:9)*10^x}
# axis(side=2,        #side2:左
#      at=sapply(-4:-1, fPow), #繰り返し(2:9)×10^(iLogL:iLogU)
#      tck=0.01,          #長さ0.01のティック
#      labels=FALSE,      #ラベル出力なし
#      mgp=c(1,0.5,0)
# )
# plot(d2$clus.group,d2$AsioV5range,xlab = "",ylab = "range (m)",
#      ylim = c(0,max(d2$AsioV5range)*1.1),
#      cex.axis=1.7,cex.lab =2)
# plot(d2$clus.group,d2$VegCover,xlab = "cluster group",ylab = "cover (%)",
#      ylim = c(0,max(d2$VegCover)*1.1),
#      cex.axis=1.7,cex.lab =2)
# plot(d2$clus.group,d2$VegComHgt,xlab = "",ylab = "com. height (cm)",
#      ylim = c(0,max(d2$VegComHgt)*1.1),
#      cex.axis=1.7,cex.lab =2)
# 
# # sf
plot(d2$clus.group,d2$meanSF,xlab = "",ylab = "saltation flux (g/s m^2)",
     ylim = c(min(d2$meanSF)*0.9, max(d2$meanSF)*1.1),
     cex.axis=1.7,cex.lab =2,log = "y")


plot(meanSF~VegCover,data = d2 ,ylab = "saltation flux (g/s m^2)",xlab= "cover (%)",
     cex.axis=1.7,cex.lab =2,pch = 16)


# 黒崎先生ご指摘、fλ算出 ------------------------------------------------------------
# d2
# 1．community heightをhとして、z0/hを計算。
temp.hm <- d2$VegComHgt / 100
temp.z0h <- d2$Z0/temp.hm

# 2. 私のスライド16にForoutan et al. (2017)を載せていますが、
# z0/h=0.96λ^1.07の関係式から 
# λ ：roughness density (Shao 2008のfrontal area index)を計算。
# log(λ) = {log(z0h / 0.96)}/1.07
temp.lambda2 <- {log(temp.z0h/0.96)}/1.07
temp.lambda <- exp(temp.lambda2)

# 3. Shao (2008) eq. (9.22)の式でf(λ)が得られます。
# Shao (2008)の値が異なりますが、WRF-Chemでは、m=0.5, σ=1.0, β=200が使われる。
# m=0.5, σ=1.0, β=200
par.mr <- 0.5
par.sigr <- 1.0
par.betar <- 200
temp.flambda <- cul.flambda(temp.lambda, par.mr, par.sigr, par.betar)

cul.flambda <- function(lambda,mr, sigr, betar){
  y <- sqrt(1 - mr * sigr * lambda) * 
          sqrt(1 + mr * betar * lambda)
}

# 4. 私のスライド2に載せた臨界摩擦速度の式において、
# 土壌水分、クラストなどの効果を無視(fw=fsc=fc=1)とすると 
# ut = ut0×f(λ)になりますので、Shaoの教科書に書かれている理論では、
# これで線形関係に持って行けます。

plot(temp.flambda,d2$Ut,ylim = c(0,2))

# ust~ fλ
result.lmUstlam <- lm(d2$Ut ~ temp.flambda)
summary(result.lmUstlam)
d2$flambda <- temp.flambda

# λ = -0.35 ln(1 - 0.01 * VC) (Shao, 2008)の式で推定するλ
temp.pred.lambda <- -0.35 * log(1 - 0.01 * d2$VegCover )

par(mar = c(5,6,3,2),family = family_serif)
plot(temp.pred.lambda ~ temp.lambda, xlim = c(0,0.15),
     ylim = c(0,0.25),
     cex = 2,pch = 16, cex.lab = 2, cex.axis = 1.7,
     ylab = expression(paste(lambda," predicted by Eq.")),
     xlab = expression(paste(lambda," calcurated with ",z[0])),
     col = topo.colors(8)[round(d2$AsioV5range,0)])
legend(0.12,0.25,1:max(round(d2$AsioV5range,0)),
      title = "range",col =topo.colors(8)[1:max(round(d2$AsioV5range,0))],pch=16)
lines(c(0,0.15),c(0,0.15))

# λ ~ VC
plot(temp.flambda~d2$VegCover,xlim = c(0,50),ylim = c(0,4))
temp.vegr <- d2$VegCover * d2$AsioV5range
plot(temp.flambda~temp.vegr,ylim = c(0,4))

plot(temp.flambda ~ d2$VegCover, xlim = c(0,50),
     ylim = c(0,4),
     cex = 2,pch = 16, cex.lab = 2, cex.axis = 1.7,
     ylab = expression(f[lambda]),
     xlab ="cover (%)",
     col = topo.colors(8)[round(d2$AsioV5range,0)])
legend(0,1,1:max(round(d2$AsioV5range,0)),
       title = "range",col =topo.colors(8)[1:max(round(d2$AsioV5range,0))],pch=16)

# 粗度要素と植生要素　再解析 -----------------------------------------------------------

d5 <- read.csv("Veg_EroAnalysis15.csv", header =T)

# 型変換
d5$SiteID <- as.factor(d5$SiteID)
d5$Event <- as.factor(d5$Event)
d5$ID <- as.factor(d5$ID)

### D to c
# c = D* \rho / g
# \rho = 1293g m^-3 = , g = 9.8 m s^-2
d5$D <- d5$D * 1293 / 9.8

# グループ
d5$clus.group<- rep(NA,length = nrow(d5))

d5$clus.group[!is.na(d5$Ut)] <- as.factor(clus.group)
d5$clus.group  <- as.factor(d5$clus.group)

# グループ間比較
layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))
par(mar = c(5,7,3,1),xpd=F,family = family_serif)
boxplot(AveHeight~clus.group, data = d5)
boxplot(QuadHeight~clus.group, data = d5)
boxplot(QuadRichness~clus.group, data = d5)
boxplot(V5range~clus.group, data = d5)
boxplot(rh~clus.group, data = d5.sel)
boxplot(d5.sel$rh ~ d5$clus.group[!is.na(d5$clus.group )])

d5$lambda = rep(NA,length = nrow(d5))
d5$lambda[!is.na(d5$Ut)] <- temp.lambda
d5$z0h = rep(NA,length = nrow(d5))
d5$z0h[!is.na(d5$Ut)] <- temp.z0h
d5$flambda = rep(NA,length = nrow(d5))
d5$flambda[!is.na(d5$Ut)] <- temp.flambda

d5.sel <- d5[!is.na(d5$Ut),c("ID","lambda","z0h","flambda","QuadCover",
                             "QuadHeight","AveHeight","meanSF","D","V5range",
                             "AsioV5range","QuadRichness","Ut","Z0")]
# d5.sel <- d5.sel[1:9,]
# pairs(d5.sel)
d5.sel$rh <- d5.sel$AsioV5range*2/(d5.sel$QuadHeight/100)
# d5.sel$rh <- d5.sel$rh / d5.sel$QuadCover
# d5.sel$rh2 <- d5.sel$AsioV5range*100/d5.sel$QuadHeight

# lmd5.lam  <- lm(lambda ~ rh + QuadCover,data = d5.sel)
# summary(lmd5.lam)
# plot(lambda~QuadHeight,data = d5.sel,pch = 16, xlim=c(0,16))
# plot(rh~QuadCover, data = d5.sel)
# plot(D~V5range, data = d5.sel,pch = 16)
# plot(AveHeight~QuadCover ,data = d5.sel,ylim = c(0,16))

lmd5.lam  <- lm(Ut ~  flambda,data = d5.sel)
summary(lmd5.lam)
# lmd5.lam  <- glm(D ~ QuadHeight + AsioV5range + flambda ,data = d5.sel,
#                  family = Gamma(link  = log))
# summary(lmd5.lam)
# plot(D~QuadHeight,data = d5.sel)



# lambda とcoverの関係、range/hを考慮して
lmd5.lam  <- lmres(lambda ~ QuadCover *rh,
                   centered = c("QuadCover","rh"),data = d5.sel)
summary(lmd5.lam)

model2 <- simpleSlope(lmd5.lam, pred="QuadCover", mod1 = "rh")
summary(model2)
PlotSlope(model2)

plot(lambda ~ QuadCover, data = d5.sel,col = rainbow(9)[round(d5.sel$rh/20,0)],
     cex = 2,pch = 16, cex.axis = 1.7,cex.lab = 2,
     ylab = expression(lambda),
     xlab = "cover (%)",xlim = c(0,45),ylim = c(0,0.15))
legend(40,0.14,title  ="range /h",seq(min(round(d5.sel$rh,0))-3,
                                 max(round(d5.sel$rh,0))-3,by = 20),
       col = rainbow(9)[1:max(round(d5.sel$rh/10,0))],pch=16,
       cex = 1.2)

# C とcoverの関係、range/hを考慮して
lmd5.lam  <- lmres(D ~ QuadCover *rh,
                   centered = c("QuadCover","rh"),data = d5.sel)
summary(lmd5.lam)

model2 <- simpleSlope(lmd5.lam, pred="QuadCover", mod1 = "rh")
summary(model2)
PlotSlope(model2)

par(mar = c(5,7,3,1),xpd=F,family = family_serif)
plot(D ~ QuadCover, data = d5.sel,col = rainbow(9)[round(d5.sel$rh/20,0)],
     cex = 2,pch = 16, cex.axis = 1.7,cex.lab = 2,
     ylab = expression(paste("coefficient c (" , m^{-1},")")),
     xlab = "cover (%)",xlim = c(0,45),ylim = c(0,14))
legend(40,12,title  ="range /h",seq(min(round(d5.sel$rh,0))-3,
                                      max(round(d5.sel$rh,0))-3,by = 20),
       col = rainbow(9)[1:max(round(d5.sel$rh/20,0))],pch=16,
       cex = 1.2)



plot(z0h~QuadCover,data = d5.sel)
plot(D~rh,data = d5.sel,log = "y")

cor.lmd5 <- cor(d5.sel[,c("QuadCover","AveHeight","V5range")])
vif.res <- 1/(1-cor.lmd5^2)
vif.res #手計算
vif(lmd5.lam) # パッケージ使用

options(na.action = "na.fail")
dredge(lmd5.lam,rank="AIC")

lmd5.lam  <- lm(D ~ + AveHeight   ,data = d5.sel)
summary(lmd5.lam)

# 異方性でないrange
d3 <- data.frame(lambda = temp.lambda, z0h =  temp.z0h,flambda = temp.flambda,
                 cover = d2$VegCover, height = d2$VegComHgt, range = d2$AsioV5range,
                 rangeh = 100*d2$V5range/d2$VegComHgt,ust = d2$Ut, coc = d2$D, sf = d2$meanSF)
# d3 <- d3[2:9,]
# d3$sf <- log(d3$sf)
pairs(d3)
lmd3.lam <- lm(coc ~  cover + height + range ,data = d3)
summary(lmd3.lam)

plot(z0h~height,data = d3,pch =16) 

library(performance)
r2(lmd3.lam)
lmd3.lam.r2 <- as.numeric(r2(lmd3.lam))

lmd3.lam.r2 <- summary(lmd3.lam)$adj.r.squared

# 多重共線性
cor.lmd3 <- cor(d3[,c("cover","height","range")])
vif.res <- 1/(1-cor.lmd3^2)
vif.res #手計算
vif(lmd3.lam) # パッケージ使用

# AIC
options(na.action = "na.fail")
dredge(lmd3.lam,rank="AIC")

lmd3.lam2 <- glm(coc ~lambda,data = d3 ,family = Gamma(link = log))
summary(lmd3.lam2)
plot(coc~lambda,data=d3)
plot(lambda~cover,data = d3)
plot(height~cover , data = d3)

pwr.f2.test(u=3,v=6,f2= lmd3.lam.r2 / (1-lmd3.lam.r2),sig.level = 0.05 ,power = NULL)



# 異方性range
d3 <- data.frame(lambda = temp.lambda, z0h =  temp.z0h,flambda = temp.flambda,
                 cover = d2$VegCover, height = d2$VegComHgt, range = d2$AsioV5range,
                 rangeh = 100*d2$AsioV5range/d2$VegComHgt,ust = d2$Ut, coc = d2$D, sf = d2$meanSF)
pairs(d3)
lmd3.lam <- lm(sf ~ cover + height  + range,data = d3 )
summary(lmd3.lam)

lmd3.lam2 <- lm(sf ~ cover + range ,data = d3 )
summary(lmd3.lam2)


# イベントごとの違い
boxplot(aveWD ~ Event, data = d2)
boxplot(aveWS ~ Event, data = d2)
length(d2$Event == c(1,2,3,4,6))
tapply(d2$SiteID,d2$Event,length)
boxplot(meanSF ~ Event, data = d2)
boxplot(D ~ Event, data = d2)
plot(D~aveWS, data = d2)
plot(meanSF~aveWS, data = d2)

# 粗度要素と植生要素　再解析2 ----------------------------------------------------------
# d4 <- read.csv("Veg_EroAnalysis14_2.csv", header =T)
# 
# # 型変換
# d4$SiteID <- as.factor(d4$SiteID)
# 
# ### D to c
# # c = D* \rho / g
# # \rho = 1293g m^-3 = , g = 9.8 m s^-2
# d4$c <- d4$c * 1293 / 9.8
# 
# # グループ付与
# d4$clus.group <- as.factor(clus.group)
# 
# d4.2 <- data.frame(lambda = temp.lambda, z0h =  temp.z0h,flambda = temp.flambda,
#                  cover = d4$SHP_Cover, height = d4$QuadCommunityHeight, range = d4$V500range,
#                  rangeh = 100*d4$V500range/d4$QuadCommunityHeight)
# pairs(d4.2)
# lmd4.lam <- lm(lambda ~ cover + height  + rangeh,data = d4.2 )
# summary(lmd4.lam)
# 
# lmd4.lam2 <- lm(lambda ~ cover + rangeh,data = d4.2 )
# summary(lmd4.lam2)


# soil condition ----------------------------------------------------------

d.soil <- cbind(d.soil,clus.group )
d.soil$clus.group <- as.factor(d.soil$clus.group)

# 土壌正式作図用
# par("mar")
layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))
par(mar = c(5,6,3,2))

boxplot(GSGravelSize ~ clus.group,data= d.soil,xlab = "",ylab = "gravel size",
        ylim = c(0, 4),yaxt  = "n",
        cex.axis=1.7,cex.lab =2)
axis(side=2,          #side2:左
     at=seq(0,4), #0から8まで1ずつ
     tck=0.03,            #長さ0.03のティック
     labels=c(
       "no gravel
(~5mm)",
       "small
(~1cm)",
       "middle
(~2cm)",
       "large
(~4cm)",
       "very large
(4cm~)"),
     mgp=c(1,0.5,0),
     cex.axis = 1.5
)

boxplot(GAIravelAmount ~ clus.group,data= d.soil,xlab = "",ylab = "an amount of gravel",
        ylim = c(0, 4),yaxt  = "n",
        cex.axis=1.7,cex.lab =2)
axis(side=2,          #side2:左
     at=seq(0,4), #0から8まで1ずつ
     tck=0.03,            #長さ0.03のティック
     labels=c("no gravel","little","middle","large","very large"),
     mgp=c(1,0.5,0),
     cex.axis = 1.5
)

plot(NULL)

boxplot(Clust  ~ clus.group,data= d.soil,xlab = "",ylab = "presence of crust",
        ylim = c(0,2),yaxt  = "n",
        cex.axis=1.7,cex.lab =2)
axis(side=2,          #side2:左
     at=seq(0,2), #0から8まで1ずつ
     tck=0.03,            #長さ0.03のティック
     labels=c("no crust","weak crust","hard crust"),
     mgp=c(1,0.5,0),
     cex.axis = 1.5
)

boxplot(Crack   ~ clus.group,data= d.soil,xlab = "cluster group",ylab = "crack of crust",
        ylim = c(0,3),yaxt  = "n",
        cex.axis=1.7,cex.lab =2)
axis(side=2,          #side2:左
     at=seq(0,3), #0から8まで1ずつ
     tck=0.03,            #長さ0.03のティック
     labels=c("no crack","small","middle","huge"),
     mgp=c(1,0.5,0),
     cex.axis = 1.5
)

boxplot(HdHardness  ~ clus.group,data= d.soil,xlab = "",ylab = "hardness of soil surface",
        ylim = c(0,4),yaxt  = "n",
        cex.axis=1.7,cex.lab =2)
axis(side=2,          #side2:左
     at=seq(0,4), #0から8まで1ずつ
     tck=0.03,            #長さ0.03のティック
     labels=c("very soft","soft","middle","hard","very hard"),
     mgp=c(1,0.5,0),
     cex.axis = 1.5
)
dev.copy(cairo_pdf, file=paste(path4,"/group_soilSA.pdf",sep=""), width = 15, height = 10)
dev.off()


# 土壌用比較
ana.fact <- c("GSGravelSize","GAIravelAmount","Clust","Crack","HdHardness","clus.group")
d3 <- d.soil[,ana.fact]
d3 <- d3[order(d3$clus.group),]
d3$GAIravelAmount <- as.numeric(d3$GAIravelAmount )

### Benjamini-Hochberg method
# 可変値
hdr.val <- 0.1 #FDRの基準値 q^*値
# 比較群対行列作成
group.lab <- 1:gnum_st
group.comb <- c(0,0)
for(i in 1:gnum_st){
  for(j in 1:gnum_st){
    if(group.lab[i] < group.lab[j]){
      group.comb <- rbind(group.comb, c(group.lab[i] ,group.lab[j]))
    }
  }
}
group.comb <-group.comb[2:nrow(group.comb),]

# 結果格納用データフレーム
p.result.BF <- data.frame(matrix(rep(0, 11), nrow=1))[numeric(0), ]
p.result.BF.name <- c("factor","groups","df","t","pvalue",
                      "95 percent confidence","interval","estimate x","estimate y","qvalues","sig")
colnames(p.result.BF) <- p.result.BF.name
# 結果を言葉で見やすくするようの文字列ベクトル
sig.val <- c("-","*")
group.comb.name <- paste(group.comb[,1],group.comb[,2],sep="-")

# 全要素について計算
for(i_unit in 1:(length(ana.fact)-1)){
  # Welchのt検定を各群に
  for(i_group in 1:nrow(group.comb)){
    temp.group1 <- d3[d3$clus.group == group.comb[i_group,1],ana.fact[i_unit]]
    temp.group2 <- d3[d3$clus.group == group.comb[i_group,2],ana.fact[i_unit]]
    if(length(temp.group1) == length(temp.group2)){
      if(sum(temp.group1 == temp.group2) == length(temp.group1)){
        p.result.BF <- rbind(p.result.BF,c(i_unit,
                                           i_group,0,0,1,
                                           rep(0,length=4),
                                           0,1))
        colnames(p.result.BF) <- p.result.BF.name
      }else{
        temp.t.result <- t.test(temp.group1,temp.group2,var.equal = F)
        p.result.BF <- rbind(p.result.BF,c(i_unit,
                                           i_group,
                                           temp.t.result$parameter,
                                           temp.t.result$statistic,temp.t.result$p.value,
                                           temp.t.result$conf.int[1],temp.t.result$conf.int[2],
                                           temp.t.result$estimate[1],temp.t.result$estimate[2],0,1))
        colnames(p.result.BF) <- p.result.BF.name
      }
    }else{
      temp.t.result <- t.test(temp.group1,temp.group2,var.equal = F)
      p.result.BF <- rbind(p.result.BF,c(i_unit,
                                         i_group,
                                         temp.t.result$parameter,
                                         temp.t.result$statistic,temp.t.result$p.value,
                                         temp.t.result$conf.int[1],temp.t.result$conf.int[2],
                                         temp.t.result$estimate[1],temp.t.result$estimate[2],0,1))
      colnames(p.result.BF) <- p.result.BF.name
    }
    
  }
  
  # 今回計算しているユニットの行位置を取得
  crow  <- (nrow(p.result.BF)-2):nrow(p.result.BF)
  
  # BH法によるq値の算出と有意判定
  p.values <- p.result.BF$pvalue[crow]
  sig.ind <- rep(1,length = length(p.values))
  p.order <- order(p.values,decreasing = T) # 元の順番を保持
  p.values <- p.values[p.order] # ソート
  q.values <- length(p.values)*p.values / length(p.values):1L # q値への変換
  q.issig <- (pmin(1, cummin(q.values)) <= hdr.val)[order(p.order)]
  sig.ind[q.issig] <- 2
  q.values <- pmin(1,q.values[order(p.order)])
  
  # 結果まとめ
  p.result.BF$qvalues[crow]  <- q.values
  p.result.BF$sig[crow]  <- sig.ind
}

# 整理
p.result.BF$factor <- ana.fact[p.result.BF$factor]
p.result.BF$groups <- group.comb.name[p.result.BF$groups]
p.result.BF$sig <- sig.val[p.result.BF$sig]

# 結果
p.result.BF


# グループ間比較検定 ---------------------------------------------------------------
library(NSM3)
# 解析する要素
ana.fact <- c("Ut","D","Z0","AsioV5range","VegCover","VegComHgt","flambda","clus.group")
d3 <- d2[,ana.fact]
# d3 <- cbind(d3[,1:(ncol(d3)-1)],test.z,d3$clus.group)
# ana.fact <- c("Ut","D","Z0","AsioV5range","VegCover","VegComHgt","x/h","clus.group")
# colnames(d3) <-ana.fact
d3 <- d3[order(d3$clus.group),]

### Steel-Dwass test
result.SD <- pSDCFlig(d3$VegCover, d3$clus.group, method="Exact")
result.SD
# summary(result.SD)

### Tukey-Kramer test
# TukeyHSD (R default)
result.TK <- list()
for(i_unit in 1:(length(ana.fact)-1)){
  aov.text <- paste("TK.aov <- aov(",ana.fact[i_unit],"~ clus.group,data = d3)",sep="")
  eval(parse(text = aov.text))
  
  result.TK[ana.fact[i_unit]] <- TukeyHSD(TK.aov)
  
}
# result.TK
# plot(result.TK)

# package multcomp
library(multcomp)


result.TK2<-glht(TK.aov, linfct = mcp(clus.group = "Tukey")) #
summary(result.TK2)
plot(result.TK2)
par(mai=c(1,1,1.5,1))
plot(cld(result.TK2) )

### Benjamini-Hochberg method
# 可変値
hdr.val <- 0.1 #FDRの基準値 q^*値
# 比較群対行列作成
group.lab <- 1:gnum_st
group.comb <- c(0,0)
for(i in 1:gnum_st){
for(j in 1:gnum_st){
  if(group.lab[i] < group.lab[j]){
    group.comb <- rbind(group.comb, c(group.lab[i] ,group.lab[j]))
  }
}
}
group.comb <-group.comb[2:nrow(group.comb),]

# 結果格納用データフレーム
p.result.BF <- data.frame(matrix(rep(0, 11), nrow=1))[numeric(0), ]
p.result.BF.name <- c("factor","groups","df","t","pvalue",
                      "95 percent confidence","interval","estimate x","estimate y","qvalues","sig")
colnames(p.result.BF) <- p.result.BF.name
# 結果を言葉で見やすくするようの文字列ベクトル
sig.val <- c("-","*")
group.comb.name <- paste(group.comb[,1],group.comb[,2],sep="-")

# 全要素について計算
for(i_unit in 1:(length(ana.fact)-1)){
# Welchのt検定を各群に
for(i_group in 1:nrow(group.comb)){
  temp.group1 <- d3[d3$clus.group == group.comb[i_group,1],ana.fact[i_unit]]
  temp.group2 <- d3[d3$clus.group == group.comb[i_group,2],ana.fact[i_unit]]
  temp.t.result <- t.test(temp.group1,temp.group2,var.equal = F)
  p.result.BF <- rbind(p.result.BF,c(i_unit,
                                     i_group,
                                     temp.t.result$parameter,
                                     temp.t.result$statistic,temp.t.result$p.value,
                                     temp.t.result$conf.int[1],temp.t.result$conf.int[2],
                                     temp.t.result$estimate[1],temp.t.result$estimate[2],0,1))
  colnames(p.result.BF) <- p.result.BF.name
}

# 今回計算しているユニットの行位置を取得
crow  <- (nrow(p.result.BF)-2):nrow(p.result.BF)

# BH法によるq値の算出と有意判定
p.values <- p.result.BF$pvalue[crow]
sig.ind <- rep(1,length = length(p.values))
p.order <- order(p.values,decreasing = T) # 元の順番を保持
p.values <- p.values[p.order] # ソート
q.values <- length(p.values)*p.values / length(p.values):1L # q値への変換
q.issig <- (pmin(1, cummin(q.values)) <= hdr.val)[order(p.order)]
sig.ind[q.issig] <- 2
q.values <- pmin(1,q.values[order(p.order)])

# 結果まとめ
p.result.BF$qvalues[crow]  <- q.values
p.result.BF$sig[crow]  <- sig.ind
}

# 整理
p.result.BF$factor <- ana.fact[p.result.BF$factor]
p.result.BF$groups <- group.comb.name[p.result.BF$groups]
p.result.BF$sig <- sig.val[p.result.BF$sig]

# 結果
p.result.BF

# 検定力検証
# library(pwr)



# mean and S.E. -----------------------------------------------------------
p.result.meanSE <- data.frame(matrix(rep(0, 4), nrow=1))[numeric(0), ]
p.result.meanSEname <- c("factor","groups","mean","SE")
colnames(p.result.meanSE) <- p.result.meanSEname
for(i_unit in 1:(length(ana.fact)-1)){
  temp.d3 <- d3[,ana.fact[c(i_unit,length(ana.fact))]]
  temp.d3.mean <- tapply(temp.d3[,1],temp.d3[,2],mean)
  temp.d3.se <- tapply(temp.d3[,1],temp.d3[,2],sd)/sqrt(tapply(temp.d3[,1],temp.d3[,2],length))
  
  p.result.meanSE <- data.frame("factor" = c(p.result.meanSE$factor,rep(ana.fact[i_unit],length=nrow(group.comb))),
                                "groups" = c(p.result.meanSE$groups,1:gnum_st),
                                "mean" = c(p.result.meanSE$mean,temp.d3.mean),
                                "SE" = c(p.result.meanSE$SE,temp.d3.se))
}
p.result.meanSE

# 投稿用作図 -------------------------------------------------------------------
# クラスター
par(mar = c(5,6,3,2),family = family_serif)
plot(clus,cex = 1.7,cex.axis=1.5,cex.lab = 1.7, main = "",
     ylab = "Ward's cluster distance",xlab = "SiteID _ EventNo.",
     sub = "") #グラフ表示
## クラスターグループ表記
xlab <- c(1.8,4.8,7.9)
rect(xlab-0.4,13.5,xlab+0.4,16,col = "White",lty = 0)
text(xlab,15,paste("Group",c(3,1,2),sep=""),adj = 0.5)
# text(1.8,15,"Group1",adj  = 0.5)
# text(4.9,15,"Group2",adj  = 0.5)
# text(8.5,15,"Group3",adj  = 0.5)
# text(10.9,15,"Group4",adj  = 0.5)
d2[,c("SiteID","clus.group")]

dev.copy(cairo_pdf, file=paste(path4,"/clusterSA.pdf",sep=""), width = 10, height = 7)
dev.off()

# 回帰
layout(matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE))
# パワポ用（縦並び）
# layout(matrix(c(1,2), nrow = 2, ncol = 1, byrow = TRUE))
par(mar = c(5,6,3,2),family = family_serif)

# ust

# plot(Ut ~ Z0, data = d2, xlim = c(10^(-4),max(d2$Z0)*1.1),
#      ylim = c(0, max(d2$Ut)*1.1),
#      cex = 2,pch = 16, cex.lab = 2, cex.axis = 1.7,
#      ylab = expression(paste(u["*"*t]," (m  ",s^{-1} ,")")),
#      xlab = expression(paste(z[0] ," (m)")),
#      log = "x",xaxt = "n")
# sLab <- c(expression(10^{-4}),expression(10^{-3}),
#           expression(10^{-2}),expression(10^{-1}))
# axis(side=1,          #side2:左
#      at=10^(-4:-1), #0から8まで1ずつ
#      tck=0.03,            #長さ0.03のティック
#      labels=sLab,
#      mgp=c(1,1,0),
#      cex.axis = 1.7
# )
# fPow <- function(x){(2:9)*10^x}
# axis(side=1,        #side2:左
#      at=sapply(-4:-1, fPow), #繰り返し(2:9)×10^(iLogL:iLogU)
#      tck=0.01,          #長さ0.01のティック
#      labels=FALSE,      #ラベル出力なし
#      mgp=c(1,0.5,0)
# )
# x <- seq(min(d2$Z0),max(d2$Z0),by = 0.01)
# eta.pred <- result.lmUst$coefficients[1] + result.lmUst$coefficients[2]*log(x)
# lines(x,eta.pred)
# title("(a)",adj = 0, cex.main = 2)

# ust-fλ
plot(d2$Ut ~ temp.flambda, xlim = c(0,4),
     ylim = c(0, max(d2$Ut)*1.1),
     cex = 2,pch = 16, cex.lab = 2, cex.axis = 1.7,
     ylab = expression(paste(u["*"*t]," (m  ",s^{-1} ,")")),
     xlab = expression(italic(f)[lambda]))

x <- seq(1,4,by = 0.01)
eta.pred <- result.lmUstlam$coefficients[1] + result.lmUstlam$coefficients[2] * x
lines(x,eta.pred)
title("(a)",adj = 0, cex.main = 2)

# dev.copy(cairo_pdf, file=paste(path4,"/regression_Ust.pdf",sep=""), width = 10, height = 10)
# dev.off()
# D
# par(mar = c(5,6,3,2),family = family_serif)
plot(D ~ VegComHgt, data = d2, xlim = c(0,16),cex = 2,pch = 16,
     cex.lab = 2, cex.axis = 1.7,
     ylab = expression(paste("coefficient c (" , m^{-1},")")),
     xlab = "community height (cm)")
x <- seq(2,16,by = 0.1)
eta.pred <- result.lmD$coefficients[1] + result.lmD$coefficients[2]*x
lines(x,exp(eta.pred))
title("(b)",adj = 0, cex.main = 2)

# dev.copy(cairo_pdf, file=paste(path4,"/regression_c.pdf",sep=""), width = 10, height = 10)
# dev.off()


dev.copy(cairo_pdf, file=paste(path4,"/regression_Ust_cSA2.pdf",sep=""), width = 20, height = 10)
dev.off()
# パワポ用
# dev.copy(cairo_pdf, file=paste(path4,"/regression_Ust_cSA2.pdf",sep=""), width = 6, height = 12)
# dev.off() 


# グループ間比較

layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))
# パワポ用
# layout(matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2, byrow = TRUE))
par(mar = c(5,7,3,1),xpd=F,family = family_serif)

plot(d2$clus.group,d2$Ut,xlab = "",
     ylab = expression(paste(u["*"*t]," (m  ",s^{-1} ,")")),
     ylim = c(0, max(d2$Ut)*1.1),
     cex.axis=2,cex.lab =2.5)
title( main="(a)",adj = 0,cex.main = 2)
points(1:3,p.result.meanSE$mean[p.result.meanSE$factor == "Ut"],pch = 3)

# beeswarm(d2$Ut~d2$clus.group, data = NULL, pch = 16, col = rainbow(8), main = 'angle', add = TRUE)
plot(d2$clus.group,d2$D,xlab = "",
     ylab = expression(paste("coefficient c (" , m^{-1},")")),
     ylim = c(0, max(d2$D)*1.1),
     cex.axis=2,cex.lab =2.5)
points(1:3,p.result.meanSE$mean[p.result.meanSE$factor == "D"],pch = 3)
title( main="(b)",adj = 0,cex.main = 2)
### 係数cの対数表記バージョン
# plot(d2$clus.group,d2$D,xlab = "",
#      ylab = expression(paste("c (" , m^{-1},")")),
#      ylim = c(0.05, max(d2$D)*1.1),
#      cex.axis=2,cex.lab =2.5,log = "y"
#      ,yaxt = "n")
# sLab <- c(expression(10^{-1}),expression(10^{0}),
#           expression(10^{1}),expression(10^{2}))
# 
# axis(side=2,          #side2:左
#      at=10^(-1:2), #0から8まで1ずつ
#      tck=0.03,            #長さ0.03のティック
#      labels=sLab,
#      mgp=c(1,0.5,0),
#      cex.axis = 2
# )
# fPow <- function(x){(2:9)*10^x}
# axis(side=2,        #side2:左
#      at=sapply(-2:2, fPow), #繰り返し(2:9)×10^(iLogL:iLogU)
#      tck=0.01,          #長さ0.01のティック
#      labels=FALSE,      #ラベル出力なし
#      mgp=c(1,0.5,0)
# )
# title( main="(b)",adj = 0,cex.main = 2)
# beeswarm(d2$D~d2$clus.group, data = NULL, pch = 16, col = rainbow(8), main = 'angle', add = TRUE)

### z0の描画
# plot(d2$clus.group,d2$Z0,log = "y",xlab = "",
#      ylab = expression(paste(z[0] ," (m)")),
#      ylim = c(10^(-4),0.15),
#      cex.axis=2,cex.lab =2.5
#      ,yaxt = "n")
# points(1:3,p.result.meanSE$mean[p.result.meanSE$factor == "Z0"],pch = 3)
# 
# sLab <- c(expression(10^{-4}),expression(10^{-3}),
#           expression(10^{-2}),expression(10^{-1}))
# 
# axis(side=2,          #side2:左
#      at=10^(-4:-1), #0から8まで1ずつ
#      tck=0.03,            #長さ0.03のティック
#      labels=sLab,
#      mgp=c(1,0.5,0),
#      cex.axis = 2
# )
# fPow <- function(x){(2:9)*10^x}
# axis(side=2,        #side2:左
#      at=sapply(-5:-1, fPow), #繰り返し(2:9)×10^(iLogL:iLogU)
#      tck=0.01,          #長さ0.01のティック
#      labels=FALSE,      #ラベル出力なし
#      mgp=c(1,0.5,0)
# )
# title( main="(c)",adj = 0,cex.main = 2)

# fλの描画
plot(d2$clus.group,d2$flambda,xlab = "",
     ylab = expression(italic(f)[lambda]),
     ylim = c(0, 4),
     cex.axis=2,cex.lab =2.5)
points(1:3,p.result.meanSE$mean[p.result.meanSE$factor == "flambda"],pch = 3)
title( main="(c)",adj = 0,cex.main = 2)

plot(d2$clus.group,d2$AsioV5range,xlab = "",ylab = "range (m)",
     ylim = c(0,max(d2$AsioV5range)*1.2),
     cex.axis=2,cex.lab =2.5)
points(1:3,p.result.meanSE$mean[p.result.meanSE$factor == "AsioV5range"],pch = 3)
title( main="(d)",adj = 0,cex.main = 2)

# 有意差表記
# 1-2
segments(1, 8.5, 2, 8.5)
segments(1, 8.5, 1, 8.3)
segments(2, 8.5, 2, 8.3)
text(1.5, 8.8, "†",cex = 1.7)
# 2-3
segments(2, 9, 3, 9)
segments(3, 9, 3, 8.8)
segments(2, 9, 2, 8.8)
text(2.5, 9.3, "†",cex = 1.7)



# beeswarm(d2$AsioV5range~d2$clus.group, data = NULL, pch = 16, col = rainbow(8), main = 'angle', add = TRUE)

plot(d2$clus.group,d2$VegCover,xlab = "group",ylab = "cover (%)",
     ylim = c(0,max(d2$VegCover)*1.2),
     cex.axis=2,cex.lab =2.5)
points(1:3,p.result.meanSE$mean[p.result.meanSE$factor == "VegCover"],pch = 3)
title( main="(e)",adj = 0,cex.main = 2)
# 有意差表記
# 1-2
segments(1, 45, 2, 45)
segments(1, 45, 1, 44)
segments(2, 45, 2, 44)
text(1.5,46, "***",cex = 2)
# 2-3
segments(2, 49, 3, 49)
segments(3, 49, 3, 48)
segments(2, 49, 2, 48)
text(2.5,51, "***",cex = 2)
# 1-3
segments(1, 53, 3, 53)
segments(3, 53, 3, 52)
segments(1, 53, 1, 52)
text(2,54, "***",cex = 2)
# beeswarm(d2$VegCover~d2$clus.group, data = NULL, pch = 16, col = rainbow(8), main = 'angle', add = TRUE)

plot(d2$clus.group,d2$VegComHgt,xlab = "",ylab = "community height (cm)",
     ylim = c(0,max(d2$VegComHgt)*1.1),
     cex.axis=2,cex.lab =2.5, boxwex=0.8)
title( main="(f)",adj = 0,cex.main = 2)
points(1:3,p.result.meanSE$mean[p.result.meanSE$factor == "VegComHgt"],pch = 3)
# beeswarm(d2$VegComHgt~d2$clus.group, data = NULL, pch = 16, col = rainbow(8), main = 'angle', add = TRUE)

dev.copy(cairo_pdf, file=paste(path4,"/group_vegSA_sig2.pdf",sep=""), width = 15, height = 10)
dev.off()
# パワポ用
# dev.copy(cairo_pdf, file=paste(path4,"/group_vegSA_sig2.pdf",sep=""), width = 8, height = 12)
# dev.off()
