# library import ----------------------------------------------------------
library(MuMIn) # AICのためのdredge関数
library(beeswarm)
library(car) # 多重共線性 virの算出
library(pwr) # 検出力計算　重回帰はpwr.f2.test
library(pequod) #重回帰用
library(RColorBrewer) #グラフ色用
library(lme4) # GLMM用
# Init setting ------------------------------------------------------------
# work directory
# path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1301MongoliaAnalysis8/NsiteRecul" #N,Bsite
setwd(path3)
# figure folder
# path4 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1202PhDthesis_submission/Fig"
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1301MongoliaAnalysis8/Fig" #N,Bsite

# d.all <- read.csv("Veg_EroAnalysis15.csv", header =T)
d.all <- read.csv("VegEro_Analysis_NB.csv", header =T)
# 型変換
d.all$SiteID <- as.factor(d.all$SiteID)
d.all$Event <- as.factor(d.all$Event)
d.all$SiteEv <- as.factor(d.all$SiteEv)

### D to c
# c = D* \rho / g
# \rho = 1293g m^-3 = , g = 9.8 m s^-2
# d.all$c <- d.all$D * 1293 / 9.8
d.all$c <- d.all$D * 9.8 / 1293

### cul range / height
d.all$rh <- d.all$AsioV5range *2 / (d.all$QuadHeight/100)

### サイト除外
d.all <- d.all[d.all$SiteID != "N3-4",]

d.all$Type[d.all$Type=="B"] <- 0
d.all$Type[d.all$Type=="W"] <- 1

# utが算出できてるサイトを抽出
d <- d.all[!is.na(d.all$Ut),]



# fλ算出 ------------------------------------------------------------------
d2 <- d
## 1．community heightをhとして、z0/hを計算。
temp.hm <- d2$QuadHeight / 100
temp.z0h <- d2$Z0/temp.hm

## 2. 私のスライド16にForoutan et al. (2017)を載せていますが、
# z0/h=0.96λ^1.07の関係式から 
# λ ：roughness density (Shao 2008のfrontal area index)を計算。
# log(λ) = {log(z0h / 0.96)}/1.07
temp.lambda2 <- {log(temp.z0h/0.96)}/1.07
temp.lambda <- exp(temp.lambda2)

## 3. Shao (2008) eq. (9.22)の式でf(λ)が得られます。
# Shao (2008)の値が異なりますが、WRF-Chemでは、m=0.5, σ=1.0, β=200が使われる。
# m=0.5, σ=1.0, β=200
cul.flambda <- function(lambda,mr, sigr, betar){
  y <- sqrt(1 - mr * sigr * lambda) * 
    sqrt(1 + mr * betar * lambda)
  print(y)
}

par.mr <- 0.5
par.sigr <- 1.0
par.betar <- 200
temp.flambda <- cul.flambda(temp.lambda, par.mr, par.sigr, par.betar)

d3 <- d2
d3$lambda <-temp.lambda
d3$z0h <- temp.z0h
d3$flambda <- temp.flambda


# 回帰 ----------------------------------------------------------------------
### ust ~ fλ
# lmresult.ut  <- lm(Ut ~  flambda,data = d3)
# summary(lmresult.ut)
flambda.cent <- d3$flambda - 1
lmresult.ut  <- lm(d3$Ut ~  flambda.cent)
summary(lmresult.ut)

# GLMM
sitedir <- c(rep(0,length = 6),rep(1,length = 4))
ust.d.glmm <- data.frame(Ut = d3$Ut, flambda = flambda.cent, sitedir = sitedir)
ust.d.glmm$sitedir <- as.factor(ust.d.glmm$sitedir)
lmresult.ut.glmm <- lmer(Ut ~  flambda + (1 | sitedir),data = ust.d.glmm)

summary(lmresult.ut.glmm)
ranef
aa <- fixef(lmresult.ut.glmm)[1]
Naa <- fixef(lmresult.ut.glmm)[1] + ranef(lmresult.ut.glmm)$sitedir[1,1]
Saa <- fixef(lmresult.ut.glmm)[1] + ranef(lmresult.ut.glmm)$sitedir[2,1]
bb <- fixef(lmresult.ut.glmm)[2]
ust.d.glmm$flambda <- ust.d.glmm$flambda+1

plot(Ut~(flambda+1), data = ust.d.glmm,col = as.numeric(sitedir)+1,pch = 21,cex = 1.5,xlim = c(0,4),ylim = c(0,0.7))
abline((Naa-bb),bb,col = 2)
text(0,0.7,paste(round(Naa,2)," + ",round(bb,2),"(λ - 1)",sep=""),adj = 0,col=2)
abline((Saa-bb),bb,col = 3)
text(0,0.65,paste(round(Saa,2)," + ",round(bb,2),"(λ - 1)",sep=""),adj = 0,col=3)
abline((aa-bb),bb,col = 1)
text(0,0.675,paste(round(aa,2)," + ",round(bb,2),"(λ - 1)",sep=""),adj = 0,col=1)

#### cover とrange/hを説明変数とした重回帰
# 中心化（切片を説明変数=0のときだったのを＝平均の意味に変える　
#         ＆　交互効果を見るときに多重共線性を防ぐ）
rh.cent <- d3$rh - mean(d3$rh)
cv.cent <- d3$QuadCover - mean(d3$QuadCover)
# 標準化（係数の大小で、影響の大小を見る）
# rh.stan <- rh.cent / sd(d3$rh)
# cv.stan <- cv.cent / sd(d3$QuadCover)

### 係数c とcoverの関係、range/hを考慮して
lmresult.c  <- lm(d3$c ~ cv.cent *rh.cent)
summary(lmresult.c)
# AICによるモデル選択
options(na.action = "na.fail")
dredge(lmresult.c,rank="AIC")
# 多重共線性のチェック
vif(lmresult.c) # パッケージ使用
# 標準偏回帰係数の算出
# c.st <- (d3$c - mean(d3$c)) / sd(d3$c)
# lmresult.c.st  <- lm(c.st ~ cv.stan *rh.stan)
# summary(lmresult.c.st)

plot()

### lambda とcoverの関係、range/hを考慮して
lmresult.lam  <- lm(d3$lambda ~ cv.cent *rh.cent)
summary(lmresult.lam)
# AICによるモデル選択
dredge(lmresult.lam,rank="AIC")
# 多重共線性のチェック
vif(lmresult.lam) # パッケージ使用
# 標準偏回帰係数の算出
# lambda.st <- (d3$lambda - mean(d3$lambda)) / sd(d3$lambda)
# lmresult.c.st  <- lm(lambda.st ~ cv.stan *rh.stan)
# summary(lmresult.c.st)


# 散布図グラフ描画 -------------------------------------------------------------------

### 回帰
layout(matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE))
# パワポ用（縦並び）
# layout(matrix(c(1,2), nrow = 2, ncol = 1, byrow = TRUE))

par(mar = c(4.5,4.5,2,1),family = family_serif)

## ust-fλ
plot(Ut ~　flambda,data = d3, xlim = c(0,8),
     ylim = c(0, 3),
     cex = 1,pch = 21,col = 1,bg = 1, cex.lab = 1.2, cex.axis = 1,
     ylab = expression(paste(italic(u)["*"*t]," (m  ",s^{-1} ,")")),
     xlab = expression(italic(f)[lambda](lambda)))
# 回帰直線
x <- seq(1,4,by = 0.01)
eta.pred <- lmresult.ut$coefficients[1] + lmresult.ut$coefficients[2] * (x-1)
lines(x,eta.pred)
title("(a)",adj = 0, cex.main = 1.2)

## c ~ cover * 2r/h
rhlegendlab <- c("< 30","< 50","< 70","< 90",
                 "< 110","< 130","< 150","< 170")

# rhlegendlab <- c("< 37","< 62","< 87",
#                  "< 112","< 137","< 162")
plot(c ~ QuadCover, data = d3[d3$Type=="0",],col="Black",
     bg = brewer.pal(10,"Spectral")[9 - round(d3$rh/20,0)],
     cex = 1,pch = 21 + as.numeric(d3$Type), cex.axis = 1,cex.lab = 1.2,
     ylab = expression(paste("coefficient ", italic(c)," (" , m^{-1},")")),
     xlab = "cover (%)",xlim = c(0,20),ylim = c(0,0.2))
legend("topright",title  =expression(italic(bar(D)/h)),rhlegendlab,
       pt.bg = brewer.pal(10,"Spectral")[max(round(d3$rh/20,0)):1],pch=21,
       cex = 1,col=1,)

title("(b)",adj = 0, cex.main = 1.2)



## lambda ~ cover * 2r/h
layout(matrix(c(1,1), nrow = 1, ncol = 1, byrow = TRUE))
par(mar = c(4.3,4.3,2,1),family = family_serif)
plot(lambda ~ QuadCover, data = d3, bg = brewer.pal(10,"Spectral")[9 - round(d3$rh/20,0)],
     cex = 1,pch = 21 + sitedir, col = 1, cex.axis = 1,cex.lab = 1.2,
     ylab = expression(italic(lambda)),
     xlab = "cover (%)",xlim = c(0,45),ylim = c(0,0.15))
legend("topright",title  =expression(italic(bar(D)/h)),rhlegendlab,
       pt.bg = brewer.pal(10,"Spectral")[max(round(d3$rh/20,0)):1],pch=21,
       cex = 1,col=1,ncol =2)



# 全サイトについての解析 -------------------------------------------------------------
# クラスターによるグループ分け ----------------------------------------------------------

## 植被率、群落高、rangeのみのデータフレームを作る
# 全サイト-イベントデータ使用
# veg.cond <- cbind(d.all$QuadCover,d.all$QuadHeight,d.all$AsioV5range)
# colnames(veg.cond) <- c("QuadCover","QuadHeight","AsioV5range")

# 全サイトデータ使用
veg.cond <- d.all[,c("SiteID","QuadCover","QuadHeight")]
row.names(veg.cond) <- d.all$SiteEv
veg.cond <- veg.cond[!duplicated(veg.cond$SiteID), ]

## クラスター分類
gnum_st <- 3
clus<-hclust(dist(veg.cond[,2:3]),"ward.D2") #ウォード法改で

# クラスター結果プロット
layout(matrix(c(1,1), nrow = 1, ncol = 1, byrow = TRUE))
plot(clus,cex = 1.7,cex.axis=1.5,cex.lab = 1.7, main = "",
     ylab = "Ward's cluster distance",xlab = "SiteID _ EventNo.",
     sub = "") #グラフ表示
clus.g <- rect.hclust(clus,gnum_st)

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

# 植被率の小さい順にグループ名を並べ替え（行順は変更なし）
clus.group[clus.group == 1] <- 4
clus.group <-5- clus.group

# 全情報の入ったデータフレームにグループ情報を付加
veg.cond <- cbind(veg.cond, clus.group)
d.all2 <- merge(d.all,veg.cond[,c("SiteID","clus.group")],by.x ="SiteID", by.y = "SiteID" )
# d.all2 <- cbind(d.all,clus.group )
# d.all2$clus.group <- as.factor(d.all2$clus.group)


# 比較検定 ---------------------------------------------------------------
# library(NSM3)
# 解析する要素
ana.fact <- c("meanSF","AsioV5range","QuadCover","QuadHeight","QuadRichness","rh","clus.group")
d.all3 <- d.all2[,ana.fact]
d.all3 <- d.all3[order(d.all3$clus.group),]


### サイトごと
ana.fact <- c("QuadCover","QuadHeight","clus.group")
d.all3 <- veg.cond
d.all3 <- d.all3[order(d.all3$clus.group),]

### Benjamini-Hochberg method
# 可変値
hdr.val <- 0.05 #FDRの基準値 q^*値
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
    temp.group1 <- d.all3[d.all3$clus.group == group.comb[i_group,1],ana.fact[i_unit]]
    temp.group2 <- d.all3[d.all3$clus.group == group.comb[i_group,2],ana.fact[i_unit]]
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

# 平均値、標準誤差算出 --------------------------------------------------------------

p.result.meanSE <- data.frame(matrix(rep(0, 4), nrow=1))[numeric(0), ]
p.result.meanSEname <- c("factor","groups","mean","SE")
colnames(p.result.meanSE) <- p.result.meanSEname
for(i_unit in 1:(length(ana.fact)-1)){
  temp.d3 <- d.all3[,ana.fact[c(i_unit,length(ana.fact))]]
  temp.d3.mean <- tapply(temp.d3[,1],temp.d3[,2],mean)
  temp.d3.se <- tapply(temp.d3[,1],temp.d3[,2],sd)/sqrt(tapply(temp.d3[,1],temp.d3[,2],length))
  
  p.result.meanSE <- data.frame("factor" = c(p.result.meanSE$factor,rep(ana.fact[i_unit],length=nrow(group.comb))),
                                "groups" = c(p.result.meanSE$groups,1:gnum_st),
                                "mean" = c(p.result.meanSE$mean,temp.d3.mean),
                                "SE" = c(p.result.meanSE$SE,temp.d3.se))
}
# p.result.meanSE

# 比較グラフ描画 ------------------------------------------------------------

# layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))
# boxplot(meanSF~clus.group, data = d.all2,log="y")
# boxplot(meanWS~clus.group, data = d.all2)
# boxplot(rh~clus.group, data = d.all2)
# boxplot(AsioV5range~clus.group, data = d.all2)
# boxplot(QuadCover~clus.group, data = d.all2)
# boxplot(QuadHeight~clus.group, data = d.all2)
# boxplot(QuadRichness~clus.group, data = d.all2)
# tapply(d.all2$SiteID,d.all2$clus.group,length)


# グループ間比較
layout(matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE))
# layout(matrix(c(1,1,1,2,2,2,
#                 1,1,1,2,2,2,
#                 1,1,1,2,2,2,
#                 3,3,3,4,4,4,
#                 3,3,3,4,4,4,
#                 3,3,3,4,4,4,
#                 5,5,5,5,5,5
#                 ), nrow = 7, ncol = 6, byrow = TRUE))
# パワポ用
# layout(matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2, byrow = TRUE))
par(mar = c(4.5,4.5,2,1),xpd=F,family = family_serif)

## saltation flux
boxplot(meanSF~clus.group, data = d.all2,log="y",xlab = "",
     ylab = expression(paste("saltation flux (",g, " ", m^{-2}," ", s^{-1},")")),
     ylim = c(10^-6, 10^-1),
     cex.axis=1.2,cex.lab =1.4,yaxt = "n")
sLab <- c(expression(10^{-6}),expression(10^{-5}),expression(10^{-4}),
          expression(10^{-3}),expression(10^{-2}),expression(10^{-1}))

axis(side=2,          #side2:左
     at=10^(-6:-1), #0から8まで1ずつ
     tck=0.03,            #長さ0.03のティック
     labels=sLab,
     mgp=c(1,0.5,0),
     cex.axis = 1.2
)
fPow <- function(x){(2:9)*10^x}
axis(side=2,        #side2:左
     at=sapply(-6:-1, fPow), #繰り返し(2:9)×10^(iLogL:iLogU)
     tck=0.01,          #長さ0.01のティック
     labels=FALSE,      #ラベル出力なし
     mgp=c(1,0.5,0)
)

title( main="(a)",adj = 0,cex.main = 1.4)
points(1:3,p.result.meanSE$mean[p.result.meanSE$factor == "meanSF"],pch = 3)

## range
boxplot(AsioV5range~clus.group, data = d.all2,xlab = "",ylab = expression(paste(italic(r)[uv]," (m)")),
     ylim = c(0,max(d.all2$AsioV5range)*1.1),
     cex.axis=1.2,cex.lab =1.4)
points(1:3,p.result.meanSE$mean[p.result.meanSE$factor == "AsioV5range"],pch = 3)
title( main="(b)",adj = 0,cex.main = 1.4)
# 有意差表記
text(1,6,"a",cex=1.2)
text(2,8.5,"b",cex=1.2)
text(3,6,"a",cex=1.2)

## cover
boxplot(QuadCover~clus.group, data = veg.cond,xlab = "",ylab = "cover (%)",
     ylim = c(0,max(d.all2$QuadCover)*1.2),
     cex.axis=1.2,cex.lab =1.4)
points(1:3,p.result.meanSE$mean[p.result.meanSE$factor == "QuadCover"],pch = 3)
title( main="(c)",adj = 0,cex.main = 1.4)
# 有意差表記
text(1,12,"a",cex=1.2)
text(2,39,"b",cex=1.2)
text(3,50,"c",cex=1.2)

## community height 
boxplot(QuadHeight~clus.group, data = veg.cond,xlab = "",ylab = "community height (cm)",
     ylim = c(0,max(d.all2$QuadHeight)*1.1),
     cex.axis=1.2,cex.lab =1.4, boxwex=0.8)
title( main="(d)",adj = 0,cex.main = 1.4)
points(1:3,p.result.meanSE$mean[p.result.meanSE$factor == "QuadHeight"],pch = 3)


## x軸ラベル
par(xpd=T)
mtext("group",side = 1,line = 3,adj = 0.5,cex = 1.2,at = -0.2)
par(xpd=F)

## 保存 
dev.copy(cairo_pdf, file=paste(path4,"/group_vegSA_sig3.pdf",sep=""), width = 9, height =9)
dev.off()







