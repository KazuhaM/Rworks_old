
# Init setting ------------------------------------------------------------
# work directory
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
setwd(path3)
path4 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1202PhDtheis_submission/Fig"

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

d.soil$ID <- as.factor(d$ID)
d.soil$Type <- as.factor(d$Type)


# クラスターによるグループ分け ----------------------------------------------------------

# 植被率、群落高、rangeのみのデータフレームを作る
veg.cond <- cbind(d$VegCover,d$VegComHgt,d$AsioV5range)
colnames(veg.cond) <- c("VegCover","VegComHgt","AsioV5range")
row.names(veg.cond) <- d$ID

# クラスター分類
gnum_st <- 4
clus<-hclust(dist(veg.cond),"ward.D2") #ウォード法改で
par(mar = c(5,6,3,2),family = family_serif)
plot(clus,cex = 1.7,cex.axis=1.5,cex.lab = 1.7, main = "") #グラフ表示
clus.g<- rect.hclust(clus,gnum_st)

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
result.lmD <- glm(D ~ VegComHgt,
                  data = d2,family = Gamma(link = log))
summary(result.lmD)
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
par(mar = c(5,6,3,2))
plot(D ~ VegComHgt, data = d2, xlim = c(0,16),
     col =rainbow(12)[round(d2$AsioV5range,0)],cex = 2,pch = 15,
     cex.lab = 1.7, cex.axis = 1.5,ylab = "D (gsm^-2)", xlab = "community height (cm)")
legend(0,0.2,title  ="range",1:max(round(d2$AsioV5range,0)),col = rainbow(12)[1:max(round(d2$AsioV5range,0))],pch=15,
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




# Analysis about others of D ----------------------------------------------

# ust~ z0
d2.logz <- d2
d2.logz$Z0 <- log(d2.logz$Z0)
result.lmD <- lm(Ut ~ VegCover + AsioV5range  + VegComHgt + Z0, data = d2.logz)
summary(result.lmD)

result.lmD <- lm(Ut ~ Z0, data = d2.logz)
summary(result.lmD)

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
eta.pred <- result.lmD$coefficients[1] + result.lmD$coefficients[2]*log(x)
lines(x,eta.pred)

# グループ間比較
# 正式作図用
# par("mar")
layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))
par(mar = c(5,6,3,2))
plot(d2$clus.group,d2$Ut,xlab = "",ylab = "threshold friction velocity (m/s)",
     ylim = c(0, max(d2$Ut)*1.1),
     cex.axis=1.7,cex.lab =2)
plot(d2$clus.group,d2$D,xlab = "group",ylab = "D (gsm^-2)",
     ylim = c(0, max(d2$D)*1.1),
     cex.axis=1.7,cex.lab =2)
# plot(d2$clus.group,d2$meanSF,xlab = "",ylab = "saltation flux (g/s m^2)",
#      ylim = c(0, max(d2$meanSF)*1.1),
#      cex.axis=1.7,cex.lab =2)
#########
plot(d2$clus.group,d2$Z0,log = "y",xlab = "",ylab = "roughness (m)",
     ylim = c(10^(-4),0.15),
     cex.axis=1.7,cex.lab =2
     ,yaxt = "n",)

sLab <- c(expression(10^{-4}),expression(10^{-3}),
          expression(10^{-2}),expression(10^{-1}))

axis(side=2,          #side2:左
     at=10^(-4:-1), #0から8まで1ずつ
     tck=0.03,            #長さ0.03のティック
     labels=sLab,
     mgp=c(1,0.5,0),
     cex.axis = 1.7
)
fPow <- function(x){(2:9)*10^x}
axis(side=2,        #side2:左
     at=sapply(-4:-1, fPow), #繰り返し(2:9)×10^(iLogL:iLogU)
     tck=0.01,          #長さ0.01のティック
     labels=FALSE,      #ラベル出力なし
     mgp=c(1,0.5,0)
)
plot(d2$clus.group,d2$AsioV5range,xlab = "",ylab = "range (m)",
     ylim = c(0,max(d2$AsioV5range)*1.1),
     cex.axis=1.7,cex.lab =2)
plot(d2$clus.group,d2$VegCover,xlab = "cluster group",ylab = "cover (%)",
     ylim = c(0,max(d2$VegCover)*1.1),
     cex.axis=1.7,cex.lab =2)
plot(d2$clus.group,d2$VegComHgt,xlab = "",ylab = "com. height (cm)",
     ylim = c(0,max(d2$VegComHgt)*1.1),
     cex.axis=1.7,cex.lab =2)

# sf
plot(d2$clus.group,d2$meanSF,xlab = "",ylab = "saltation flux (g/s m^2)",
     ylim = c(0, max(d2$meanSF)*1.1),
     cex.axis=1.7,cex.lab =2)

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


# 投稿用作図 -------------------------------------------------------------------
# クラスター

# テスト
par(family = family_serif)
plot(d2$c~d2$clus.group, xlab = "group",ylab = "D")

dev.copy(cairo_pdf, file=paste(path4,"/test.pdf",sep=""), width = 10, height = 10)
dev.off()

