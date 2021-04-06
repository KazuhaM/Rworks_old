library("rgl")
#path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1401春期モンゴル解析4"
# setwd(path2)
# path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"

setwd(path3)

# d <- read.csv("Veg_EroAnalysis5.csv", header =T)
# d <- read.csv("Veg_EroAnalysis7.csv", header =T)
d <- read.csv("Veg_EroAnalysis14.csv", header =T)
d.b<-read.csv("Veg_EroAnalysis14_B.csv", header =T)
d.soil <- read.csv("Veg_EroAnalysis14_soil.csv", header =T)

# names
# [1] "SiteID"      "Event"       "ID"          "Type"        "Type2"           "E"           "c"          
# [8] "d0"          "R"           "AveDev"      "SdDev"       "WM_l_hgt"    "z0.d0"       "meanSF"     
# [15] "sdSF"        "aveWS"       "aveWD"       "VegCover"    "VegComHgt"   "VegVol"      "V5Sill"     
# [22] "V5range"     "AsioV5Sill"  "AsioV5range" "Z0_scale"    "Z0"          "Ut"  

# 型変換
d$SiteID <- as.factor(d$SiteID)
d$Event <- as.factor(d$Event)
d$ID <- as.factor(d$ID)
d$Type <- as.factor(d$Type)
d$Type2 <- as.factor(d$Type2)
d$Z0_scale <- as.factor(d$Z0_scale)
str(d)

d.soil$ID <- as.factor(d$ID)
d.soil$Type <- as.factor(d$Type)
str(d.soil)




hist(d$Ut)
result.ut.z <- lm(Ut~VegCover + VegComHgt + AsioV5range + AsioV5Sill+Z0 ,data=d)
summary(result.ut.z)
# 基本
plot(d$Ut~d$Z0, log="x",cex = 2,xlab = "roughness (m)",ylab = "threshold friction velocity(m/s)",
        cex.axis=1.2,cex.lab =1.2)
plot(d$Ut~d$VegCover,cex = 2,xlab = "cover (%)",ylab = "threshold friction velocity(m/s)",
     cex.axis=1.2,cex.lab =1.2)
plot(d$Ut~d$VegComHgt,cex = 2,xlab = "com. height (cm)",ylab = "threshold friction velocity(m/s)",
     cex.axis=1.2,cex.lab =1.2)
plot(d$Ut~d$AsioV5range,cex = 2,xlab = "range (m)",ylab = "threshold friction velocity(m/s)",
     cex.axis=1.2,cex.lab =1.2)

plot(d$meanSF~d$Z0, log="x",cex = 2,xlab = "roughness (m)",ylab = "saltation flux (g/s m^2)",
     cex.axis=1.2,cex.lab =1.2)
plot(d$meanSF~d$VegCover,cex = 2,xlab = "cover (%)",ylab = "saltation flux (g/s m^2)",
     cex.axis=1.2,cex.lab =1.2)
plot(d$meanSF~d$VegComHgt,cex = 2,xlab = "com. height (cm)",ylab = "saltation flux (g/s m^2)",
     cex.axis=1.2,cex.lab =1.2)
plot((d$VegCover * d$VegComHgt),d$meanSF,cex = 2)
plot(d$meanSF~d$AsioV5range,cex = 2,xlab = "range (m)",ylab = "saltation flux (g/s m^2)",
     cex.axis=1.2,cex.lab =1.2)

plot(d$Z0~d$VegCover,log="y",cex = 2,xlab = "cover (%)",ylab = "roughness (m)",
     cex.axis=1.2,cex.lab =1.2,
     col=rainbow(12)[round(d$AsioV5range,0)])
plot(d$Z0~d$VegComHgt,log="y",cex = 2,xlab = "height (cm)",ylab = "roughness (m)",
     cex.axis=1.2,cex.lab =1.2)
plot(d$Z0~d$AsioV5range,log="y",cex = 2,xlab = "range (m)",ylab = "roughness (m)",
     cex.axis=1.2,cex.lab =1.2)

plot(d$VegCover,d$AsioV5range)

result.SF.v <- lm(meanSF~VegCover * VegComHgt * AsioV5Sill * AsioV5Sill, data = d)
result.ut.v 

# boxplot(d$AsioV5range)
plot(d$Type,d$AsioV5range)
plot(d$Type,d$VegCover)
plot(d$Type,d$VegComHgt)
plot(sort(d$AsioV5range),cex = 1.2,pch="●")


library(vegan)
# いろいろいれてみた
# veg.cond <- cbind(d$Type,d$VegCover,d$VegComHgt,d$AsioV5Sill,d$AsioV5range,d$Z0)
# colnames(veg.cond) <- c("VegType","VegCover","VegComHgt","AsioV5Sill","AsioV5range","Z0")
# row.names(veg.cond) <- d$ID

# Cover入←これを使う
veg.cond <- cbind(d$VegCover,d$VegComHgt,d$AsioV5range)
colnames(veg.cond) <- c("VegCover","VegComHgt","AsioV5range")
row.names(veg.cond) <- d$ID

# # Coverなし
# veg.cond <- cbind(d$Type,d$VegComHgt,d$AsioV5range,d$Z0)
# colnames(veg.cond) <- c("VegType","VegComHgt","AsioV5range","Z0")
# row.names(veg.cond) <- d$ID

plot3d(veg.cond)
gnum_st <- 4
# veg.cond <- t(veg.cond)
d.mds<-decorana(veg.cond)
summary(d.mds)
plot(d.mds)
d.mds<-metaMDS(veg.cond,zerodist="add") ##### zerodist="add" を追加
stressplot(d.mds) # Shepard diagram を描く。

plot(d.mds, type="t",cex = 1.1)
# plot(d.mds, display = c("sites", "species"), choices = c(1, 2),
#      type = "t", shrink = FALSE)
points(0,0,pch=3,cex = 2)

# sco<-scores(d.mds,display="sites") #地点の序列化スコア
clus<-hclust(dist(veg.cond),"ward.D2") #ウォード法改で
# clus<-hclust(dist(sco),"ward.D2") #ウォード法改で
plot(clus,cex = 1.7,cex.axis=1.5,cex.lab = 1.7, main = "") #グラフ表示
rect.hclust(clus,gnum_st)

k<-cutree(clus,gnum_st)
ordipointlabel(d.mds,display="sites",col = c(1:(gnum_st+1))[k],pch = c(1:gnum_st+1)[k])
# ordiarrows(d.mds,display = "species",levels=gnum_st,show.groups = c("VegType","VegCover","VegComHgt","AsioV5Sill","AsioV5range","Z0"))
ordiellipse(d.mds,k, display="sites",kind="sd", conf=0.9,lwd=1,lty=2, col="black")

# which(d$ID=="W2-3_6")
# 3グループ
# clus.group <- c(1,1,1,3,3,3,2,2,1,2,1,1,2)
clus.group <- c(1,1,1,3,3,3,2,2,1,2,4,4,2)

# coverなし
# clus.group <- c(2,2,2,1,1,1,1,1,3,2,3,3,4)

# 2グループ
# clus.group <- c(1,1,1,1,1,1,2,2,1,2,1,1,2)

veg.cond <- cbind(veg.cond, clus.group)
colnames(veg.cond)[length(colnames(veg.cond))] <- "Group"
d2 <- cbind(d,clus.group )
d2$clus.group <- as.factor(d2$clus.group)


plot(d2$clus.group,d2$Ut,xlab = "cluster group",ylab = "threshold friction velocity (m/s)",
     cex.axis=2,cex.lab =2)
plot(d2$clus.group,d2$meanSF,xlab = "cluster group",ylab = "saltation flux (g/s m^2)",
     cex.axis=2,cex.lab =2)
#########
plot(d2$clus.group,d2$Z0,log = "y",xlab = "cluster group",ylab = "roughness (m)",
     cex.axis=2,cex.lab =2)
plot(d2$clus.group,d2$AsioV5range,xlab = "cluster group",ylab = "range (m)",
     cex.axis=2,cex.lab =2)
plot(d2$clus.group,d2$AsioV5Sill,xlab = "cluster group",ylab = "sill(m^2)",
     cex.axis=2,cex.lab =2)
plot(d2$clus.group,d2$VegCover,xlab = "cluster group",ylab = "cover (%)",
     cex.axis=2,cex.lab =2)
plot(d2$clus.group,d2$VegComHgt,xlab = "cluster group",ylab = "com. height (cm)",
     cex.axis=2,cex.lab =2)

plot(d2$Z0[d2$clus.group==1],d2$AsioV5range[d2$clus.group==1],cex = 2)
plot(d2$VegComHgt[d2$clus.group==1],d2$AsioV5range[d2$clus.group==1],cex = 2)
plot(d2$V5range,d2$VegCover)

# 正式作図用
# par("mar")
layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))
par(mar = c(5,6,3,2))
plot(d2$clus.group,d2$Ut,xlab = "",ylab = "threshold friction velocity (m/s)",
     ylim = c(0, max(d2$Ut)*1.1),
     cex.axis=1.7,cex.lab =2)
plot(d2$clus.group,d2$meanSF,xlab = "",ylab = "saltation flux (g/s m^2)",
     ylim = c(0, max(d2$meanSF)*1.1),
     cex.axis=1.7,cex.lab =2)
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


# soil情報グループ間比較
d.soil <- cbind(d.soil,clus.group )
d.soil$clus.group <- as.factor(d.soil$clus.group)

boxplot(HorseNew ~ clus.group,data= d.soil,ylim=c(0,1))
boxplot(SheepNew ~ clus.group,data= d.soil,ylim=c(0,3))
boxplot(CamelNew ~ clus.group,data= d.soil,ylim=c(0,6))
boxplot(GSGravelSize ~ clus.group,data= d.soil,ylim=c(0,3))
boxplot(GAIravelAmount ~ clus.group,data= d.soil,ylim=c(0,4))
boxplot(Clust  ~ clus.group,data= d.soil,ylim=c(0,2))
boxplot(Crack   ~ clus.group,data= d.soil,ylim=c(0,3))
boxplot(HdHardness  ~ clus.group,data= d.soil,ylim=c(0,4))

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
plot(meanSF~ Ut, data = d.soil[d.soil$meanSF<0.01,])

# 重回帰
d2.hCov <- d2[d2$clus.group != 2,]
# d2.hCov <- d2[d2$clus.group == 1,]
# d2.hCov <- d2
result.SF.v <- lm(meanSF~(VegCover + VegComHgt + AsioV5range)^2 , data = d2.hCov)
summary(result.SF.v)
result.Ut.v <- lm(Ut~(VegCover + VegComHgt + AsioV5range)^2 , data = d2.hCov)
summary(result.Ut.v)

plot(d2.hCov$Z0~d2.hCov$VegCover,log="y")
plot(d2.hCov$AsioV5range ~d2.hCov$VegCover)

# d2.hCov$VegCover = 1/d2.hCov$VegCover
d2.hCov$VegCover = sqrt(d2.hCov$VegCover/100)
d2.hCov$VegComHgt = d2.hCov$VegComHgt/100
result.Z0.v <- lm(Z0~ VegCover + VegComHgt + AsioV5range , data = d2.hCov)
summary(result.Z0.v) 

est.z <- d2.hCov$VegComHgt   /  ( d2.hCov$AsioV5range*d2.hCov$VegCover)
result.est <- lm(d2.hCov$Z0~est.z)
summary(result.est)
plot(d2.hCov$Z0~est.z,log = "y",cex = 1.5,xlab = "height / cover * range ",ylab = "roughness (m)",
     cex.axis=1.2,cex.lab =1.2)

##########################################
# # 仮説をもとに、cover, rangeで
plot(d$AsioV5range,d$VegCover)
plot(d$meanSF~d$VegCover)
veg.cond <- cbind(d$Type,d$VegCover,d$AsioV5range)
colnames(veg.cond) <- c("VegType","VegCover","AsioV5range")
row.names(veg.cond) <- d$ID


gnum_st <- 3
# veg.cond <- t(veg.cond)
d.mds<-decorana(veg.cond)
summary(d.mds)
plot(d.mds)
d.mds<-metaMDS(veg.cond,zerodist="add") ##### zerodist="add" を追加
stressplot(d.mds) # Shepard diagram を描く。

plot(d.mds, type="t")

sco<-scores(d.mds,display="sites") #地点の序列化スコア
clus<-hclust(dist(sco),"ward.D2") #ウォード法改で
plot(clus) #グラフ表示
rect.hclust(clus,gnum_st)

which(d$ID=="S1-4_3")
# 3グループ
clus.group <- c(1,1,1,1,1,1,1,1,1,1,1,1,1)
# 2グループ
clus.group <- c(1,1,1,1,1,1,2,2,1,2,1,1,2)

veg.cond <- cbind(veg.cond, clus.group)
colnames(veg.cond)[length(colnames(veg.cond))] <- "Group"
d2 <- cbind(d,clus.group )
d2$clus.group <- as.factor(d2$clus.group)

plot(d2$clus.group,d2$Ut)
plot(d2$clus.group,d2$meanSF)
#########
plot(d2$clus.group,d2$Z0,log = "y")
plot(d2$clus.group,d2$AsioV5range)
plot(d2$clus.group,d2$AsioV5Sill)
plot(d2$clus.group,d2$VegCover)
plot(d2$clus.group,d2$VegComHgt)

plot(d2$Z0[d2$clus.group==1],d2$AsioV5range[d2$clus.group==1],cex = 2)
plot(d2$VegComHgt[d2$clus.group==1],d2$AsioV5range[d2$clus.group==1],cex = 2)


# rangeの大小による分類
d3 <- d
d3$range.group[d3$AsioV5range > 4.5] <- 2
d3$range.group[d3$AsioV5range <= 4.5] <- 1
d3$range.group <- as.factor(d3$range.group)

plot(d3$range.group,d3$Ut)
plot(d3$range.group,d3$meanSF)
#########
plot(d3$VegCover[d3$range.group==1],d3$Z0[d3$range.group==1],log="y",col =1,
     xlim = c(0,max(d3$VegCover)),ylim = c(min(d3$Z0),max(d3$Z0)),cex=1.5)
par(new = T)
plot(d3$VegCover[d3$range.group==2],d3$Z0[d3$range.group==2],log="y",col =2,
     xlim = c(0,max(d3$VegCover)),ylim = c(min(d3$Z0),max(d3$Z0)),cex=1.5)
plot(d3$range.group,d3$Z0,log = "y")
plot(d3$range.group,d3$AsioV5range)
plot(d3$range.group,d3$AsioV5Sill)
plot(d3$range.group,d3$VegCover)
plot(d3$range.group,d3$VegComHgt)





################################ 素材 ####################################################
# d$LateralCover[d$LateralCover=="#N/A"] <- NA
d$LateralCover <- as.numeric(d$LateralCover)

pairs(d[,3:14])

# 群落タイプ数字##########################################################

#粗度と臨界摩擦速度(色分け)
plot(d$Z0,d$Ut,xlab = "roughness length(m)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5, col = d$VegType, pch= d$VegType)
legend(0.08,0.5,c("Bottom","SA(N)","SA(S)","Nirtaria"), col = 1:4, pch= 1:4)
# d.result <- lm(d$Ut~d$Z0)
# abline(d.result)
# text(35,15,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
#                          round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
#                          round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Ust_Z0_VTCol2.pdf",sep=""), width = 10, height = 10)
dev.off()

#粗度と臨界摩擦速度(色分け)　　粗度対数表示
plot(d$Z0,d$Ut,xlab = "roughness length(m, log)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5, col = d$VegType, pch= d$VegType, log="x")
legend(0.0002,0.9,c("Bottom","SA(N)","SA(S)","Nirtaria"), col = 1:4, pch= 1:4)
# d.result <- lm(d$Ut~d$Z0)
# abline(d.result)
# text(35,15,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
#                          round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
#                          round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Ust_logZ0_VTCol2.pdf",sep=""), width = 10, height = 10)
dev.off()
abline(v=1)
abline(v=0.2)
abline(v=0.04)
abline(v=0.01)
abline(v=0.001)
abline(v=0.0003)
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Ust_logZ0_VTCol2_refline.pdf",sep=""), width = 10, height = 10)
dev.off()
#coverと臨界摩擦速度
plot(d$QuadCover,d$Ut,xlab = "Coverage(%)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
d.result <- lm(d$Ut~d$QuadCover)
abline(d.result)
text(35,1,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
                          round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
                          round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Ust_Cov2.pdf",sep=""), width = 10, height = 10)
dev.off()

#coverと臨界摩擦速度(色分け)
plot(d$QuadCover,d$Ut,xlab = "Coverage(%)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5, col = d$VegType, pch= d$VegType)
legend(35,1,c("Bottom","SA(N)","SA(S)","Nirtaria"),col = 1:4, pch= 1:4)
# d.result <- lm(d$Ut~d$QuadCover)
# abline(d.result)
# text(35,15,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
#                          round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
#                          round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Ust_Cov_VTCol2.pdf",sep=""), width = 10, height = 10)
dev.off()

#heightと臨界摩擦速度
temp.hght <- d$QuadCommunityHeight/100
plot(temp.hght,d$Ut,xlab = "community height(m)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
d.result <- lm(d$Ut ~ temp.hght)
abline(d.result)
text(0.03,0.5,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
                          round(d.result$coefficients[2],digits = 2),"x\nR^2: ",
                          round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Ust_Hght2.pdf",sep=""), width = 10, height = 10)
dev.off()

#heightと臨界摩擦速度(色分け)
temp.hght <- d$QuadCommunityHeight/100
plot(temp.hght,d$Ut,xlab = "community height(m)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5, col = d$VegType, pch= d$VegType)
legend(0.03,0.5,c("Bottom","SA(N)","SA(S)","Nirtaria"), col = 1:4, pch= 1:4)
# d.result <- lm(d$Ut~d$QuadCover)
# abline(d.result)
# text(35,15,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
#                          round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
#                          round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Ust_Hght_VTCol.pdf",sep=""), width = 10, height = 10)
dev.off()


#植生量と臨界摩擦速度
plot(d$DomVegVol,d$Ut,xlab = "vegetation volume(cm^2)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
d.result <- lm(d$Ut ~ d$DomVegVol)
abline(d.result)
text(45000,0.9,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
                           round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
                           round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Ust_Vol2.pdf",sep=""), width = 10, height = 10)
dev.off()

# lateral coverとz0/h
plot(d$LateralCover[!is.na(d$LateralCover)],
     d$Z0[!is.na(d$LateralCover)], #/d$QuadCommunityHeight[!is.na(d$LateralCover)],
     xlab = "Lateral cover",ylab = "z_0 (m, log)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5,log = "y",pch = 16,cex = 1.5)
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Lc_Z.pdf",sep=""), width = 10, height = 10)
dev.off()

#粗度と植被率（色分け)　粗度対数表示
plot(d$QuadCover ,d$Z0, xlab = "cover (%)", ylab = "roughness length(m, log)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5, col = d$VegType, pch= d$VegType, log="y")
legend(40,0.1,c("Bottom","SA(N)","SA(S)","Nirtaria"), col = 1:4, pch= 1:4)
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_z_cov.pdf",sep=""), width = 10, height = 10)
dev.off()

#粗度と群落高（色分け)　粗度対数表示
plot(d$QuadCommunityHeight ,d$Z0, xlab = "community height (m)", ylab = "roughness length(m, log)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5, col = d$VegType, pch= d$VegType, log="y")
legend(2,0.0005,c("Bottom","SA(N)","SA(S)","Nirtaria"), col = 1:4, pch= 1:4)
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_z_height.pdf",sep=""), width = 10, height = 10)
dev.off()

#粗度とてすと　粗度対数表示
plot(d$NumofSpeceis ,d$Z0, xlab = "community height (m)", ylab = "roughness length(m, log)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5, col = d$VegType, pch= d$VegType, log="y")
legend(2,0.0005,c("Bottom","SA(N)","SA(S)","Nirtaria"), col = 1:4, pch= 1:4)
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_z_height.pdf",sep=""), width = 10, height = 10)
dev.off()
# 群落タイプ名分け##########################################################
#群落タイプ名入力
d$VegType[d$VegType== 1] <-"Bottom"
d$VegType[d$VegType== 2] <-"SA(N)"
d$VegType[d$VegType== 3] <-"SA(S)"
d$VegType[d$VegType== 4] <-"Nirtaria"


#typeごとの植被率と群落高, Z0
temp.type <- as.factor(d$VegType)
boxplot(d$QuadCover~temp.type,xlab = "community type", ylab = "cover(%)",
        main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Type_Cov2.pdf",sep=""), width = 10, height = 10)
dev.off()
boxplot(d$QuadCommunityHeight~temp.type,xlab = "community type", ylab = "community height(m)",
        main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Type_Hght2.pdf",sep=""), width = 10, height = 10)
dev.off()
boxplot(d$Z0~temp.type,xlab = "community type", ylab = "roughness length(m)",
        main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Type_Z0_2.pdf",sep=""), width = 10, height = 10)
dev.off()
boxplot(d$Z0~temp.type,xlab = "community type", ylab = "roughness length(m, log)",
        main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5,log="y")
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Type_logZ0_2.pdf",sep=""), width = 10, height = 10)
dev.off()
abline(h=1)
abline(h=0.2)
abline(h=0.04)
abline(h=0.01)
abline(h=0.001)
abline(h=0.0003)
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/Type_logZ0_refline.pdf",sep=""), width = 10, height = 10)
dev.off()
boxplot(d$AveDev~temp.type,xlab = "community type", ylab = "NSDsf",
        main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Type_Avedev2.pdf",sep=""), width = 10, height = 10)
dev.off()

#粗度と臨界摩擦速度
plot(d$Z0,d$Ut,xlab = "roughness length(m)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5,log ="x",xlim =c(min(d$Z0),max(d$Z0)),
     ylim = c(0,max(d$Ut)))
d.result <- lm(d$Ut~log(d$Z0))
y <- function(x){ return(d.result$coefficients[2]*log(x)+d.result$coefficients[1]) }
par(new = T)
plot(y,0.0001,0.1,xlim =c(min(d$Z0),max(d$Z0)),
     ylim = c(0,max(d$Ut)),log = "x",xlab = "", ylab = "",xaxt  = "n" ,yaxt = "n")

text(0.0004,0.9,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
                        round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
                        round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Ust_Z02.pdf",sep=""), width = 10, height = 10)
dev.off()
summary(d.result)

#veg typeと臨界摩擦速度
temp.type <- as.factor(d$VegType)
boxplot(d$Ut~temp.type,xlab = "community type", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
# d.result <- lm(d$Ut ~ temp.type)
# summary(d.result)
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Ust_Type2.pdf",sep=""), width = 10, height = 10)
dev.off()

####################################################################################

library(MuMIn)
options(na.action = "na.fail")
#Z0と植生のGLM
glm.result <- lm(Z0 ~ AveDev+ VegType + QuadCover +QuadCommunityHeight+Sociability +avarageWD.1,data = d)
summary(glm.result)
dredge(glm.result,rank="AIC")

#u*t と植生のGLM
glm.result <- lm(Ut ~ Z0 +AveDev + VegType + QuadCover+QuadCommunityHeight+Sociability+avarageWD.1 ,data = d)
summary(glm.result)
dredge(glm.result,rank="AIC")

#植生量ver
#Z0と植生のGLM
glm.result <- lm(Z0 ~ AveDev+ VegType +DomVegVol +Sociability ,data = d)
summary(glm.result)
dredge(glm.result,rank="AIC")

#u*t と植生のGLM
glm.result <- lm(Ut ~ Z0 +AveDev + VegType + DomVegVol +Sociability ,data = d)
summary(glm.result)
dredge(glm.result,rank="AIC")

plot(d$QuadCover,d$Z0)
plot(d$QuadCommunityHeight,d$Z0)
plot()


# 風向ごと（-25~100, -150~-25）の植生とz0, 臨界摩擦速度と植生
plot(d$QuadCover[d$avarageWD.1 < -25],d$Ut[d$avarageWD.1 < -25],xlab = "Coverage(%)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5, col = d$VegType[d$avarageWD.1 < -25], pch= 1, ylim = c(0.3, 1.1), xlim = c(0,48))
legend(35,1,c("Bottom","SA(N)","SA(S)","Nirtaria"),col = 1:4, pch= 1)
par(new = T)
plot(d$QuadCover[d$avarageWD.1 > -25],d$Ut[d$avarageWD.1 > -25],xlab = "", ylab = "",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5, col = d$VegType[d$avarageWD.1 < -25], pch= 2, ylim = c(0.3, 1.1), xlim = c(0,48))
legend(35,0.85,c("Bottom","SA(N)","SA(S)","Nirtaria"),col = 1:4, pch= 2)
# d.result <- lm(d$Ut~d$QuadCover)
# abline(d.result)
# text(35,15,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
#                          round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
#                          round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste(path3,"/VegEro_Analysis/10_Ust_Cov_VTCol2.pdf",sep=""), width = 10, height = 10)
dev.off()
