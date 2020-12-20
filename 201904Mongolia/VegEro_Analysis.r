#path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1401春期モンゴル解析4"
# setwd(path2)
path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5"
setwd(path3)

# d <- read.csv("Veg_EroAnalysis5.csv", header =T)
# d <- read.csv("Veg_EroAnalysis7.csv", header =T)
d <- read.csv("Veg_EroAnalysis11.csv", header =T)
d$LateralCover[d$LateralCover=="#N/A"] <- NA
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
