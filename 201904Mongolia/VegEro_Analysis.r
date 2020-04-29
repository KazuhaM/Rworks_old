#path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1401春期モンゴル解析4"
setwd(path2)
d <- read.csv("Veg_EroAnalysis4.csv", header =T)
d

pairs(d[,3:14])
#群落タイプ名入力
d$VegType[d$VegType== 1] <-"Bottom"
d$VegType[d$VegType== 2] <-"SA(N)"
d$VegType[d$VegType== 3] <-"SA(S)"
d$VegType[d$VegType== 4] <-"Nirtaria"



#typeごとの植被率と群落高, Z0
temp.type <- as.factor(d$VegType)
boxplot(d$QuadCover~temp.type,xlab = "community type", ylab = "coverage(%)",
        main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
dev.copy(pdf, file=paste("Type_Cov2.pdf",sep=""), width = 10, height = 10)
dev.off()
boxplot(d$QuadCommunityHeight~temp.type,xlab = "community type", ylab = "community height(m)",
        main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
dev.copy(pdf, file=paste("Type_Hght2.pdf",sep=""), width = 10, height = 10)
dev.off()
boxplot(d$Z0~temp.type,xlab = "community type", ylab = "roughness length(m)",
        main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
dev.copy(pdf, file=paste("Type_Z0.pdf2",sep=""), width = 10, height = 10)
dev.off()
boxplot(d$AveDev~temp.type,xlab = "community type", ylab = "NSDsf",
        main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
dev.copy(pdf, file=paste("Type_Avedev2.pdf",sep=""), width = 10, height = 10)
dev.off()

#粗度と臨界摩擦速度
plot(d$Z0,d$Ut,xlab = "roughness length(m)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
d.result <- lm(d$Ut~d$Z0)
abline(d.result)
text(0.05,5,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
                        round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
                        round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste("Ust_Z02.pdf",sep=""), width = 10, height = 10)
dev.off()
summary(d.result)

#粗度と臨界摩擦速度(色分け)
plot(d$Z0,d$Ut,xlab = "roughness length(m)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5, col = d$VegType, pch= d$VegType)
legend(0.2,4,c("Bottom","SA(N)","SA(S)","Nirtaria"), col = 1:4, pch= 1:4)
# d.result <- lm(d$Ut~d$Z0)
# abline(d.result)
# text(35,15,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
#                          round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
#                          round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste("Ust_Z0_VTCol2.pdf",sep=""), width = 10, height = 10)
dev.off()

#coverと臨界摩擦速度
plot(d$QuadCover,d$Ut,xlab = "Coverage", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
d.result <- lm(d$Ut~d$QuadCover)
abline(d.result)
text(35,5,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
                           round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
                           round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste("Ust_Cov2.pdf",sep=""), width = 10, height = 10)
dev.off()

#coverと臨界摩擦速度(色分け)
plot(d$QuadCover,d$Ut,xlab = "Coverage(%)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5, col = d$VegType, pch= d$VegType)
legend(35,4,c("Bottom","SA(N)","SA(S)","Nirtaria"),col = 1:4, pch= 1:4)
# d.result <- lm(d$Ut~d$QuadCover)
# abline(d.result)
# text(35,15,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
#                          round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
#                          round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste("Ust_Cov_VTCol2.pdf",sep=""), width = 10, height = 10)
dev.off()

#heightと臨界摩擦速度
temp.hght <- d$QuadCommunityHeight/100
plot(temp.hght,d$Ut,xlab = "community height(m)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
d.result <- lm(d$Ut ~ temp.hght)
abline(d.result)
text(0.12,5,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
                         round(d.result$coefficients[2],digits = 2),"x\nR^2: ",
                         round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste("Ust_Hght2.pdf",sep=""), width = 10, height = 10)
dev.off()

#heightと臨界摩擦速度(色分け)
temp.hght <- d$QuadCommunityHeight/100
plot(temp.hght,d$Ut,xlab = "community height(m)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5, col = d$VegType, pch= d$VegType)
legend(0.14,4,c("Bottom","SA(N)","SA(S)","Nirtaria"), col = 1:4, pch= 1:4)
# d.result <- lm(d$Ut~d$QuadCover)
# abline(d.result)
# text(35,15,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
#                          round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
#                          round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste("Ust_Hght_VTCol.pdf",sep=""), width = 10, height = 10)
dev.off()

#veg typeと臨界摩擦速度
temp.type <- as.factor(d$VegType)
boxplot(d$Ut~temp.type,xlab = "community type", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
d.result <- lm(d$Ut ~ temp.type)
summary(d.result)
dev.copy(pdf, file=paste("Ust_Type.pdf2",sep=""), width = 10, height = 10)
dev.off()

#植生量と臨界摩擦速度
plot(d$DomVegVol,d$Ut,xlab = "vegetation volume(cm^2)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5)
d.result <- lm(d$Ut ~ d$DomVegVol)
abline(d.result)
text(45000,15,label = paste("y = ",round(d.result$coefficients[1],digits = 2), " + ",
                           round(d.result$coefficients[2],digits = 2),"x\nR^2: ", 
                           round(summary(d.result)$r.squared,digits = 2) ,sep = ""))
dev.copy(pdf, file=paste("Ust_Vol.pdf2",sep=""), width = 10, height = 10)
dev.off()

library(MuMIn)
options(na.action = "na.fail")
#Z0と植生のGLM
glm.result <- lm(Z0 ~ AveDev+ VegType + QuadCover +QuadCommunityHeight+Sociability ,data = d)
summary(glm.result)
dredge(glm.result,rank="AIC")

#u*t と植生のGLM
glm.result <- lm(Ut ~ Z0 +AveDev + VegType + QuadCover+QuadCommunityHeight+Sociability ,data = d)
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
