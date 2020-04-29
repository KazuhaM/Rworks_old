#path2 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
# path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/0802春季モンゴル解析2/OriginalData"
path2 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2019/現行資料/1401春期モンゴル解析4"
setwd(path2)
d <- read.csv("Veg_EroAnalysis5.csv", header =T)
d

#粗度と平均saltation flux
plot(d$Z0,d$ave_SF,xlab = "roughness length(m)", ylab = "avarage saltation flux(g s^-1 m^-2)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5,col = d$VegType, pch= d$Event, cex = 1.5)
legend(0.7,10,c("Bottom","SA(N)","SA(S)","Nirtaria"), col = 1:4,pch=1)
legend(0.7,8,c("event1","event2","event3","event4"), pch = 1:4,col = 1)

dev.copy(pdf, file=paste("avesf_Z0.pdf",sep=""), width = 10, height = 10)
dev.off()

#被度と平均saltation flux
plot(d$QuadCover,d$ave_SF,xlab = "coverage(%)", ylab = "avarage saltation flux(g s^-1 m^-2)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5,col = d$VegType, pch= d$Event, cex = 1.5)
legend(40,10,c("Bottom","SA(N)","SA(S)","Nirtaria"), col = 1:4,pch=1)
legend(40,8,c("event1","event2","event3","event4"), pch = 1:4,col = 1)

dev.copy(pdf, file=paste("avesf_cov.pdf",sep=""), width = 10, height = 10)
dev.off()

#群落高と平均saltation flux
plot(d$QuadCommunityHeight,d$ave_SF,xlab = "height(cm)", ylab = "avarage saltation flux(g s^-1 m^-2)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5,col = d$VegType, pch= d$Event, cex = 1.5)
legend(14,6,c("Bottom","SA(N)","SA(S)","Nirtaria"), col = 1:4,pch=1)
legend(14,4,c("event1","event2","event3","event4"), pch = 1:4,col = 1)

dev.copy(pdf, file=paste("avesf_hgt.pdf",sep=""), width = 10, height = 10)
dev.off()

#平均saltation fluxと臨界摩擦速度
plot(d$ave_SF,d$Ut,xlab = "avarage saltation flux(g s^-1 m^-2)", ylab = "threshold friction velocity(m/s)",
     main = "",cex.axis=1.2, cex.lab=1.5,cex.main = 1.5,col = d$VegType, pch= d$Event, cex = 1.5)
legend(10,5,c("Bottom","SA(N)","SA(S)","Nirtaria"), col = 1:4,pch=1)
legend(10,4,c("event1","event2","event3","event4"), pch = 1:4,col = 1)

dev.copy(pdf, file=paste("avesf_ut.pdf",sep=""), width = 10, height = 10)
dev.off()
