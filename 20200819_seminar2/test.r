# データの読み込み
d <- read.csv("SpCor_summer.csv", header = T)
d

# グラフの描画
plot(d$swimming_accidents,d$ice_cream, 
     main = "Spurious correlation in summer vacation",
     xlab = "Number of accidents while swimming", ylab = "Ice cream sales",
     xlim = c(0, 50), ylim = c(0, max(d$ice_cream)),
     cex.main = 1.5, cex.lab = 1.2)
