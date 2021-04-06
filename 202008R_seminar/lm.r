# データの読み込み
d <- read.csv("SpCor_summer.csv", header = T)
d

# 回帰分析
d.result.lm <- lm(ice_cream ~ swimming_accidents, data = d)
summary(d.result.lm) #結果の表示

# 散布図描画
plot(d$swimming_accidents,d$ice_cream, 
     main = "Spurious correlation in summer vacation",
     xlab = "Number of accidents while swimming", ylab = "Ice cream sales",
     xlim = c(0, max(d$swimming_accidents)), ylim = c(0, max(d$ice_cream)),
     cex.main = 1.5, cex.lab = 1.2)
# 回帰直線描画
abline(d.result.lm$coefficients)
