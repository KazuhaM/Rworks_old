# データの読み込み
d <- read.csv("t_test.csv", header = T)
d

# スチューデントのt検定
d.result.t <- t.test(d$family, d$seven, var.equal = T)
d.result.t

# 箱ひげ図の描画
boxplot(d)
mean(d)
