# ハッシュでコメントアウト
# 作業ディレクトリの変更
path <- "D:/Users/%username%/Documents"
setwd(path)

a <- 10   #代入
a <- 10 + 2 #算術演算

d <- read.csv("<ファイル名>.csv", header = T)   #CVSファイル読み込み、データの読み込み
d #変数を閲覧
plot(x,y) #xとyの散布図を書く。x,yはベクトルデータ

str(d)  #データ形式を表示
d2 <- c(1,2,3,4,5) #(1,2,3,4,5)というベクトルの生成


rand.d <- rnorm(10000,10,5)
hist(rand.d)
