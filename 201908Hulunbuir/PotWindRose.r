d <- read.csv("", header = T)
plot(d$x,d$y,col=rainbow(d$WS%/% 7 ,start = 7, end = 1), xlim=c(-10000,10000), ylim=c(-10000,10000))