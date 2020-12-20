path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/0402春期モンゴル解析5/distribution"
setwd(path3)

d.lateral <- read.csv("WsiteShrubDistribution.csv", header =T)
d.lateral$long_axis.cm. <- d.lateral$long_axis.cm. / 100
d.lateral$short_axis.cm. <- d.lateral$short_axis.cm. / 100
d.lateral$height.cm. <- d.lateral$height.cm. / 100
sitelev.l <- levels(d.lateral$Site)

result.l <- data.frame("site" =NA,"wind dir.E0left" =NA,"lateralC" =NA,"est_len" = NA)

for (i in 1:length(sitelev.l)) {
  d.l.temp <- d.lateral[d.lateral$Site == sitelev.l[i],]
  result.l.temp <- data.frame("site" =NA,"wind dir.E0left" =NA,"lateralC" =NA)
  for(waz in seq(0,360-360/32,length = 32)){
    d.l.temp$thetaC <- d.l.temp$SiteDir.E0_left. + waz -d.l.temp$shrub_dir.E0_left.
    d.l.temp$thetaC <- d.l.temp$thetaC * pi / 180
    shortax <- d.l.temp$short_axis.cm. /2
    longax <- d.l.temp$long_axis.cm./2
    d.l.temp$width <- 2*sqrt((( shortax* tan(d.l.temp$thetaC))^2 + longax^2) /
                               (1 + tan(d.l.temp$thetaC)^2))
    d.l.temp$LS <- d.l.temp$width * d.l.temp$height.cm.
    result.l.temp <- rbind(result.l.temp,c(sitelev.l[i], waz, sum(d.l.temp$LS)/20^2))
  }
  result.l.temp <- result.l.temp[-1,]
  rownames(result.l.temp) <- NULL
  result.l.temp$wind.dir.E0left <- as.factor(result.l.temp$wind.dir.E0left)
  result.l.temp$lateralC <- as.numeric(result.l.temp$lateralC)
  
  
  wr_filename <- paste(sitelev.l[i],"_shdist.csv", sep = "")
  wstr <- read.csv(wr_filename, header = T)
  wstr <- wstr[,3:ncol(wstr)]
  wstr <- as.matrix(wstr)
  
  est_len <- apply(wstr, 2, mean)
  
  radial.plot(est_len,lwd = 2,
              labels=c("","","","","","","",""),
              start=0,clockwise=FALSE, rp.type="p",radial.lim =seq(0,trunc(max(est_len))+1,length = 5),
              grid.col = "white",
              show.radial.grid = FALSE,grid.left = TRUE,show.grid.labels = 3)
  arrows(0, 0, cos(d.l.temp$SiteDir.E0_left.[1] * pi / 180),
         sin(d.l.temp$SiteDir.E0_left.[1] * pi / 180)  ,angle = 15, length = 0.15,lwd = 2)
  mtext(paste("Elongation: ",signif(max(est_len) / min(est_len),3),sep = ""), side = 3, line = 2, at = NA)
  
  par(new = T)
  
  radial.plot(result.l.temp$lateralC,lwd = 2,
              labels=c("E","NE","N","NW","W","SW","S","SE"),
              start=(d.l.temp$SiteDir.E0_left.[1] - 90)*pi /180 ,
              clockwise=FALSE, rp.type="p",
              radial.lim =signif(seq(0,max(result.l.temp$lateralC)*1.1,
                                     ,length = 5),2),
              show.radial.grid = TRUE,grid.left = FALSE,show.grid.labels = 1,grid.col = "black" ,
              line.col = "blue")
  
  
  dev.copy(pdf, file=paste("LC_rosePlot_",sitelev.l[i],".pdf",sep=""), width = 10, height = 10)
  dev.off()
  
  result.l <- rbind(result.l,cbind(result.l.temp,est_len))
  rownames(result.l) <- NULL
}
result.l <- result.l[-1,]
rownames(result.l) <- NULL

write.csv(result.l, paste(path3,"/LC_street.csv", sep = ""),row.names=FALSE)

