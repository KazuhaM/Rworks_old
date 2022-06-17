library(plotrix)

path3 <- "D:/SpAr"
setwd(path3)

aniso.var <- read.csv("21forVariogram/anisovar_allresult2.csv", header =T)
sites.id <- read.csv("Sites_var.csv",header =T)

for(i_site in 1:nrow(sites.id)){
    temp.d <- aniso.var[aniso.var$SiteID == sites.id[i_site,],]
    temp.colnames <- temp.d[,"degree"]
    temp.colnames <- c(temp.colnames, temp.colnames+180)
    temp.d2 <- temp.d[,"range"]
    temp.d2 <- t(temp.d2)
    temp.d2 <- data.frame(cbind(temp.d2, temp.d2))
    colnames(temp.d2) <- temp.colnames

    radial.plot(temp.d2,lwd = 2,
              # labels=c("N","NNE","NE","ENE","E","ESE","SE","SSE",
              #          "S","SSW","SW","WSW","W","WNW","NW","NNW"),
              labels = c("","","","","","","",""),
              start=pi/2,clockwise=TRUE, rp.type="p",radial.lim =c(0,trunc(max(temp.d2))+1),
              show.radial.grid = TRUE,grid.left = TRUE,show.grid.labels = 3)

    dev.copy(pdf, file=paste("21forVariogram/radplt_",sites.id[i_site,],".pdf",sep=""), width = 10, height = 10)
    dev.off()
}