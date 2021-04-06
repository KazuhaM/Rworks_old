# library("ggplot2")
# path="E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1202PhDthesis_submission"
path <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1202PhDthesis_submission"
setwd(path)
d <- read.csv("site_period_for_table2.csv",header =T)
siteNO <- 8

### Nitrariaサイトを除いてSAのみに
d <- d[1:siteNO,]

group <- c(2,2,2,3,5,1,2,1)
d <- cbind(d,group)


ev <- read.csv("EventPeriod2.csv")
d$Start <- as.POSIXct(d$Start)
d$End <- as.POSIXct(d$End) 
d <- d[order(d$SiteID,decreasing=T),]
d <- d[order(d$group,decreasing=T),]
ev$Start <-as.POSIXct(ev$Start)
ev$End <-as.POSIXct(ev$End)
ev$Ave <-as.POSIXct(ev$Ave)

x <- 1:siteNO
timeline <-c(as.POSIXct("2019-04-27 15:00:00 JST"),
             seq(as.POSIXct("2019-04-28 0:00:00 JST"),
                 as.POSIXct("2019-05-11 0:00:00 JST"),
                 by = 60*60*12),
             as.POSIXct("2019-05-11 10:24:00 JST"))


par(mar=c(10, 6, 5, 1),xpd=F,family = family_serif)
plot(NULL,ylim = c(1,siteNO),xlim=c(min(d$Start),max(d$End)),xaxt ="n",yaxt="n",
    xlab = "",ylab="")
rect(d$Start,x-0.1,d$End,x+0.1, col  = "Black")
axis(side=1, labels=FALSE,at = timeline)
mtext("time", side = 1, line = 8, at = NA,cex = 1.7)
mtext("sites", side = 2, line = 4, at = NA,
      cex = 1.7)
#text(x=timeline, par("usr")[3], labels=timeline, srt=-90, pos=4, xpd=TRUE)
axis.POSIXct(1, at=timeline, format="%m/%d %H:%M",
             las=2,cex.axis = 1.3)
axis(side=2, labels=d$SiteID,at = 1:length(d$SiteID),
     las=1,cex.axis = 1.3)

abline(v=ev$Start,lty=2 )
abline(v=ev$End,lty=2 )
rect(ev$Start,0,ev$End,11, col  = rgb(0.8,0.8,0.8,alpha=0.3),lty=0)
par(xpd=T)
mtext(ev$Event,side = 3,at=ev$Ave,line =1,las = 2,cex = 1.2)
text(par()$usr[1] -40000, par()$usr[4]+0.8,"Events",cex = 1.2)

dev.copy(cairo_pdf, file=paste(path,"/Fig/site_eventSA.pdf",sep=""), width = 13, height = 6)
dev.off()

# for(i_site in 1:length(d$SiteID)){
#  mtext(d$SiteID[i_site], side = 2, line = 0, at = i_site)
# }
# p <- ggplot( d)
# p +
#   geom_segment(aes(x = SiteID, xend = SiteID, y = Start, yend = End),
#                size = 5, colour = "black", alpha = 0.6) +
#   coord_flip() +
#   scale_x_discrete(limits = as.character(d$SiteID))+
#   ylab("time")
# #theme_bw()
#   #theme(text = element_text(family = windowsFonts()$serif))
# #print(p)
# 
# ggsave("site_periods.pdf",device = cairo_pdf,width= 15,height=10)