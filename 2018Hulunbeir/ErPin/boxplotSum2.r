# without 2015 data
setwd("D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/論文投稿/修論投稿/解析/ErosionPin")
library(stats)
library(ggplot2)
library(ggThemeAssist)
x<-read.csv("erosion_3year.csv",header=TRUE)
str(x)
x <- transform(x,site = factor(site, levels = c("CT","SC","Site14","Site13")))

gg2  <- ggplot(data=x,aes(x = site, y = mm, fill = site))+
        geom_boxplot()+
        facet_wrap(~year)+
        theme_bw()+
        stat_summary(fun = "mean", geom = "point", shape = 4, size = 2.) +
        theme(plot.background = element_rect(fill = "white", colour = NA),
              panel.grid.major = element_line(colour = "gray92", linetype = "dashed",size = 0.3), 
              panel.grid.minor = element_line(colour = "gray88", linetype = "blank"), 
              panel.background = element_rect(colour = "white"), 
              axis.text = element_text(family = "sans", size = 12),
              axis.title = element_text(size = 18), 
              axis.ticks = element_line(linetype = "blank"),
              axis.line = element_line(colour = NA),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 15), 
              legend.text = element_text(size = 14), 
              legend.background = element_rect(fill = "white"), 
              legend.position = "bottom", 
              legend.direction = "horizontal",
              legend.title = element_text(family = "serif"),
              strip.text.x = element_text(size = 13)
              ) +
        xlab("Year")+
        ylab("Erosion pin (mm)")+
        scale_fill_grey(start = 0.9, end = 0.1)+
        scale_color_grey(start = 0.1, end = 0.1)+
        labs(site = NULL,fill = NULL)
print(gg2)

xt<-xtabs(~DataYear + Year2,data=x[!is.na(x$Erosion),])
xt <- t(xt)
dim(xt)<-c(16,1)
xt <- paste("n=", xt, sep="")

boxplot(Erosion~ Year2*DataYear,data=x,ylab = "ErosionPin (mm)",xlab= "Year/Treatment")
abline(v=4.5)
abline(v=8.5)
abline(v=12.5)
mtext(xt, side = 1, line = 2, at = 1:16)
