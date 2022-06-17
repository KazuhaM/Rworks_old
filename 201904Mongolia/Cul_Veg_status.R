
# initialize --------------------------------------------------------------


path3 <- "D:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
# path3 <- "E:/Clouds/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2020/00working/1102MongoliaAnalysis7/Cul"
setwd(path3)

d.veg <- read.csv("SiteVegStatus_CH.csv", header =T)
d.veg$SiteID <- as.factor(d.veg$SiteID)


# mean --------------------------------------------------------------------
result.veg.mean <- data.frame(matrix(rep(0, 1), nrow=length(levels(d.veg$SiteID))))
row.names(result.veg.mean) <- levels(d.veg$SiteID)

d.veg.19 <- d.veg[d.veg$Year==201904,]
result.veg.mean$cover19 = tapply(d.veg.19$QuadCover,INDEX = d.veg.19$SiteID,"mean")
result.veg.mean$height19 = tapply(d.veg.19$QuadCommunityHeight,INDEX = d.veg.19$SiteID,"mean")
result.veg.mean$richness19 = tapply(d.veg.19$QuadRichness,INDEX = d.veg.19$SiteID,"mean")

d.veg.18 <- d.veg[d.veg$Year==201808,]
result.veg.mean$cover18 = tapply(d.veg.18$QuadCover,INDEX = d.veg.18$SiteID,"mean")
result.veg.mean$height18 = tapply(d.veg.18$QuadCommunityHeight,INDEX = d.veg.18$SiteID,"mean")
result.veg.mean$richness18 = tapply(d.veg.18$QuadRichness,INDEX = d.veg.18$SiteID,"mean")

result.veg.mean <- result.veg.mean[,-1]
result.veg.mean
# standard error ----------------------------------------------------------
result.veg.se <- data.frame(matrix(rep(0, 1), nrow=length(levels(d.veg$SiteID))))
row.names(result.veg.se) <- levels(d.veg$SiteID)

d.veg.19 <- d.veg[d.veg$Year==201904,]
result.veg.se$cover19 = tapply(d.veg.19$QuadCover,INDEX = d.veg.19$SiteID,"sd")
result.veg.se$height19 = tapply(d.veg.19$QuadCommunityHeight,INDEX = d.veg.19$SiteID,"sd")
result.veg.se$richness19 = tapply(d.veg.19$QuadRichness,INDEX = d.veg.19$SiteID,"sd")

d.veg.18 <- d.veg[d.veg$Year==201808,]
result.veg.se$cover18 = tapply(d.veg.18$QuadCover,INDEX = d.veg.18$SiteID,"sd")
result.veg.se$height18 = tapply(d.veg.18$QuadCommunityHeight,INDEX = d.veg.18$SiteID,"sd")
result.veg.se$richness18 = tapply(d.veg.18$QuadRichness,INDEX = d.veg.18$SiteID,"sd")

result.veg.se <- result.veg.se[,-1]

result.veg.se <- result.veg.se / sqrt(9)
result.veg.se
# tapply(d.veg.19$QuadCover,INDEX = d.veg.19$SiteID,"length")[!is.na(tapply(d.veg.19$QuadCover,INDEX = d.veg.19$SiteID,"length"))]
# tapply(d.veg.18$QuadCover,INDEX = d.veg.18$SiteID,"length")[!is.na(tapply(d.veg.18$QuadCover,INDEX = d.veg.18$SiteID,"length"))]



# summary -----------------------------------------------------------------
# 両年
result.veg.mean
result.veg.se

# 2019年のみ
result.veg.mean[!is.na(result.veg.mean$cover19),1:3]
result.veg.se[!is.na(result.veg.se$cover19),1:3]
cbind(result.veg.mean[!is.na(result.veg.mean$cover19),1:3],result.veg.se[!is.na(result.veg.se$cover19),1:3])

# 2019年にデータが有るサイトのみ両年
result.veg.mean[!is.na(result.veg.mean$cover19),]
result.veg.se[!is.na(result.veg.se$cover19),]
cbind(result.veg.mean[!is.na(result.veg.mean$cover19),],result.veg.se[!is.na(result.veg.se$cover19),])
