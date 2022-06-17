sc <- read.csv("Cover.csv",row.names=1)

sc.d <- rbind(sc[1,],sc["Salsola passerina",],sc["Anabasis brevifolia",],sc["Kalidium gracile",],sc["Nitraria sibirica",])

sd.d <- sd[5:8,]

dom <- t(rbind(sc.d,sd.d))


#sa
dom.sa <- dom[dom[,1]==1,]
dom.sa <- dom.sa[,2:ncol(dom.sa)]
dom.sa <- cbind(dom.sa[1:2],dom.sa[5:7])
dom.sa <- as.data.frame(dom.sa)
pairs(dom.sa)
dom.sadlm1 <- lm(dom.sa[,1] ~ Horse + Sheep + Camel,dom.sa)
summary(dom.sadlm1)
dom.sadlm2 <- lm(dom.sa[,2] ~ Horse + Sheep + Camel,dom.sa)
summary(dom.sadlm2)


#kn
dom.km <- dom[dom[,1]==0,]
dom.km <- dom.km[,2:ncol(dom.km)]
dom.km <- dom.km[,3:7]
dom.km <- as.data.frame(dom.km)
pairs(dom.km)
dom.kmdlm1 <- lm(dom.km[,1] ~ Horse + Sheep + Camel,dom.km)
summary(dom.kmdlm1)
dom.kmdlm2 <- lm(dom.km[,2] ~ Horse + Sheep + Camel,dom.km)
summary(dom.kmdlm2)





