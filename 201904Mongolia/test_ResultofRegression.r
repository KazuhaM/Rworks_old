# c ~ cover * \bar{D}/h ---------------------------------------------------
# c = 3.45 - (0.29 + 0.01 \bar{D}/h)* cover - 0.04 \bar{D}/h
# 中心化をしているので、平均値が必要
# mean(d3$rh): 73.12324
# mean(d3$QuadCover): 23.62778

c_cvdh <- function (cv,dh,mcv,mdh) {
  coc = NULL
  for (i_cv in cv) {
    for (i_dh in dh) {
      temp.cv <- i_cv - mcv
      temp.dh <- i_dh - mdh
      
      temp.coc <- 3.45 - (0.29 + 0.01 * temp.dh)* temp.cv - 0.04 * temp.dh
      temp.coc <- 3.45 - 0.29 * temp.cv - (0.04 + 0.01 * temp.cv)* temp.dh
      coc = rbind(coc,c(i_cv,temp.coc,i_dh))
    }
  }
  coc <- data.frame("cover"=coc[,1],"c" = coc[,2],"dh" = coc[,3])
  return(coc)
}
cv = seq(0,50,by = 1)
dh = seq(1,170,by = 1)
m.dh = 73.12324
m.cv = 23.62778
result.coc <- c_cvdh(cv, dh,m.cv,m.dh)
library(RColorBrewer) #グラフ色用
plot(c~cover,data =result.coc,
     ylim = c(0,15),cex.lab = 1.5, cex.axis = 1.2,
     col = brewer.pal(10,"Spectral")[9 - round(result.coc$dh/20,0)],pch=21)
points(19.6,4.61,pch = 16 , cex   =1.2)
text(20,5,"(19.6, 4.61)")



# λ~cover * d/h -----------------------------------------------------------

lam_cvdh <- function (cv,dh,mcv,mdh) {
  lam = NULL
  for (i_cv in cv) {
    for (i_dh in dh) {
      temp.cv <- i_cv - mcv
      temp.dh <- i_dh - mdh
      
      temp.lam <- 0.05937 - (0.00351 + 0.00012 * temp.dh)* temp.cv - 0.00072 * temp.dh
      lam = rbind(lam,c(i_cv,temp.lam,i_dh))
    }
  }
  lam <- data.frame("cover"=lam[,1],"lambda" = lam[,2],"dh" = lam[,3])
  return(lam)
}
cv = seq(0,50,by = 1)
dh = seq(1,170,by = 1)
m.dh = 73.12324
m.cv = 23.62778
result.lam <- lam_cvdh(cv, dh,m.cv,m.dh)
library(RColorBrewer) #グラフ色用
plot(lambda~cover,data =result.lam,
     ylim = c(0,0.15),cex.lab = 1.5, cex.axis = 1.2,
     col = brewer.pal(10,"Spectral")[9 - round(result.coc$dh/20,0)],pch=21)
points(17.6,0.0804,pch = 16 , cex   =1.2)
text(17,0.1,"(17.6, 0.0804)")


# flambda ~ lambda --------------------------------------------------------

cul.flambda <- function(lambda,mr, sigr, betar){
  y <- sqrt(1 - mr * sigr * lambda) * 
    sqrt(1 + mr * betar * lambda)
  print(y)
}

par.mr <- 0.5
par.sigr <- 1.0
par.betar <- 200
lam <- seq(0.001,0.2,by = 0.001)
result.flambda <- cul.flambda(lam, par.mr, par.sigr, par.betar)
result.flambda <- data.frame("lambda" = lam, "f_lambda" = result.flambda)
plot(f_lambda ~ lambda , data= result.flambda,
     ylim = c(0,max(result.flambda$f_lambda)),cex.lab = 1.5, cex.axis = 1.2)
