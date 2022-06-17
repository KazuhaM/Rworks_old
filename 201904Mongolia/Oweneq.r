

t.ust <- seq(0.3,0.7,by=0.2)
t.ust <- 0.5
t.c <- seq(10^-4,8*10^-4,by=2*10^-4)
t.c <- 10^-4
ymax <- 10^-1

for(i in 1:length(t.ust)){
for(j in 1:length(t.c)){
  t.u <- seq(t.ust[i],1.2,by=0.01)
  if(!(i == 1 & j==1)){
    par(new =T)
  }
  owen.plot(c = t.c[j], ust = t.ust[i], us = t.u,
            xM=1.2,yM =ymax,lcol =j,llty =i)
  if(i == length(t.ust)){
    par(new =F)
  }
}
}
legend(0,ymax,title = "ust",t.ust,col = 1,lty=1:length(t.c))
legend(0,ymax*0.8,title = expression(italic(c)),t.c,col = 1:length(t.c),lty=1,cex=1.3)



owen.plot <- function(c,ust,us,xM =1.2 ,yM = 1000,lcol=1,llty = 1){
  # cul sf
  sf <- c * 1293 * (1-(ust / us)^2)*us^3 / 9.8
  # g m-2 s-1 = c(-m) * rho(g m-3) * u(m3 s-3) /g(m s-2)
  # plot
  old.par <- par("mar")
  par(mar = c(5,7,3,1),family = family_serif)
  plot(sf~ us,type = "l",xlim = c(0,xM),ylim = c(0,yM),
       xlab = expression(paste(italic(u)["*"],sep="")),
       ylab = expression(paste("saltation flux (g ",m^{-2},s^{-1},")",sep="")),
       cex.lab = 2,cex.axis = 1.7,col=lcol,lty = llty,lwd = 2)
  # segments(ust,0,ust,10,col=lcol)
  par(mar = old.par)
  # return(sf)
}
