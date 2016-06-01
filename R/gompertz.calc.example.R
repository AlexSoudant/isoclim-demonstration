gompertz.calc.example <-
function() 
{

year <-  2007

setwd("/home/alex/Rprojects/Paper1/Derived data sets/hyytiala")
load(paste("gompertzequation_y.eq1",year,".rda",sep=""))

y.eq1.norm <- matrix(NA,length(y.eq1),1)

for(i in 1:length(y.eq1)) {
y.eq1.norm[i] <- y.eq1[i]*100/max(y.eq1,na.rm=TRUE)
}

x11()
 par(xpd=FALSE)
par(oma=c(2,4,0,0))
plot(NA, axes=F,ylim=range(-0,110),xlim=range(0,365),ylab="",xlab="",frame.plot=T)


  ActualYMin <- par("usr")[3] 
  ActualYMax <- par("usr")[4]



rect(xleft = 145, ybottom = ActualYMin, 145+40, ActualYMax, density = NULL, angle = 45,col = "gray95", border = NA)
rect(xleft = 145+40, ybottom = ActualYMin, 145+40+40, ActualYMax, density = NULL, angle = 45,col = "gray80", border = NA)
rect(xleft = 145+40+40, ybottom = ActualYMin, 265, ActualYMax, density = NULL, angle = 45,col = "gray66", border = NA)


text(165, 110, "Early",cex = 2)
text(205, 110, "Mid",cex = 2)
text(245, 110, "Late",cex = 2)


scale.month <- c(1,32,60,91,121,152,182,213,244,274,305,335,365)
label.month <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","")

axis(3,at=scale.month,labels=rep(NA,length(scale.month)),tick=TRUE)


gompertz.enlargement <- c(1:161+23,162:167+21,168:171+19,172:175+17,176:180+15,181:184+13,185:190+11,191:197+9,198:209+7,210:365+5)
gompertz.maturation <- c(1:161+23+15,162:167+21+19,168:171+19+23,172:175+17+27,176:180+15+31,181:184+13+35,185:190+11+39,191:197+9+43,198:209+7+47,210:365+5+51)


lines(smooth.spline(gompertz.enlargement,spar=0.4)$y,y.eq1.norm,col="black",lty=2,lwd=4)
lines(smooth.spline(gompertz.maturation,spar=0.4)$y,y.eq1.norm,col="black",lty=3,lwd=4)

lines(y.eq1.norm ,col="black",lwd=4)


 par(xpd=TRUE)
 mtext("relative wood growth (%)",line= 5, side = 2,cex=2)

 mtext("days of the year",line= 5, side = 1,cex=2)

axis(3,at = scale.month+15,labels = label.month, las= 1,cex.axis=1.5, tick = FALSE)

axis(2,at = c(seq(from=0,100,by=10)),labels = c(seq(from=0,100,by=10)), las= 1,cex.axis=2)


axis(1,at=seq(from=5,365,by=40)
		,labels=seq(from=5,365,by=40)
		,line=1,col="black",cex.axis=2,tick=FALSE)

axis(1,at=seq(from=5,365,by=40)
		,labels=rep(NA,length(seq(from=5,365,by=40)))
		,line=0,col="black",cex.axis=2,tick=TRUE)

rect(xleft = 365, ybottom = ActualYMin, 380, ActualYMax, density = NULL, angle = 45,col = "white", border = NA)


box(which = "plot", lty = "solid", col="black")

legend(1,110, c(expression("cell division","cell enlargement","cell maturation")), col = c("black","black","black"), pch = c(NA,NA) ,lty= c(1,2,3),lwd = c(5,5,7),bg="white",cex=2)

setwd(.DATA)
savePlot(file = paste("gompertz_presentation",site,".png",sep=""))


setwd(.DATA)
load("final_isotope.rda")

final.isotope <<-final.isotope
}
