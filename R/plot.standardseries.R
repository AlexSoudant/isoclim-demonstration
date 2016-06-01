plot.standardseries <-
function(y,isotope.data,mean.isotope,cor.iso,EPS,sample.res)
{

# object containing ticks to place on x-axis
tick.axis1 <- matrix(NA,length(y),1)

# attribution of tick values to place on x-axis
for(g in 1:length(y)){
	tick.axis1[g] <- isotope.data[,1][isotope.data[,2]==y[g]][1]
}

# object containing the ticks used for the x-axis
scale.year <- tick.axis1
# object containing the labels for the x-axis
label.year <- y


# range of isotope values to determine max and min values of y-axis
iso.range <- range(ceiling(max(c(isotope.data[,3],isotope.data[,4],isotope.data[,5]))),floor(min(c(isotope.data[,3],isotope.data[,4],isotope.data[,5]))))
# ticks to place the labels on the y-axis
iso.at <- seq(from = iso.range[1], to = iso.range[2]+2, by = 2 ) 
# labels of the y-axis
iso.labels <- format(iso.at , nsmall = 1)

# New plot creation
x11()
# Setting outer margins
par(oma=c(8,2,6,0))

# Creation of two subfigures
par(mfrow=c(2,1))
par(xpd=FALSE)
# Setting the inner margins
par(mar=c(2,4,0,2))

#----------------- Plotting first subfigure ------------------------------------

plot(NA,ylim=range(iso.range[1],iso.range[2]+2),xlim=range(0,length(isotope.data[,1])),xlab="", ylab="",axes = F,col="black",cex=0.9,type="l",frame.plot=T)

# Drawing reading horizontal lines for y-axis
abline(h=iso.at,lty=2,col="dark grey",lwd=2 )

# Setting values for y-axis
axis(2,at = iso.at,labels =iso.labels,las=3,cex.axis=1.5)

# Drawing raw time serie 1
lines(isotope.data[,3],col="grey30",lwd=3,type="l",cex=1)
# Drawing raw time serie 2
lines(isotope.data[,4],col="grey50",lwd=3,type="l",cex=1)
# Drawing raw time serie 3
lines(isotope.data[,5],col="grey70",lwd=3,type="l",cex=1)

# Writting y-axis label
mtext(expression({delta}^13*C~''[VPDB]~('\u2030')),line= 3, side = 2,cex=2)

# Setting top axis for Ring width
axis(3, at = seq( from = 0, to = length(isotope.data[,3])+1, by = 20)  , tick=TRUE,labels = seq( from = 0, to = (length(isotope.data[,3])+1)*sample.res/1000, by = 20*sample.res/1000),cex.axis=2)

par(xpd=TRUE)
# Writting title of the top axis
mtext("radial increment of rings (mm)",line= 4, side = 3,cex=2)

# Writing the legend of the subfigure
legend("top",c("Tree 1","Tree 2","Tree 3") ,lty=c(1,1,1),cex=2,lwd=c(4,4,4),pch=c(NA,NA,NA),col=c("grey30","grey50","grey70"), bty = "n",horiz = TRUE)

# Writing the EPS value topright
legend("topright", inset=c(0.0,0),paste("EPS =" ,round(EPS,2)) , bty = "n",cex=2)

# Drawing a solid box around the subfigure 
box(which = "plot", lty = "solid", col="black")

#----------------- Plotting second subfigure -----------------------------------

par(xpd=FALSE)
plot(NA,ylim=range(iso.range[1],iso.range[2]+2),xlim=range(0,length(isotope.data[,1])),xlab="", ylab="",axes = F,frame.plot=T)

# Drawing reading horizontal lines for y-axis
abline(h=iso.at,lty=2,col="dark grey",lwd=2 )


# Drawing the atmospheric corrected time series 
lines(cor.iso[,3],col="black",lwd=4,type="l",cex=0.9)

# Drawing the mean time series
lines(mean.isotope[,3],col="black",lwd=3,type="l",cex=0.9,lty=2)

# Setting values for x-axis
axis(1,at = c(tick.axis1),labels = y, las= 3,cex.axis=2,tick=TRUE)

# Setting values for y-axis
axis(2,at = iso.at,labels =iso.labels,las=3,cex.axis=1.5)

# Writing the legend of the subfigure
legend("top",c("Atm non corrected","Atm corrected") ,lty=c(2,1),cex=2,lwd=c(3,4),pch=c(NA,NA),col=c("black","black"), bty = "n",horiz = TRUE)

# Writting y-axis label
mtext(expression({delta}^13*C~''[VPDB]~('\u2030')),line=3, side = 2,cex=2)


# Drawing a solid box around the subfigure 
box(which = "plot", lty = "solid", col="black")

setwd(.DATA)
savePlot(file = paste("stddata_presentation",site,".png",sep=""))

}
