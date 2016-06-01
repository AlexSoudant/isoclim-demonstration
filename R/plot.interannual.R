plot.interannual <-
function(y,annual.dataset)
{

x11()
# Storage annual isotope values into a specific script object
mean.iso <- annual.dataset

# Graphic parameters for setting axis scales
isotope.graphicscale.max<- round(max(mean.iso[,2])+3,0)
isotope.graphicscale.min <- round(min(mean.iso[,2])-1,0)

temperature.graphicscale.max <-  round(max(mean.iso[,4])+3,0) 
temperature.graphicscale.min <- round(min(mean.iso[,4])-1,0)

PAR.graphicscale.max <-  round(max(mean.iso[,5])+50,0) 
PAR.graphicscale.min <- round(min(mean.iso[,5])-50,0)

precip.graphicscale.max <-  round(max(mean.iso[,6])+1000,0) 
precip.graphicscale.min <- round(min(mean.iso[,6])-100,0)

# storage number of years and labels for plotting
scale.year <- 1:length(y)
label.year <- y


# Plotting
 par(xpd=FALSE)
# Setting 4 subfigures
par(mfrow=c(2,2))
# Margins
par(mar=c(4,8,2,2))
par(oma=c(6,2,0,0))

# Open new plot area
plot(NA,xlab="", ylab="",axes = F,frame.plot=T
		,ylim=range(isotope.graphicscale.min,isotope.graphicscale.max)
		,xlim=range(temperature.graphicscale.min,temperature.graphicscale.max))

# Draw grey dashed lines for y-axis and x-axis  
abline(v=seq(temperature.graphicscale.min, temperature.graphicscale.max ,by = 2),col= "grey60",lwd = 2,lty =2)
abline(h=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),col= "grey60",lwd = 2,lty =2)

# Plot points of the relationship isotope vs temperature
points(mean.iso[,4],mean.iso[,2], cex = 2,pch=21,bg = "black")

# x-axis 
axis(1,at=seq(temperature.graphicscale.min, temperature.graphicscale.max ,by = 2)
		,labels=seq(temperature.graphicscale.min, temperature.graphicscale.max ,by = 2)
		,line=1,col="black",cex.axis=2,tick=FALSE)
# x-axis ticks
axis(1,at=seq(temperature.graphicscale.min, temperature.graphicscale.max ,by = 2)
		,labels=rep(NA,length(seq(temperature.graphicscale.min, temperature.graphicscale.max ,by = 2)))
		,line=0,col="black",cex.axis=2,tick=TRUE)

# y-axis
axis(2,at=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),labels=format(seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2) , nsmall = 1) , line=0,col="black",las=3,cex.axis=2) 

# generalised linear model
glm.model<- glm(mean.iso[,2]~mean.iso[,4])
# Draw the regression line
abline(glm.model[1],glm.model[2],lwd=3,col="black")
# Storage glm properties
glm.sum<- summary.lm(glm.model)

# Add legend with R squared results
legend(temperature.graphicscale.min,isotope.graphicscale.max,paste("Rsq"," temp"," =" ,round(glm.sum$adj.r.squared,3)),lty=c(1),cex=2,lwd=c(4),bty="n")

par(xpd=TRUE)
# y-axis label
mtext(expression(paste({delta}^13*C,''[VPDB],' ',('\u2030'))),line= 6, side = 2,cex=2)
# x-axis label
mtext(expression('Temperature'~({}^o*C)),line= 4.5, side = 1,cex=2)

# Draw a box around subfigure
box(which = "plot", lty = "solid", col="black")

#PAR

par(xpd=FALSE)
# Open new plot area
plot(NA,xlab="", ylab="",axes = F,frame.plot=T
		,ylim=range(isotope.graphicscale.min,isotope.graphicscale.max)
		,xlim=range(PAR.graphicscale.min,PAR.graphicscale.max))

# Draw grey dashed lines for y-axis and x-axis 
abline(v=seq(round(PAR.graphicscale.min,-2), round(PAR.graphicscale.max,-2) ,by = 50),col= "grey60",lwd = 2,lty =2)
abline(h=seq(isotope.graphicscale.min,isotope.graphicscale.max, by = 2),col= "grey60",lwd = 2,lty =2)

# Plot points of the relationship isotope vs PAR
points(mean.iso[,5],mean.iso[,2], cex = 2,pch=21,bg = "black")

# x-axis 
axis(1,at=seq(round(PAR.graphicscale.min,-2), round(PAR.graphicscale.max,-2) ,by = 50)
		,labels=seq(round(PAR.graphicscale.min,-2),round(PAR.graphicscale.max,-2) ,by = 50)
		,line=1,col="black",cex.axis=2,tick=FALSE)
# x-axis ticks
axis(1,at=seq(round(PAR.graphicscale.min,-2), round(PAR.graphicscale.max,-2) ,by = 50)
		,labels=rep(NA,length(seq(round(PAR.graphicscale.min,2), round(PAR.graphicscale.max,-2) ,by = 50)))
		,line=0,col="black",cex.axis=2,tick=TRUE)

# y-axis
axis(2,at=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),labels=format(seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2), nsmall = 1), line=0,col="black",las=3,cex.axis=2) 

# generalised linear model
glm.model<- glm(mean.iso[,2]~mean.iso[,5])
# Draw the regression line
abline(glm.model[1],glm.model[2],lwd=3,col="black")
# Storage glm properties
glm.sum<- summary.lm(glm.model) 

# Add legend with R squared results
legend(PAR.graphicscale.min,isotope.graphicscale.max,paste("Rsq"," PAR = ",round(glm.sum$adj.r.squared,3)),lty=1,cex=2,lwd=4,bty="n")

par(xpd=TRUE)
# y-axis label
mtext(expression(paste({delta}^13*C,''[VPDB],' ',('\u2030'))),line= 6, side = 2,cex=2)
# x-axis label
mtext(expression('PAR'~({mu}*'mol'~{m}^{-2}~{s}^{-1})),line= 5, side = 1,cex=2)
# Draw a box around subfigure
box(which = "plot", lty = "solid", col="black")


#Precipitation

par(xpd=FALSE)
# Open new plot area
plot(NA,xlab="", ylab="",axes = F,frame.plot=T
		,ylim=range(isotope.graphicscale.min,isotope.graphicscale.max)
		,xlim=range(precip.graphicscale.min,precip.graphicscale.max))

# Draw grey dashed lines for y-axis and x-axis
abline(v=seq(precip.graphicscale.min, precip.graphicscale.max ,by = 1000),col= "grey60",lwd = 2,lty =2)
abline(h=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),col= "grey60",lwd = 2,lty =2)

# Plot points of the relationship isotope vs precipitation
points(mean.iso[,6],mean.iso[,2], cex = 2,pch=21,bg = "black")

# x-axis 
axis(1,at=seq(round(precip.graphicscale.min,-2), round(precip.graphicscale.max,-2) ,by = 500)
		,labels=seq(round(precip.graphicscale.min,-2), round(precip.graphicscale.max,-2) ,by = 500)
		,line=1,col="black",cex.axis=2,tick=FALSE)
# x-axis ticks
axis(1,at=seq(round(precip.graphicscale.min,-2), round(precip.graphicscale.max,-2) ,by = 500)
		,labels=rep(NA,length(seq(round(precip.graphicscale.min,-2), round(precip.graphicscale.max,-2) ,by = 500)))
		,line=0,col="black",cex.axis=2,tick=TRUE)

# y-axis
axis(2,at=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),labels=format(seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2) , nsmall = 1), line=0,col="black",las=3,cex.axis=2)

# generalised linear model
glm.model<- glm(mean.iso[,2]~mean.iso[,6])
# Draw the regression line
abline(glm.model[1],glm.model[2],lty=1,lwd=3,col="black")
# Storage glm properties
glm.sum<- summary.lm(glm.model)

# Add legend with R squared results
legend(round(precip.graphicscale.min,-2),isotope.graphicscale.max,paste("Rsq"," precip = ",round(glm.sum$adj.r.squared,3)),lty=1,cex=2,lwd=4,bty="n")


par(xpd=TRUE)
# y-axis label
 mtext(expression(paste({delta}^13*C,''[VPDB],' ',('\u2030'))),line= 5, side = 2,cex=2)
# x-axis label
mtext("Precipitation (mm)",line= 4, side = 1,cex=2)
# Draw a box around subfigure
box(which = "plot", lty = "solid", col="black")

#Model

# Setting Multiple linear regression model
tot.model <- glm(mean.iso[,2]~mean.iso[,4]+mean.iso[,5]+mean.iso[,6])
# Setting regression between measured values and modelled values
glm.model<- glm(mean.iso[,2]~tot.model$fitted)
# Storage glm properties
glm.sum<- summary.lm(glm.model)

par(xpd=FALSE)
# Open new plot area
plot(tot.model$fitted,mean.iso[,2],xlab="", ylab="",axes = F,frame.plot=T
		,ylim=range(isotope.graphicscale.min,isotope.graphicscale.max)
		,xlim=range(isotope.graphicscale.min,isotope.graphicscale.max), cex = 2,pch=21,bg = "black")

# Draw grey dashed lines for y-axis and x-axis
abline(v=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),col= "grey60",lwd = 2,lty =2)
abline(h=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),col= "grey60",lwd = 2,lty =2)

# Plot points of the relationship measured isotope vs modelled isotopes
points(tot.model$fitted,mean.iso[,2], cex = 2,pch=21,bg = "black")

# Draw the regression line
abline(glm.model[1],glm.model[2],lwd=3,col="black")

# x-axis 
 axis(1,at=seq(isotope.graphicscale.min,isotope.graphicscale.max, by = 2),labels=format(seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2) , nsmall = 1) , line=1,col="black",cex.axis=2,tick=FALSE)
# x-axis ticks
 axis(1,at=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),labels=rep(NA,length(format(seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2) , nsmall = 1))) , line=0,col="black",cex.axis=2,tick=TRUE)

# y-axis
axis(2,at=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),labels=format(seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2) , nsmall = 1) , line=0,col="black",las=3,cex.axis=2) 

# Add legend with R squared results
legend(isotope.graphicscale.min,isotope.graphicscale.max,paste("Rsq"," model = ",round(glm.sum$adj.r.squared,3)),lty=1,cex=2,lwd=4,bty="n")

# x-axis and y-axis labels
par(xpd=TRUE)
 mtext(expression(paste({delta}^13*C ,' '[VPDB],' ',' ',meas,' '('\u2030'))),line= 5, side = 2,cex=2)
 mtext(expression(paste({delta}^13*C ,' '[VPDB],' ',' ',model,' '('\u2030'))),line= 4.5, side = 1,cex=2)

# Draw a box around subfigure
box(which = "plot", lty = "solid", col="black")

setwd(.DATA)
#dev.copy2pdf(file = paste("LR_inter_JuneFALSE",site,".pdf",sep=""),   encoding="WinAnsi.enc")
savePlot(file = paste("inter_presentation",site,".png",sep=""))

}
