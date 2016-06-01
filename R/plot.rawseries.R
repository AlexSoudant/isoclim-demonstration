plot.rawseries <-
function(iso1,iso2,iso3,sample.res)
{

# years lablels
y1 <- range(iso1[,2])[1]:range(iso1[,2])[2]
y2 <- range(iso2[,2])[1]:range(iso2[,2])[2]
y3 <- range(iso3[,2])[1]:range(iso3[,2])[2]

# fix the size of y-axis labels 
axis.labels.size <- 2

#Subfigure 1: parameters to display labels on y-axis 

# range of isotope values to determine max and min values of y-axis
iso1.range <- range(ceiling(max(iso1[,3])),floor(min(iso1[,3])))
# ticks to place the labels on the y-axis
iso1.at <- seq(from = iso1.range[1], to = iso1.range[2], by = 2 ) 
# labels of the y-axis
iso1.labels <- format(iso1.at , nsmall = 1)

#Subfigure 2: parameters to display labels on y-axis 

# range of isotope values to determine max and min values of y-axis
iso2.range <- range(ceiling(max(iso2[,3])),floor(min(iso2[,3])))
# ticks to place the labels on the y-axis
iso2.at <- seq(from = iso2.range[1], to = iso1.range[2], by = 2 )  

iso2.labels <- format(iso2.at , nsmall = 1)

#Subfigure 3: parameters to display labels on y-axis 

# range of isotope values to determine max and min values of y-axis
iso3.range <- range(ceiling(max(iso3[,3])),floor(min(iso3[,3])))
# ticks to place the labels on the y-axis
iso3.at <- seq(from = iso3.range[1], to = iso1.range[2], by = 2 )
# labels of the y-axis  
iso3.labels <- format(iso3.at , nsmall = 1)


##Subfigure 1: parameters to display labels on x-axis 

# object creation to contain ticks to place on x-axis
tick.axis1 <- matrix(NA,length(y1),1)

# attribution of tick values to place on x-axis
for(g in 1:length(y1)){
	tick.axis1[g] <- which(iso1[,1] == iso1[,1][iso1[,2]==y1[g]][1])
}

##Subfigure 2: parameters to display labels on x-axis 

# object creation to contain ticks to place on x-axis
tick.axis2 <- matrix(NA,length(y2),1)

# attribution of tick values to place on x-axis
for(g in 1:length(y2)){
	tick.axis2[g] <- which(iso2[,1] == iso2[,1][iso2[,2]==y2[g]][1])
}

##Subfigure 3: parameters to display labels on x-axis 

# object creation to contain ticks to place on x-axis
tick.axis3 <- matrix(NA,length(y3),1)

# attribution of tick values to place on x-axis
for(g in 1:length(y3)){
	tick.axis3[g] <- which(iso3[,1] == iso3[,1][iso3[,2]==y3[g]][1])
}



# Plotting
# plot creation
x11()
# creation of 3 subgraphics
par(mfrow=c(3,1))
# margin allocation
par(mar=c(2,2,2,2))
# outer margin allocation
par(oma=c(10,6,6,0))
par(xpd=FALSE)

# script object to contain the years to plot
label.year <- y3

# subfigure 1
plot(NA,ylim=iso1.range,xlim=range(0,length(iso1[,3])),xlab="", ylab=""
	,axes = F,col="black",cex=0.9,type="l",frame.plot=T)

# y-axis creation
axis(2,at = iso1.at ,labels = iso1.labels,las=3,cex.axis=axis.labels.size)
# plot isotope values as a continuous line
lines(iso1[,3],lwd=2)
# plot isotope values as filled points 
points(iso1[,3],pch = 21,bg="white",cex=1.5)
# x-axis creation
axis(1,at = tick.axis1,labels= rep("",length(y1)),las=3,cex.axis=2)
# horizontal dashed lines at y-axis ticks
abline(h=iso1.at,lty=3,col="dark grey",lwd=2  )
# plot tree ring width top axis
axis(3, at = seq( from = 0, to = length(iso1[,3])
	, by = length(iso1[,3])/(length(iso1[,3])*sample.res/1000)*2 )
	, labels = seq( from = 0, to = length(iso1[,3])*sample.res/1000
	, by = length(iso1[,3])/(length(iso1[,3])*sample.res/1000)*2*sample.res/1000 ),cex.axis=2)
# vertical dashed line to mark the end of the isotope time series
abline(v=length(iso1[,3]),lty =33)
# generalised linear model applied to the isotope values to determine trend
t1.hyy <- glm(iso1[,3]~iso1[,1])
# horizontal line representing the trend line in the time series 
abline(t1.hyy[1],t1.hyy[2],lty=5 ,col="black",lwd=2)

# plot y-axis title
 mtext(expression(paste({delta}^13*C,''[VPDB],' ',('\u2030'))),line= 4
	, side = 2,cex=2)
# plot top axis title
 mtext("Tree ring width increment (mm)",line= 4, side = 3,cex=2)
# draw a box around the subfigure
box(which = "plot", lty = "solid", col="black")


# subfigure 2
par(xpd=FALSE)
plot(NA,ylim=iso2.range,xlim=range(0,length(iso2[,3])),xlab="", ylab=""
	,axes = F,col="black",cex=0.9,type="l",frame.plot=T)

# y-axis creation
axis(2,at = iso2.at,labels =iso2.labels,las=3,cex.axis=axis.labels.size)
# plot isotope values as a continuous line
lines(iso2[,3],lwd=2)
# plot isotope values as filled points 
points(iso2[,3],pch = 21,bg="white",cex=1.5)
# x-axis creation
axis(1,at = tick.axis2,labels= rep("",length(y2)),las=3,cex.axis=2)
# horizontal dashed lines at y-axis ticks
abline(h=iso2.at,lty=3,col="dark grey",lwd=2  )
# plot tree ring width top axis
axis(3, at = seq( from = 0, to = length(iso2[,3])
	, by = length(iso2[,3])/(length(iso2[,3])*sample.res/1000)*2 )
	, labels = seq( from = 0, to = length(iso2[,3])*sample.res/1000
	, by = length(iso2[,3])/(length(iso2[,3])*sample.res/1000)*2*sample.res/1000 ),cex.axis=2)
# vertical dashed line to mark the end of the isotope time series
abline(v=length(iso2[,3]),lty =33)
# generalised linear model applied to the isotope values to determine trend
t2.hyy <- glm(iso2[,3]~iso2[,1])
# horizontal line representing the trend line in the time series 
abline(t2.hyy[1],t2.hyy[2],lty=5 ,col="black",lwd=2)
# plot y-axis title
mtext(expression(paste({delta}^13*C,''[VPDB],' ',('\u2030'))),line= 4
	, side = 2,cex=2)
# draw a box around the subfigure
box(which = "plot", lty = "solid", col="black")

# subfigure 3
par(xpd=FALSE)
plot(NA,ylim=iso3.range,xlim=range(0,length(iso3[,3])),xlab="", ylab=""
	,axes = F,col="black",cex=0.9,type="l",frame.plot=T)
# y-axis creation
axis(2,at = iso3.at,labels =iso3.labels,las=3,cex.axis=axis.labels.size)
# plot isotope values as a continuous line
lines(iso3[,3],lwd=2)
# plot isotope values as filled points 
points(iso3[,3],pch = 21,bg="white",cex=1.5)
# x-axis creation
axis(1,at = tick.axis3,labels =y3,las=3,cex.axis=2)
# vertical dashed line to mark the end of the isotope time series
abline(h=iso3.at,lty=3,col="dark grey" ,lwd=2 )
# plot tree ring width top axis
axis(3, at = seq( from = 0, to = length(iso3[,3])
	, by = length(iso3[,3])/(length(iso3[,3])*sample.res/1000)*2 )
	, labels = seq( from = 0, to = length(iso3[,3])*sample.res/1000
	, by = length(iso3[,3])/(length(iso3[,3])*sample.res/1000)*2*sample.res/1000 ),cex.axis=2)
# vertical dashed line to mark the end of the isotope time series
abline(v=length(iso3[,3]),lty =33)
# generalised linear model applied to the isotope values to determine trend
t3.hyy <- glm(iso3[,3]~iso3[,1])
# horizontal line representing the trend line in the time series 
abline(t3.hyy[1],t3.hyy[2],lty=5 ,col="black",lwd=2)
# plot y-axis title
mtext(expression(paste({delta}^13*C,''[VPDB],' ',('\u2030'))),line= 4
	, side = 2,cex=2)

# draw a box around the subfigure
box(which = "plot", lty = "solid", col="black")

setwd(.DATA)
savePlot(file = paste("rawdata_presentation",site,".png",sep=""))

}
