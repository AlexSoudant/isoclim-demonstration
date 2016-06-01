plot.intraannual <-
function(y,climate.iso,data.early,data.mid,data.late)
{

x11()
# Setting graphical parameters to determine y-axis ans x-axis scales
isotope.graphicscale.max<- round(max(climate.iso[,2])+2,0)
isotope.graphicscale.min <- round(min(climate.iso[,2])-1,0)

temperature.graphicscale.max<- round(max(climate.iso[,4])+5)
temperature.graphicscale.min <- 0

PAR.graphicscale.max<- round(max(climate.iso[,5])+100)
PAR.graphicscale.min <- 0

precipitation.graphicscale.max <- round(max(climate.iso[,6])+100)
precipitation.graphicscale.min <- 0


#__________________________________________________________________Program

# Temperature

# Parameter value to plot temperature
i <- 4

# Set layout with margins and outer margins
 par(xpd=FALSE)
par(mfrow=c(2,2))
par(mar=c(4,8,2,2))
par(oma=c(6,2,0,0))


# Open new plot with data.late values
plot(data.late[,i],data.late[,2],xlab="", ylab="",axes = F,frame.plot=T
		,ylim=range(isotope.graphicscale.min ,isotope.graphicscale.max)
		,xlim=range(temperature.graphicscale.min,temperature.graphicscale.max), cex = 2,pch=3)

# Draw grey dashed lines 
abline(v=seq(0, temperature.graphicscale.max ,by = 5),col= "grey60",lwd = 2,lty =2)
abline(h=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),col= "grey60",lwd = 2,lty =2)

# Plot points for early and mid values
points(data.early[,i],data.early[,2],pch=1, cex = 2)
points(data.mid[,i],data.mid[,2],pch=2, cex = 2)

# Set x-axis values
axis(1,at=seq(0, temperature.graphicscale.max ,by = 5)
		,labels=seq(0, temperature.graphicscale.max ,by = 5)
		,line=1,col="black",cex.axis=2,tick=FALSE)
# Set x-axis ticks
axis(1,at=seq(0, temperature.graphicscale.max ,by = 5)
		,labels=rep(NA,length(seq(0, temperature.graphicscale.max ,by = 5)))
		,line=0,col="black",cex.axis=2,tick=TRUE)

# Set y-axis
axis(2,at=seq(isotope.graphicscale.min,isotope.graphicscale.max, by = 2),labels=format(seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2) , nsmall = 1) , line=0,col="black",las=3,cex.axis=2) 
   
# Calculate the generalised linear model for early values 
glm.temperature.early<- glm(data.early[,2]~data.early[,i])
# Draw the linear regression fit for early values 
abline(glm.temperature.early[1],glm.temperature.early[2],lty=2,lwd=4)

# Calculate the generalised linear model for mid values
glm.temperature.mid<- glm(data.mid[,2]~data.mid[,i])
# Draw the linear regression fit for mid values 
abline(glm.temperature.mid[1],glm.temperature.mid[2],lty=3,lwd=4)

# Calculate the generalised linear model for late values
glm.temperature.late <- glm(data.late[,2]~data.late[,i])
# Draw the linear regression fit for late values 
abline(glm.temperature.late[1],glm.temperature.late[2],lty=4,lwd=4)

# Calculate the generalised linear model for total values
glm.temperature.total<- glm(climate.iso[,2]~climate.iso[,i])
# Draw the linear regression fit for total values 
abline(glm.temperature.total[1],glm.temperature.total[2],lty=1,lwd=4)

# Draw the legend area as a white rectangle  
rect(temperature.graphicscale.min-((temperature.graphicscale.max-temperature.graphicscale.min)*0.025),mean(c(isotope.graphicscale.min ,isotope.graphicscale.max))+0.5,temperature.graphicscale.min+((temperature.graphicscale.max-temperature.graphicscale.min)*0.4),isotope.graphicscale.max,col="white", border = NULL)

# Display the legend
legend(temperature.graphicscale.min+0.5,isotope.graphicscale.max,c("early","mid","late","total"),lty=c(2,3,4,1),cex=1.5,lwd=c(4,4,4,4),bty="n")
legend(temperature.graphicscale.min-0.5,isotope.graphicscale.max,c(NA,NA,NA),pch=c(1,2,3),cex=1.5,bty="n")
legend(mean(c(temperature.graphicscale.min,temperature.graphicscale.max))-2,isotope.graphicscale.max,paste("Rsq"," temp"," =" ,round(summary.lm(glm.temperature.total)$adj.r.squared,3)),cex=1.5,bty="n")


legend(temperature.graphicscale.min-0.5,isotope.graphicscale.max,c(NA,NA,NA),pch=c(1,2,3),cex=1.5,bty="n")


# Display y-axis label
par(xpd=TRUE)
 mtext(expression(paste({delta}^13*C,''[VPDB],' ',('\u2030'))),line= 5, side = 2,cex=2)

# Display x-axis label
if(i == 4){mtext(expression('Temperature'~({}^o*C)),line= 4.5, side = 1,cex=2)}

# Draw a box around the subfigure
box(which = "plot", lty = "solid", col="black")


#PAR
par(xpd=FALSE)
# Parameter value to plot PAR
i <- 5

# Open new plot with data.late values
plot(data.late[,i],data.late[,2],xlab="", ylab="",axes = F,frame.plot=T
		,ylim=range(isotope.graphicscale.min,isotope.graphicscale.max)
		,xlim=range(PAR.graphicscale.min,PAR.graphicscale.max), cex = 2,pch=3)

# Draw grey dashed lines 
abline(v=seq(PAR.graphicscale.min, PAR.graphicscale.max ,by = 200),col= "grey60",lwd = 2,lty =2)
abline(h=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),col= "grey60",lwd = 2,lty =2)

# Plot points for early and mid values
points(data.early[,i],data.early[,2],pch=1, cex = 2)
points(data.mid[,i],data.mid[,2],pch=2, cex = 2)

# Set x-axis values
axis(1,at=seq(PAR.graphicscale.min, PAR.graphicscale.max ,by = 200)
		,labels=seq(PAR.graphicscale.min, PAR.graphicscale.max ,by = 200)
		,line=1,col="black",cex.axis=2,tick=FALSE)
# Set x-axis ticks
axis(1,at=seq(PAR.graphicscale.min, PAR.graphicscale.max ,by = 200)
		,labels=rep(NA,length(seq(PAR.graphicscale.min, PAR.graphicscale.max ,by = 200)))
		,line=0,col="black",cex.axis=2,tick=TRUE)

# Set y-axis
axis(2,at=seq(isotope.graphicscale.min,isotope.graphicscale.max, by = 2),labels=format(seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2) , nsmall = 1) , line=0,col="black",las=3,cex.axis=2) 

# Calculate the generalised linear model for early values 
glm.PAR.early <- glm(data.early[,2]~data.early[,i])
# Draw the linear regression fit for early values 
abline(glm.PAR.early[1],glm.PAR.early[2],lty=2,lwd=4)

# Calculate the generalised linear model for mid values 
glm.PAR.mid <- glm(data.mid[,2]~data.mid[,i])
# Draw the linear regression fit for mid values 
abline(glm.PAR.mid[1],glm.PAR.mid[2],lty=3,lwd=4)

# Calculate the generalised linear model for late values 
glm.PAR.late<- glm(data.late[,2]~data.late[,i])
# Draw the linear regression fit for late values 
abline(glm.PAR.late[1],glm.PAR.late[2],lty=4,lwd=4)

# Calculate the generalised linear model for total values 
glm.PAR.total <- glm(climate.iso[,2]~climate.iso[,i])
# Draw the linear regression fit for total values 
abline(glm.PAR.total[1],glm.PAR.total[2],lty=1,lwd=4)

# Draw the legend area as a white rectangle  
rect(PAR.graphicscale.min-((PAR.graphicscale.max-PAR.graphicscale.min)*0.025),mean(c(isotope.graphicscale.min ,isotope.graphicscale.max))+0.5,PAR.graphicscale.min+((PAR.graphicscale.max-PAR.graphicscale.min)*0.4),isotope.graphicscale.max,col="white", border = NULL)

# Display the legend
legend(PAR.graphicscale.min+10,isotope.graphicscale.max,c("early","mid","late","total"),lty=c(2,3,4,1),cex=1.5,lwd=c(4,4,4,4),bty="n")
legend(PAR.graphicscale.min-20,isotope.graphicscale.max,c(NA,NA,NA),pch=c(1,2,3),cex=1.5,bty="n")
legend(mean(c(PAR.graphicscale.min,PAR.graphicscale.max))-30,isotope.graphicscale.max,paste("Rsq"," PAR"," =" ,round(summary.lm(glm.PAR.total)$adj.r.squared,3)),cex=1.5,bty="n")


# Display y-axis label
par(xpd=TRUE)
 mtext(expression(paste({delta}^13*C,''[VPDB],' ',('\u2030'))),line= 5, side = 2,cex=2)
# Display x-axis label
if(i == 5){mtext(expression('PAR'~({mu}*'mol'~{m}^{-2}~{s}^{-1})),line= 5, side = 1,cex=2)}

# Draw a box around the subfigure
box(which = "plot", lty = "solid", col="black")


#Precipitation
# Parameter value to plot Precipitation
i <- 6

par(xpd=FALSE)
# Open new plot with data.late values
plot(data.late[,i],data.late[,2],xlab="", ylab="",axes = F,frame.plot=T
		,ylim=range(isotope.graphicscale.min,isotope.graphicscale.max)
		,xlim=range(precipitation.graphicscale.min,precipitation.graphicscale.max),cex=2,pch=3)

# Draw grey dashed lines 
abline(v=seq(precipitation.graphicscale.min, precipitation.graphicscale.max ,by = 100),col= "grey60",lwd = 2,lty =2)
abline(h=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),col= "grey60",lwd = 2,lty =2)

# Plot points for early and mid values
points(data.early[,i],data.early[,2],pch=1, cex = 2)
points(data.mid[,i],data.mid[,2],pch=2, cex = 2)

# Set x-axis values
axis(1,at=seq(precipitation.graphicscale.min, precipitation.graphicscale.max ,by = 100)
		,labels=seq(precipitation.graphicscale.min, precipitation.graphicscale.max ,by = 100)
		,line=1,col="black",cex.axis=2,tick=FALSE)
# Set x-axis ticks
axis(1,at=seq(precipitation.graphicscale.min, precipitation.graphicscale.max ,by = 100)
		,labels=rep(NA,length(seq(precipitation.graphicscale.min, precipitation.graphicscale.max ,by = 100)))
		,line=0,col="black",cex.axis=2,tick=TRUE)

# Set y-axis
axis(2,at=seq(isotope.graphicscale.min,isotope.graphicscale.max, by = 2),labels=format(seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2) , nsmall = 1) , line=0,col="black",las=3,cex.axis=2) 

# Calculate the generalised linear model for early values
glm.precipitation.early <- glm(data.early[,2]~data.early[,i])
# Draw the linear regression fit for early values 
abline(glm.precipitation.early[1],glm.precipitation.early[2],lty=2,lwd=4)

# Calculate the generalised linear model for mid values
glm.precipitation.mid<- glm(data.mid[,2]~data.mid[,i])
# Draw the linear regression fit for mid values 
abline(glm.precipitation.mid[1],glm.precipitation.mid[2],lty=3,lwd=4)

# Calculate the generalised linear model for late values
glm.precipitation.late<- glm(data.late[,2]~data.late[,i])
# Draw the linear regression fit for late values 
abline(glm.precipitation.late[1],glm.precipitation.late[2],lty=4,lwd=4)

# Calculate the generalised linear model for total values
glm.precipitation.tot <- glm(climate.iso[,2]~climate.iso[,i])
# Draw the linear regression fit for total values 
abline(glm.precipitation.tot[1],glm.precipitation.tot[2],lty=1,lwd=4)

# Draw the legend area as a white rectangle  
rect(precipitation.graphicscale.min+((precipitation.graphicscale.max-precipitation.graphicscale.min)*0.6),mean(c(isotope.graphicscale.min ,isotope.graphicscale.max))+0.5,precipitation.graphicscale.min+((precipitation.graphicscale.max-precipitation.graphicscale.min)*1.02),isotope.graphicscale.max,col="white", border = NULL)

# Display the legend
legend(0.65*precipitation.graphicscale.max,isotope.graphicscale.max,c("early","mid","late","total"),lty=c(2,3,4,1),cex=1.5,lwd=c(4,4,4,4),bty="n")
legend(0.60*precipitation.graphicscale.max,isotope.graphicscale.max,c(NA,NA,NA),pch=c(1,2,3),cex=1.5,bty="n")
legend(precipitation.graphicscale.min,isotope.graphicscale.max,paste("Rsq"," precip"," =" ,round(summary.lm(glm.precipitation.tot)$adj.r.squared,3)),cex=1.5,bty="n")


# Display y-axis label
par(xpd=TRUE)
 mtext(expression(paste({delta}^13*C,''[VPDB],' ',('\u2030'))),line= 5, side = 2,cex=2)
# Display x-axis label
if(i == 6){ mtext("Precipitation (mm)",line= 4.5, side = 1,cex=2)}

# Draw a box around the subfigure
box(which = "plot", lty = "solid", col="black")

#Model

# generalised linear models over each section and total values
tot.model.early <- glm(data.early[,2]~data.early[,4]+data.early[,5]+data.early[,6])
tot.model.mid <- glm(data.mid[,2]~data.mid[,4]+data.mid[,5]+data.mid[,6])
tot.model.late <- glm(data.late[,2]~data.late[,4]+data.late[,5]+data.late[,6])
tot.model <- glm(climate.iso[,2]~climate.iso[,4]+climate.iso[,5]+climate.iso[,6])
fitted.model.early<- glm(data.early[,2]~tot.model.early$fitted)
fitted.model.mid<- glm(data.mid[,2]~tot.model.mid$fitted)
fitted.model.late<- glm(data.late[,2]~tot.model.late$fitted)
glm.climate.iso <- glm(climate.iso[,2]~tot.model$fitted)

par(xpd=FALSE)
# Open new plot with fitted.model.late fitted values
plot(fitted.model.late$fitted ,data.late[,2],xlab="", ylab="",axes = F,frame.plot=T
		,ylim=range(isotope.graphicscale.min,isotope.graphicscale.max)
		,xlim=range(isotope.graphicscale.min-1,isotope.graphicscale.max-2), cex = 2,pch=3)

# Draw grey dashed lines 
abline(v=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),col= "grey60",lwd = 2,lty =2)
abline(h=seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2),col= "grey60",lwd = 2,lty =2)

# Plot points for early and mid values
points(tot.model.early$fitted ,data.early[,2],pch=1, cex = 2)
points(fitted.model.mid$fitted ,data.mid[,2],pch=2, cex = 2)

# Draw the linear regression fit for all regressions
abline(glm.climate.iso[1],glm.climate.iso[2],lwd=4)

# Set x-axis values
axis(1,at=seq(isotope.graphicscale.min,isotope.graphicscale.max, by = 2),labels=format(seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2) , nsmall = 1) , line=1,col="black",cex.axis=2,tick=FALSE) 
# Set x-axis ticks
axis(1,at=seq(isotope.graphicscale.min,isotope.graphicscale.max, by = 2),labels=rep(NA,length(seq(isotope.graphicscale.min,isotope.graphicscale.max, by = 2))) , line=0,col="black",cex.axis=2,tick=TRUE)

# Set y-axis
axis(2,at=seq(isotope.graphicscale.min,isotope.graphicscale.max, by = 2),labels=format(seq(isotope.graphicscale.min, isotope.graphicscale.max, by = 2) , nsmall = 1) , line=0,col="black",las=3,cex.axis=2) 

# Draw the legend area as a white rectangle  
rect(isotope.graphicscale.min-((isotope.graphicscale.max-isotope.graphicscale.min)*0.16),mean(c(isotope.graphicscale.min ,isotope.graphicscale.max))+0.5,isotope.graphicscale.min+((isotope.graphicscale.max-isotope.graphicscale.min)*0.2),isotope.graphicscale.max,col="white", border = NULL)
legend(isotope.graphicscale.min-1,isotope.graphicscale.max,c("early","mid","late","total"),pch=c(1,2,3,NA),lty=c(NA,NA,NA,1),lwd=c(NA,NA,NA,4),cex=1.5,bty="n")
legend(mean(c(isotope.graphicscale.min,isotope.graphicscale.max))-2,isotope.graphicscale.max,paste("Rsq"," model"," =" ,round(summary.lm(glm.climate.iso)$adj.r.squared,3)),cex=1.5,bty="n")


# Display y-axis and x-axis labels
par(xpd=TRUE)
 mtext(expression(paste({delta}^13*C ,' '[VPDB],' ',' ',meas,' '('\u2030'))),line= 5, side = 2,cex=2)
 mtext(expression(paste({delta}^13*C ,' '[VPDB],' ',' ',model,' '('\u2030'))),line= 5, side = 1,cex=2)

# Draw a box around the subfigure
box(which = "plot", lty = "solid", col="black")

# Linear regression properties
lr.prop <<- data.frame(regrline = c("temperature.early","temperature.mid","temperature.late","temperature.total"
	,"PAR.early","PAR.mid","PAR.late","PAR.total"
	,"precipitation.early","precipitation.mid" ,"precipitation.late" ,"precipitation.total"
	,"model.early","model.mid","model.late","model.tot")
	,Rsquared = c(round(summary.lm(glm.temperature.early)$adj.r.squared,3),round(summary.lm(glm.temperature.mid)$adj.r.squared,3),round(summary.lm(glm.temperature.late)$adj.r.squared,3),round(summary.lm(glm.temperature.total)$adj.r.squared,3)
	,round(summary.lm(glm.PAR.early)$adj.r.squared,3),round(summary.lm(glm.PAR.mid)$adj.r.squared,3),round(summary.lm(glm.PAR.late)$adj.r.squared,3),round(summary.lm(glm.PAR.total)$adj.r.squared,3)
	,round(summary.lm(glm.precipitation.early)$adj.r.squared,3),round(summary.lm(glm.precipitation.mid)$adj.r.squared,3),round(summary.lm(glm.precipitation.late)$adj.r.squared,3),round(summary.lm(glm.precipitation.tot)$adj.r.squared,3)
	,round(summary.lm(fitted.model.early)$adj.r.squared,3),round(summary.lm(fitted.model.mid)$adj.r.squared,3),round(summary.lm(fitted.model.late)$adj.r.squared,3),round(summary.lm(glm.climate.iso)$adj.r.squared,3) )
	,pvalue= c(round(summary.lm(glm.temperature.early)$coefficients[8],3),round(summary.lm(glm.temperature.mid)$coefficients[8],3),round(summary.lm(glm.temperature.late)$coefficients[8],3),round(summary.lm(glm.temperature.total)$coefficients[8],3)
	,round(summary.lm(glm.PAR.early)$coefficients[8],3),round(summary.lm(glm.PAR.mid)$coefficients[8],3),round(summary.lm(glm.PAR.late)$coefficients[8],3),round(summary.lm(glm.PAR.total)$coefficients[8],3)
	,round(summary.lm(glm.precipitation.early)$coefficients[8],3),round(summary.lm(glm.precipitation.mid)$coefficients[8],3),round(summary.lm(glm.precipitation.late)$coefficients[8],3),round(summary.lm(glm.precipitation.tot)$coefficients[8],3)
	,round(summary.lm(fitted.model.early)$coefficients[8],3),round(summary.lm(fitted.model.mid)$coefficients[8],3),round(summary.lm(fitted.model.late)$coefficients[8],3),round(summary.lm(glm.climate.iso)$coefficients[8],3))
)


setwd(.DATA)
#dev.copy2pdf(file = paste("LR_intra_JuneFALSE",site,".pdf",sep=""),   encoding="WinAnsi.enc")
savePlot(file = paste("intra_presentation",site,".png",sep=""))

return(lr.prop)
#lr.prop.june
#lr.prop.juneFALSE

}
