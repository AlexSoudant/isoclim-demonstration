plot.correlation <-
function(annual.dataset,climate.iso)
{

 y <- 1998:2009

setwd(.DATA)
load("inter.clim.1998.rda")
load("inter.clim.1999.rda")
load("inter.clim.2000.rda")
load("inter.clim.2001.rda")
load("inter.clim.2002.rda")
load("inter.clim.2003.rda")
load("inter.clim.2004.rda")
load("inter.clim.2005.rda")
load("inter.clim.2006.rda")
load("inter.clim.2007.rda")
load("inter.clim.2008.rda")
load("inter.clim.2009.rda")


inter.iso.clim <- matrix(NA,12*12,5)

for(i in 1:4){
inter.iso.clim[1:12,i] <- inter.clim.1998[,i]
}
inter.iso.clim[1:12,5] <- rep(annual.dataset[2,2],12)
for(i in 1:4){
inter.iso.clim[13:24,i] <- inter.clim.1999[,i]
}
inter.iso.clim[13:24,5] <- rep(annual.dataset[3,2],12)
for(i in 1:4){
inter.iso.clim[25:36,i] <- inter.clim.2000[,i]
}
inter.iso.clim[25:36,5] <- rep(annual.dataset[4,2],12)
for(i in 1:4){
inter.iso.clim[37:48,i] <- inter.clim.2001[,i]
}
inter.iso.clim[37:48,5] <- rep(annual.dataset[5,2],12)
for(i in 1:4){
inter.iso.clim[49:60,i] <- inter.clim.2002[,i]
}
inter.iso.clim[49:60,5] <- rep(annual.dataset[6,2],12)
for(i in 1:4){
inter.iso.clim[61:72,i] <- inter.clim.2003[,i]
}
inter.iso.clim[61:72,5] <- rep(annual.dataset[7,2],12)
for(i in 1:4){
inter.iso.clim[73:84,i] <- inter.clim.2004[,i]
}
inter.iso.clim[73:84,5] <- rep(annual.dataset[8,2],12)
for(i in 1:4){
inter.iso.clim[85:96,i] <- inter.clim.2005[,i]
}
inter.iso.clim[85:96,5] <- rep(annual.dataset[9,2],12)
for(i in 1:4){
inter.iso.clim[97:108,i] <- inter.clim.2006[,i]
}
inter.iso.clim[97:108,5] <- rep(annual.dataset[10,2],12)
for(i in 1:4){
inter.iso.clim[109:120,i] <- inter.clim.2007[,i]
}
inter.iso.clim[109:120,5] <- rep(annual.dataset[11,2],12)
for(i in 1:4){
inter.iso.clim[121:132,i] <- inter.clim.2008[,i]
}
inter.iso.clim[121:132,5] <- rep(annual.dataset[12,2],12)
for(i in 1:4){
inter.iso.clim[133:144,i] <- inter.clim.2009[,i]
}
inter.iso.clim[133:144,5] <- rep(annual.dataset[13,2],12)

# correlation 

#_____________________________October
#_____________temperature
cor.oct.temp <- cor(inter.iso.clim[,2][inter.iso.clim[,1]==10],inter.iso.clim[,5][inter.iso.clim[,1]==10])
#_____________PAR
cor.oct.PAR <- cor(inter.iso.clim[,3][inter.iso.clim[,1]==10],inter.iso.clim[,5][inter.iso.clim[,1]==10])
#_____________precip
cor.oct.precip <- cor(inter.iso.clim[,4][inter.iso.clim[,1]==10],inter.iso.clim[,5][inter.iso.clim[,1]==10])
#_____________________________November
#_____________temperature
cor.nov.temp <- cor(inter.iso.clim[,2][inter.iso.clim[,1]==11],inter.iso.clim[,5][inter.iso.clim[,1]==11])
#_____________PAR
cor.nov.PAR <- cor(inter.iso.clim[,3][inter.iso.clim[,1]==11],inter.iso.clim[,5][inter.iso.clim[,1]==11])
#_____________precip
cor.nov.precip <- cor(inter.iso.clim[,4][inter.iso.clim[,1]==11],inter.iso.clim[,5][inter.iso.clim[,1]==11])
#_____________________________December
#_____________temperature
cor.dec.temp <- cor(inter.iso.clim[,2][inter.iso.clim[,1]==12],inter.iso.clim[,5][inter.iso.clim[,1]==12])
#_____________PAR
cor.dec.PAR <- cor(inter.iso.clim[,3][inter.iso.clim[,1]==12],inter.iso.clim[,5][inter.iso.clim[,1]==12])
#_____________precip
cor.dec.precip <- cor(inter.iso.clim[,4][inter.iso.clim[,1]==12],inter.iso.clim[,5][inter.iso.clim[,1]==12])
#_____________________________January
#_____________temperature
cor.jan.temp <- cor(inter.iso.clim[,2][inter.iso.clim[,1]==1],inter.iso.clim[,5][inter.iso.clim[,1]==1])
#_____________PAR
cor.jan.PAR <- cor(inter.iso.clim[,3][inter.iso.clim[,1]==1],inter.iso.clim[,5][inter.iso.clim[,1]==1])
#_____________precip
cor.jan.precip <- cor(inter.iso.clim[,4][inter.iso.clim[,1]==1],inter.iso.clim[,5][inter.iso.clim[,1]==1])
#_____________________________February
#_____________temperature
cor.feb.temp <- cor(inter.iso.clim[,2][inter.iso.clim[,1]==2],inter.iso.clim[,5][inter.iso.clim[,1]==2])
#_____________PAR
cor.feb.PAR <- cor(inter.iso.clim[,3][inter.iso.clim[,1]==2],inter.iso.clim[,5][inter.iso.clim[,1]==2])
#_____________precip
cor.feb.precip <- cor(inter.iso.clim[,4][inter.iso.clim[,1]==2],inter.iso.clim[,5][inter.iso.clim[,1]==2])
#_____________________________March
#_____________temperature
cor.mar.temp <- cor(inter.iso.clim[,2][inter.iso.clim[,1]==3],inter.iso.clim[,5][inter.iso.clim[,1]==3])
#_____________PAR
cor.mar.PAR <- cor(inter.iso.clim[,3][inter.iso.clim[,1]==3],inter.iso.clim[,5][inter.iso.clim[,1]==3])
#_____________precip
cor.mar.precip <- cor(inter.iso.clim[,4][inter.iso.clim[,1]==3],inter.iso.clim[,5][inter.iso.clim[,1]==3])
#_____________________________april
#_____________temperature
cor.apr.temp <- cor(inter.iso.clim[,2][inter.iso.clim[,1]==4],inter.iso.clim[,5][inter.iso.clim[,1]==4])
#_____________PAR
cor.apr.PAR <- cor(inter.iso.clim[,3][inter.iso.clim[,1]==4],inter.iso.clim[,5][inter.iso.clim[,1]==4])
#_____________precip
cor.apr.precip <- cor(inter.iso.clim[,4][inter.iso.clim[,1]==4],inter.iso.clim[,5][inter.iso.clim[,1]==4])
#_____________________________may
#_____________temperature
cor.may.temp <- cor(inter.iso.clim[,2][inter.iso.clim[,1]==5],inter.iso.clim[,5][inter.iso.clim[,1]==5])
#_____________PAR
cor.may.PAR <- cor(inter.iso.clim[,3][inter.iso.clim[,1]==5],inter.iso.clim[,5][inter.iso.clim[,1]==5])
#_____________precip
cor.may.precip <- cor(inter.iso.clim[,4][inter.iso.clim[,1]==5],inter.iso.clim[,5][inter.iso.clim[,1]==5])
#_____________________________june
#_____________temperature
cor.jun.temp <- cor(inter.iso.clim[,2][inter.iso.clim[,1]==6],inter.iso.clim[,5][inter.iso.clim[,1]==6])
#_____________PAR
cor.jun.PAR <- cor(inter.iso.clim[,3][inter.iso.clim[,1]==6],inter.iso.clim[,5][inter.iso.clim[,1]==6])
#_____________precip
cor.jun.precip <- cor(inter.iso.clim[,4][inter.iso.clim[,1]==6],inter.iso.clim[,5][inter.iso.clim[,1]==6])
#_____________________________july
#_____________temperature
cor.jul.temp <- cor(inter.iso.clim[,2][inter.iso.clim[,1]==7],inter.iso.clim[,5][inter.iso.clim[,1]==7])
#_____________PAR
cor.jul.PAR <- cor(inter.iso.clim[,3][inter.iso.clim[,1]==7],inter.iso.clim[,5][inter.iso.clim[,1]==7])
#_____________precip
cor.jul.precip <- cor(inter.iso.clim[,4][inter.iso.clim[,1]==7],inter.iso.clim[,5][inter.iso.clim[,1]==7])
#_____________________________august
#_____________temperature
cor.aug.temp <- cor(inter.iso.clim[,2][inter.iso.clim[,1]==8],inter.iso.clim[,5][inter.iso.clim[,1]==8])
#_____________PAR
cor.aug.PAR <- cor(inter.iso.clim[,3][inter.iso.clim[,1]==8],inter.iso.clim[,5][inter.iso.clim[,1]==8])
#_____________precip
cor.aug.precip <- cor(inter.iso.clim[,4][inter.iso.clim[,1]==8],inter.iso.clim[,5][inter.iso.clim[,1]==8])
#_____________________________september
#_____________temperature
cor.sep.temp <- cor(inter.iso.clim[,2][inter.iso.clim[,1]==9],inter.iso.clim[,5][inter.iso.clim[,1]==9])
#_____________PAR
cor.sep.PAR <- cor(inter.iso.clim[,3][inter.iso.clim[,1]==9],inter.iso.clim[,5][inter.iso.clim[,1]==9])
#_____________precip
cor.sep.precip <- cor(inter.iso.clim[,4][inter.iso.clim[,1]==9],inter.iso.clim[,5][inter.iso.clim[,1]==9])

x11()
par(mfrow=c(2,2))

cor.temp <- c(cor.oct.temp,cor.nov.temp,cor.dec.temp,cor.jan.temp,cor.feb.temp
	,cor.mar.temp,cor.apr.temp,cor.may.temp,cor.jun.temp,cor.jul.temp,cor.aug.temp,cor.sep.temp)

barplot(cor.temp,ylim=range(0.80,-0.60) ,axes=FALSE,
  xlab="", col=c("red"), beside=T)

abline(h=0.50)
legend(0,0.50,"significance level",cex=1,bty="n")
  axis(1,at=c(0,1.20,2.4,3.6,4.8,6,7.2,8.4,9.6,10.8,12,13.2,14.4) ,labels=rep(NA,13),tick=TRUE)
  axis(1,at=c(mean(c(0,1.20)),mean(c(1.20,2.4)),mean(c(2.4,3.6)),mean(c(3.6,4.8)),mean(c(4.8,6)),mean(c(6,7.2)),mean(c(7.2,8.4)),mean(c(8.4,9.6)),mean(c(9.6,10.8)),mean(c(10.8,12)),mean(c(12,13.2)),mean(c(13.2,14.4))) ,labels=c("October","November","December","January","February","March","April","May","June","July","August","September"),tick=FALSE,las=2)
 axis(2,at=seq(0.80,-0.60, by = -0.20),labels=seq(0.80,-0.60, by = -0.20) , line=0)
 axis(2,at=c(0.80,0.40,-0.40),labels=c(0.80,0.40,-0.40) , line=0,tick=FALSE)
mtext("Correlation",line= 2.5, side = 2,cex=1)
legend(0,0.80,"Temperature",cex=1,bty="n")
box(which = "plot", lty = "solid", col="black")

cor.PAR <- c(cor.oct.PAR,cor.nov.PAR,cor.dec.PAR,cor.jan.PAR,cor.feb.PAR
	,cor.mar.PAR,cor.apr.PAR,cor.may.PAR,cor.jun.PAR,cor.jul.PAR,cor.aug.PAR,cor.sep.PAR)

barplot(cor.PAR,ylim=range(0.80,-0.60) ,axes=FALSE,
  xlab="", col=c("red"), beside=T)
abline(h=0.50)
legend(0,0.50,"significance level",cex=1,bty="n")
  axis(1,at=c(0,1.20,2.4,3.6,4.8,6,7.2,8.4,9.6,10.8,12,13.2,14.4) ,labels=rep(NA,13),tick=TRUE)
  axis(1,at=c(mean(c(0,1.20)),mean(c(1.20,2.4)),mean(c(2.4,3.6)),mean(c(3.6,4.8)),mean(c(4.8,6)),mean(c(6,7.2)),mean(c(7.2,8.4)),mean(c(8.4,9.6)),mean(c(9.6,10.8)),mean(c(10.8,12)),mean(c(12,13.2)),mean(c(13.2,14.4))) ,labels=c("October","November","December","January","February","March","April","May","June","July","August","September"),tick=FALSE,las=2)
 axis(2,at=seq(0.80,-0.60, by = -0.20),labels=seq(0.80,-0.60, by = -0.20) , line=0)
 axis(2,at=c(0.80,0.40,-0.40),labels=c(0.80,0.40,-0.40) , line=0)
mtext("Correlation",line= 2.5, side = 2,cex=1)
legend(0,0.80,"PAR",cex=1,bty="n")
box(which = "plot", lty = "solid", col="black")

cor.precip <- c(cor.oct.precip,cor.nov.precip,cor.dec.precip,cor.jan.precip,cor.feb.precip
	,cor.mar.precip,cor.apr.precip,cor.may.precip,cor.jun.precip,cor.jul.precip,cor.aug.precip,cor.sep.precip)

barplot(cor.precip,ylim=range(0.40,-0.80) ,axes=FALSE,
  xlab="", col=c("red"), beside=T)
abline(h=-0.50)
legend(0,-0.50,"significance level",cex=1,bty="n")
  axis(1,at=c(0,1.20,2.4,3.6,4.8,6,7.2,8.4,9.6,10.8,12,13.2,14.4) ,labels=rep(NA,13),tick=TRUE)
  axis(1,at=c(mean(c(0,1.20)),mean(c(1.20,2.4)),mean(c(2.4,3.6)),mean(c(3.6,4.8)),mean(c(4.8,6)),mean(c(6,7.2)),mean(c(7.2,8.4)),mean(c(8.4,9.6)),mean(c(9.6,10.8)),mean(c(10.8,12)),mean(c(12,13.2)),mean(c(13.2,14.4))) ,labels=c("October","November","December","January","February","March","April","May","June","July","August","September"),tick=FALSE,las=2)
 axis(2,at=seq(0.40,-0.80, by = -0.20),labels=seq(0.40,-0.80, by = -0.20) , line=0)
 axis(2,at=c(-0.20,-0.60),labels=c(-0.20,-0.60), line=0)
mtext("Correlation",line= 2.5, side = 2,cex=1)
legend(0,0.40,"Precipitation",cex=1,bty="n")
box(which = "plot", lty = "solid", col="black")

plot.new()
legend(0,1,"inter-annual",cex=2,bty="n")
legend(0,0.5,"correlations",cex=2,bty="n")

setwd(.DATA)
savePlot(file = paste("correlation_presentation",site,".png",sep=""))


}
