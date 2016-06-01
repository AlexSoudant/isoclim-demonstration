gs.calc <-
function(y,GS.method,cor.iso,treshold.Rad,treshold.T,treshold.NEE.onset,treshold.NEE.cessation,treshold.ET.onset,treshold.ET.cessation,FT.res)
{


if(GS.method == 1){

	# create the storing object for growing season dates
	data_gs <-data.frame(year=y,Minmeas=rep(NA,length(y)), onset=rep(NA,length(y)) ,cessation=rep(NA,length(y)) ,length=rep(NA,length(y)), threshold=rep(NA,length(y)))
	# Specify the default method to determine the growing season (by using PAR)
	data_gs[,6] <- "PAR"

	# Loop to attribute Onset and Cessation dates for the radial growing season to all years considered
	for(g in 1:length(y)){


		# Load potential radiation to correct for missing data in radiative variable
		setwd(.DATA)
		load(paste(as.character(site),"_rad_pot_",y[g],".rda",sep=""))
		load(paste(as.character(site),"_FTdata",y[g],".rda",sep=""))

		# Correction value to match potential radiation to PAR variable
		rad_pot[,2] <- rad_pot[,2]*1.2


		# Attribute corrected potential radiation value when missing data is found in FT.data
		FT.data$Rad[FT.data$Rad == "NaN"] <- rad_pot[,2][which(FT.data$Rad == "NaN")]
		FT.data$Rad[FT.data$Rad == "-9999"] <- rad_pot[,2][which(FT.data$Rad == "-9999")]

		FT.data$Temp[FT.data$Temp == "NaN"] <- 0
		FT.data$Temp[FT.data$Temp == "-9999"] <- 0




		# Calculate smoothing splines on PAR 
		splinesRad. <- smooth.spline(FT.data$Rad, spar = 0.8)
		# Calculate smoothing splines on temperature 
		splinesT. <- smooth.spline(FT.data$Temp, spar = 0.8)

		# Fill data_gs object with dates found by the threshold method
		data_gs[,2][data_gs[,1]==y[g]]<- length(isotope.data[,1][isotope.data[,2]==y[g]])
		# Determine Onset dates estimated by the PAR threshold
		data_gs[,3][data_gs[,1]==y[g]]  <- trunc(which(splinesRad.$y> treshold.Rad)[1]  /FT.res )
		# Determine Onset dates with temperature if the PAR threshold produced NA's also specify the threshold used (PAR or temp)
		if(is.na(data_gs[g,3])){ data_gs[,3][data_gs[,1]==y[g]]<- trunc(which(splinesT.$y> treshold.T)[1]  /FT.res )
			data_gs[g,6] <- "Temp"
		}
		# Determine Onset dates with temperature if the PAR threshold produced onset dates after DOY 170 (unrealistic at our study site)
		if(data_gs[g,3] > 170){ data_gs[,3][data_gs[,1]==y[g]]<- trunc(which(splinesT.$y> treshold.T)[1]  /FT.res )
 			data_gs[g,6] <- "Temp"
		}
		# Determine Cessation dates with the temperature threshold
		data_gs[,4][data_gs[,1]==y[g]]  <- trunc(which(splinesT.$y > treshold.T)[length(which(splinesT.$y > treshold.T))] /FT.res )
		# Calculate the length of the radial growing season (length = cessation - onset)
		data_gs[,5][data_gs[,1]==y[g]]  <-  data_gs[,3][data_gs[,1]==y[g]]  -   data_gs[,2][data_gs[,1]==y[g]]
  

	}

data.gs <<- data_gs
return(data.gs)
}

if(GS.method == 2){

	# create the storing object for growing season dates
	data_gs <-data.frame(year=y,Minmeas=rep(NA,length(y)), onset=rep(NA,length(y)) ,cessation=rep(NA,length(y)) ,length=rep(NA,length(y)), threshold=rep(NA,length(y))   )
	# Specify the default method to determine the growing season (by using PAR)
	data_gs[,6] <- "ET"

	# First column = years
	ET.Growth.Days[,1]<-y

	# Loop to implement onset and cessation dates for all years considered
	for(g in 1:length(y)){

		# Fill data_gs object with dates found by the threshold method
		data_gs[,2][data_gs[,1]==y[g]]<- length(isotope.data[,1][isotope.data[,2]==y[g]])

		# Load Flux tower data
		setwd(.DATA)
		load(paste(as.character(site),"_FTdata",y[g],".rda",sep=""))

		# Objects to contain minimum and maximum temperatures
		min.Hyy.T <- matrix(NA,1:max(FT.data$JDay),1)
		max.Hyy.T <- matrix(NA,1:max(FT.data$JDay),1)

		# Loop to fill min.Hyy.T and max.Hyy.T
		for (j in 1:(length(FT.data[,1])/FT.res)) {
			min.Hyy.T[j] <- min(FT.data$Temp[FT.data$JDay== j],na.rm=TRUE)
			max.Hyy.T[j] <- max(FT.data$Temp[FT.data$JDay== j],na.rm=TRUE)
		}


		# Object to define NEE values to use for the cumulative variable
		ET.Hyy.flux <- matrix(NA,1:max(FT.data$JDay),1)

		# Loop to fill NEE.Hyy.flux
		for (j in 1:(length(FT.data[,1])/FT.res)) {
			ET.Hyy.flux[j] <- mean(FT.data$EvapoT[FT.data$JDay== j],na.rm=TRUE)
		} 

		# Object to contain the cumulative NEE
		ET.cumul <- matrix(NA,length(ET.Hyy.flux),1)
		# Set up the first value of NEE.cumul
		ET.cumul[1] <- ET.Hyy.flux[1]

		# Loop to fill ET.cumul
		for ( j in 2:(length(ET.cumul))){

			ET.cumul[j] <- ET.cumul[j-1]+ ET.Hyy.flux[j]
		}

		# Object to contain normalised cumulative ET in percentage
		norm.cumul <-  matrix(NA,length(ET.Hyy.flux),1)

		# Loop to fill norm.cumul
		for(i in 1:length(ET.cumul)){
			norm.cumul[i] <- ET.cumul[i] / max(ET.cumul) * 100
		}

		# Round values from norm.cumul 
		norm.cumul <- round(norm.cumul)

		# Attribute dates for onset and cessation of radial growth according to 10% and 95% of cumulative ET values reached respectively
 		data_gs[g,3]<-which(norm.cumul == treshold.ET.onset)[1]
 		data_gs[g,4]<-which(norm.cumul == treshold.ET.cessation)[1]
 		data_gs[g,5]<-which(norm.cumul == treshold.ET.cessation)[1]-which(norm.cumul == treshold.ET.onset)[1]

	}

data.gs <<- data_gs
return(data.gs)
}   


if(GS.method == 3){
	# create the storing object for growing season dates
	data_gs <-data.frame(year=y,Minmeas=rep(NA,length(y)), onset=rep(NA,length(y)) ,cessation=rep(NA,length(y)) ,length=rep(NA,length(y)), threshold=rep(NA,length(y))   )
	# Specify the default method to determine the growing season (by using PAR)
	data_gs[,6] <- "NEE"

	# Loop to implement onset and cessation dates for all years considered
	for(g in 1:length(y)){
 
		# Fill data_gs object with dates found by the threshold method
		data_gs[,2][data_gs[,1]==y[g]]<- length(isotope.data[,1][isotope.data[,2]==y[g]])


		# Load Flux tower data
		setwd(.DATA)
		load(paste(as.character(site),"_FTdata",y[g],".rda",sep=""))

		# Filter out missing data and outliers
		FT.data$NEE[FT.data$NEE == NA] <- 0
		FT.data$NEE[FT.data$NEE == -9999] <- 0
		FT.data$NEE[FT.data$NEE > 0] <- 0
		FT.data$NEE[FT.data$NEE < -50] <- 0
		FT.data$Temp[FT.data$Temp < -40] <- NA
		FT.data$Temp[FT.data$Temp > 50] <- NA

		# Objects to contain minimum and maximum temperatures
		min.Hyy.T <- matrix(NA,1:max(FT.data$JDay),1)
		max.Hyy.T <- matrix(NA,1:max(FT.data$JDay),1)

		# Loop to fill min.Hyy.T and max.Hyy.T
		for (j in 1:(length(FT.data[,1])/FT.res)) {
			min.Hyy.T[j] <- min(FT.data$Temp[FT.data$JDay== j],na.rm=TRUE)
			max.Hyy.T[j] <- max(FT.data$Temp[FT.data$JDay== j],na.rm=TRUE)
		}

		# Object to define NEE values to use for the cumulative variable
		NEE.Hyy.flux <- matrix(NA,1:max(FT.data$JDay),1)

		# Loop to fill NEE.Hyy.flux
		for (j in 1:(length(FT.data[,1])/FT.res)) {
			NEE.Hyy.flux[j] <- min(FT.data$NEE[FT.data$JDay== j],na.rm=TRUE)
		} 

		# Object to contain the cumulative NEE
		NEE.cumul <- matrix(NA,length(NEE.Hyy.flux),1)

		# Set up the first value of NEE.cumul
		NEE.cumul[1] <- NEE.Hyy.flux[1]

		# Loop to fill NEE.cumul
		for ( j in 2:(length(NEE.cumul))){
			NEE.cumul[j] <- NEE.cumul[j-1]+ NEE.Hyy.flux[j]
		}

		# Inverse sign to get positive values
		NEE.cumul <- -NEE.cumul 

		# Object to contain normalised cumulative NEE in percentage
		norm.cumul <- matrix(NA,length(NEE.Hyy.flux),1)

		# Loop to fill norm.cumul
		for(i in 1:length(NEE.cumul)){
			norm.cumul[i] <- NEE.cumul[i] / max(NEE.cumul) * 100
		}

		# Round values from norm.cumul 
		norm.cumul <- round(norm.cumul)

		# Attribute dates for onset and cessation of radial growth according to 2% and 99% of cumulative NEE values reached respectively
		data_gs[g,2]<-which(norm.cumul == treshold.NEE.onset)[1]
		data_gs[g,3]<-which(norm.cumul == treshold.NEE.cessation)[1]
		data_gs[g,4]<-which(norm.cumul == treshold.NEE.cessation)[1]-which(norm.cumul == treshold.NEE.onset)[1]

	} 


data.gs <<- data_gs
return(data.gs)
}

}
