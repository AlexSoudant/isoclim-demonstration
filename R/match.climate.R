match.climate <-
function(y,cellphase,final.isotope,increment.division,increment.enlargement,increment.wallthick,increment.total,FT.res)
{

# Loop to create the isotope - climate match for all years considered
for ( g in 1:length(y)){

	# Load the Gompertz relative values for radial growth 
	load(paste("gompertzequation_y.eq1",y[g],".rda",sep=""))
	# Load th flux tower data
	load(paste(as.character(site),"_FTdata",y[g],".rda",sep=""))

	# Night conditions FILTER
	FT.data$Temp[FT.data$hour < 7] <- NA
	FT.data$Temp[FT.data$hour > 21] <- NA
	FT.data$Rad[FT.data$hour < 7] <- NA
	FT.data$Rad[FT.data$hour > 21] <- NA

	# Outliers filter
	FT.data$Temp[FT.data$Temp < -50] <- NA
	FT.data$Temp[FT.data$Temp > 50] <- NA

	FT.data$Rad[FT.data$Rad < 0] <- NA
	FT.data$Rad[FT.data$Rad > 3000] <- NA

	FT.data$Precip[FT.data$Precip < 0] <- NA
	FT.data$Precip[FT.data$Precip > 300] <- NA

	# Object to contain the relative growth values for a specific year
	span <- matrix(NA, 1,length(final.isotope[,1][final.isotope[,1] == y[g]]))
	# Calculate span values from final.isotope 
	for(i in 1:length(final.isotope[,1][final.isotope[,1] == y[g]])){
		span[i] <- i/length(final.isotope[,1][final.isotope[,1] == y[g]])*100
	}


	# Cell division ________________________________________________________________

	if(cellphase == 1){


		# Object to contain temperature values averaged for the dates of wood formation
		Temperature <- matrix(NA,length(final.isotope[,3][final.isotope[,1]==y[g]]),1)

		# Calculate the first value of Temperature
		Temperature[1] <-  mean(FT.data$Temp[(which(y.eq1==y.eq1[y.eq1 > 0.1][1], arr.ind = TRUE)*FT.res):(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.division[,2][which(trunc(span[1]) == increment.division[,1] )]*FT.res)],na.rm=TRUE)

		# Loop to calculate all following values of temperature between two dates of wood formation
		for( i in 2:length(final.isotope[,3][final.isotope[,1]==y[g]])-1)   {

			Temperature[i+1] <- mean(FT.data$Temp[which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i])):which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.division[,2][which(trunc(span[i+1]) == increment.division[,1] )])],na.rm=TRUE)
		}

		# Object to contain PAR values averaged for the dates of wood formation
		PAR <- matrix(NA,length(final.isotope[,3][final.isotope[,1]==y[g]]),1)

		# Calculate the first value of PAR
		PAR[1] <-  mean(FT.data$Rad[(which(y.eq1==y.eq1[y.eq1 > 0.1][1], arr.ind = TRUE)*FT.res):(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.division[,2][which(trunc(span[1]) == increment.division[,1] )]*FT.res)],na.rm=TRUE)

		# Loop to calculate all following values of PAR between two dates of wood formation
		for( i in 2:length(final.isotope[,3][final.isotope[,1]==y[g]])-1)   {

			PAR[i+1] <- mean(FT.data$Rad[which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i])):which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.division[,2][which(trunc(span[i+1]) == increment.division[,1] )])],na.rm=TRUE)
		}

		# Object to contain precipitation values summed for the dates of wood formation
		Precip <- matrix(NA,length(final.isotope[,3][final.isotope[,1]==y[g]]),1)

		# Calculate the first value of precipitation
		Precip[1] <-  sum(FT.data$Precip[(which(y.eq1==y.eq1[y.eq1 > 0.1][1], arr.ind = TRUE)*FT.res):(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.division[,2][which(trunc(span[1]) == increment.division[,1] )]*FT.res)],na.rm=TRUE)

		# Loop to calculate all following values of precipitation between two dates of wood formation
		for( i in 2:length(final.isotope[,3][final.isotope[,1]==y[g]])-1)   {
			Precip[i+1] <- sum(FT.data$Precip[which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i])):which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.division[,2][which(trunc(span[i+1]) == increment.division[,1] )])],na.rm=TRUE)
		}
	}

	# Cell enlargement _____________________________________________________________

	if(cellphase == 2){

		# Object to contain temperature values averaged for the dates of wood formation
		Temperature <- matrix(NA,length(final.isotope[,3][final.isotope[,1]==y[g]]),1)

		# Calculate the first value of Temperature
		Temperature[1] <-  mean(FT.data$Temp[(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res):(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.enlargement[,2][which(trunc(span[1]) == increment.enlargement[,1] )]*FT.res)],na.rm=TRUE)

		# Loop to calculate all following values of temperature between two dates of wood formation
		for( i in 2:length(final.isotope[,3][final.isotope[,1]==y[g]])-1)   {
			Temperature[i+1] <- mean(FT.data$Temp[which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])):which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.enlargement[,2][which(trunc(span[i+1]) == increment.enlargement[,1] )])],na.rm=TRUE)
		}

		# Object to contain PAR values averaged for the dates of wood formation
		PAR <- matrix(NA,length(final.isotope[,3][final.isotope[,1]==y[g]]),1)

		# Calculate the first value of PAR
		PAR[1] <-  mean(FT.data$Rad[(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res):(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.enlargement[,2][which(trunc(span[1]) == increment.enlargement[,1] )]*FT.res)],na.rm=TRUE)

		# Loop to calculate all following values of PAR between two dates of wood formation
		for( i in 2:length(final.isotope[,3][final.isotope[,1]==y[g]])-1)   {
			PAR[i+1] <- mean(FT.data$Rad[which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])):which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.enlargement[,2][which(trunc(span[i+1]) == increment.enlargement[,1] )])],na.rm=TRUE)
		}

		# Object to contain precipitation values summed for the dates of wood formation
		Precip <- matrix(NA,length(final.isotope[,3][final.isotope[,1]==y[g]]),1)

		# Calculate the first value of precipitation
		Precip[1] <-  sum(FT.data$Precip[(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res):(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.enlargement[,2][which(trunc(span[1]) == increment.enlargement[,1] )]*FT.res)],na.rm=TRUE)

		# Loop to calculate all following values of precipitation between two dates of wood formation
		for( i in 2:length(final.isotope[,3][final.isotope[,1]==y[g]])-1)   {
			Precip[i+1] <- sum(FT.data$Precip[which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])):which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.enlargement[,2][which(trunc(span[i+1]) == increment.enlargement[,1] )])],na.rm=TRUE)
		}
	}
	# secondary wall thickening ____________________________________________________

	if(cellphase == 3){

		# Object to contain temperature values averaged for the dates of wood formation
		Temperature <- matrix(NA,length(final.isotope[,3][final.isotope[,1]==y[g]]),1)

		# Calculate the first value of Temperature
		Temperature[1] <-  mean(FT.data$Temp[(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.enlargement[,2][which(trunc(span[1]) == increment.wallthick[,1] )]*FT.res):(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.wallthick[,2][which(trunc(span[1]) == increment.wallthick[,1] )]*FT.res)],na.rm=TRUE)

		# Loop to calculate all following values of temperature between two dates of wood formation
		for( i in 2:length(final.isotope[,3][final.isotope[,1]==y[g]])-1)   {
			Temperature[i+1] <- mean(FT.data$Temp[which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.enlargement[,2][which(trunc(span[i+1]) == increment.wallthick[,1] )]):which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.wallthick[,2][which(trunc(span[i+1]) == increment.wallthick[,1] )])],na.rm=TRUE)
		}

		# Object to contain PAR values averaged for the dates of wood formation
		PAR <- matrix(NA,length(final.isotope[,3][final.isotope[,1]==y[g]]),1)

		# Calculate the first value of PAR
		PAR[1] <-  mean(FT.data$Rad[(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.enlargement[,2][which(trunc(span[1]) == increment.wallthick[,1] )]*FT.res):(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.wallthick[,2][which(trunc(span[1]) == increment.wallthick[,1] )]*FT.res)],na.rm=TRUE)

		# Loop to calculate all following values of PAR between two dates of wood formation
		for( i in 2:length(final.isotope[,3][final.isotope[,1]==y[g]])-1)   {
			PAR[i+1] <- mean(FT.data$Rad[which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.enlargement[,2][which(trunc(span[i+1]) == increment.wallthick[,1] )]):which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.wallthick[,2][which(trunc(span[i+1]) == increment.wallthick[,1] )])],na.rm=TRUE)
		}

		# Object to contain precipitation values summed for the dates of wood formation
		Precip <- matrix(NA,length(final.isotope[,3][final.isotope[,1]==y[g]]),1)

		# Calculate the first value of precipitation
		Precip[1] <-  mean(FT.data$Precip[(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.enlargement[,2][which(trunc(span[1]) == increment.wallthick[,1] )]*FT.res):(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.wallthick[,2][which(trunc(span[1]) == increment.wallthick[,1] )]*FT.res)],na.rm=TRUE)

		# Loop to calculate all following values of precipitation between two dates of wood formation
		for( i in 2:length(final.isotope[,3][final.isotope[,1]==y[g]])-1)   {
			Precip[i+1] <- mean(FT.data$Precip[which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.enlargement[,2][which(trunc(span[i+1]) == increment.wallthick[,1] )]):which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.wallthick[,2][which(trunc(span[i+1]) == increment.wallthick[,1] )])],na.rm=TRUE)
		}
	}
	# total growth ______________________________________________________________________________________________________________________________________

	if(cellphase == 4){

		# Object to contain temperature values averaged for the dates of wood formation
		Temperature <- matrix(NA,length(final.isotope[,3][final.isotope[,1]==y[g]]),1)

		# Calculate the first value of Temperature
		Temperature[1] <-  mean(FT.data$Temp[(which(y.eq1==y.eq1[y.eq1 > 0.1][1], arr.ind = TRUE)*FT.res):(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.total[,2][which(trunc(span[1]) == increment.total[,1] )]*FT.res)],na.rm=TRUE)

		# Loop to calculate all following values of temperature between two dates of wood formation
		for( i in 2:length(final.isotope[,3][final.isotope[,1]==y[g]])-1)   {
			Temperature[i+1] <- mean(FT.data$Temp[which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i])):which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.total[,2][which(trunc(span[i+1]) == increment.total[,1] )])],na.rm=TRUE)
		}

		# Object to contain PAR values averaged for the dates of wood formation
		 PAR <- matrix(NA,length(final.isotope[,3][final.isotope[,1]==y[g]]),1)

		# Calculate the first value of PAR
		PAR[1] <-  mean(FT.data$Rad[(which(y.eq1==y.eq1[y.eq1 > 0.1][1], arr.ind = TRUE)*FT.res):(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.total[,2][which(trunc(span[1]) == increment.total[,1] )]*FT.res)],na.rm=TRUE)

		# Loop to calculate all following values of PAR between two dates of wood formation
		for( i in 2:length(final.isotope[,3][final.isotope[,1]==y[g]])-1)   {
			PAR[i+1] <- mean(FT.data$Rad[which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i])):which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.total[,2][which(trunc(span[i+1]) == increment.total[,1] )])],na.rm=TRUE)
		}

		# Object to contain precipitation values summed for the dates of wood formation
		Precip <- matrix(NA,length(final.isotope[,3][final.isotope[,1]==y[g]]),1)

		# Calculate the first value of precipitation
		Precip[1] <-  sum(FT.data$Precip[(which(y.eq1==y.eq1[y.eq1 > 0.1][1], arr.ind = TRUE)*FT.res):(round(final.isotope[,3][final.isotope[,1]==y[g]][1])*FT.res+increment.total[,2][which(trunc(span[1]) == increment.total[,1] )]*FT.res)],na.rm=TRUE)

		# Loop to calculate all following values of precipitation between two dates of wood formation
		for( i in 2:length(final.isotope[,3][final.isotope[,1]==y[g]])-1)   {
			Precip[i+1] <- sum(FT.data$Precip[which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i])):which(FT.data$JDay==round(final.isotope[,3][final.isotope[,1]==y[g]][i+1])+increment.total[,2][which(trunc(span[i+1]) == increment.total[,1] )])],na.rm=TRUE)
		}
	}

	# Object to contain the isotope values
	d13c <-final.isotope[,2][final.isotope[,1]==y[g]]

	# Object to contain isotope and climate values by dates of wood formation
	Pairs.dataframe <- data.frame(d13c,Temperature,PAR,Precip)


	#____________________________________________________________ Saving

	setwd(.DATA)
	save(Pairs.dataframe,file=paste(as.character(site),"_matchclimate",y[g],".rda",sep=""))

}
}
