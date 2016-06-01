intraannual.data <-
function(y,final.isotope)
{
# Find dataset boundary (index value) for the starting year to study
start.dataset <- which(final.isotope[,1] == final.isotope[,1][final.isotope[,1]==range(y)[1]][1])[1]
# Find dataset boundary (index value) for the last year to study
end.dataset <-   which(final.isotope[,1] == final.isotope[,1][final.isotope[,1]==range(y)[2]][length(final.isotope[,1][final.isotope[,1]==range(y)[2]])])[length(final.isotope[,1][final.isotope[,1]==range(y)[2]])]

# Object to contain the dataset with isotope and climate value for the period specified in Data_Main.R
climate.iso <- matrix(NA,end.dataset-start.dataset+1,6)

# Loop to fill climate.iso with the three first column (year, isotope, days of formation)
for(i  in 1:3) {
	climate.iso[,i] <- final.isotope[,i][start.dataset:end.dataset]
}

# Loop to fill climate.iso for all years considered
for(j in 1:length(y)){

	# Load Pairs.dataframe from match_climate.R
	load(paste(as.character(site),"_matchclimate",y[j],".rda",sep=""))

	# Fill climate.iso for columns 4 to 6 (temperature, PAR and Precipitation)
	for(k in 4:6){
		climate.iso[,k][climate.iso[,1] == y[j]] <- Pairs.dataframe[,k-2]
	}
}

# Attribute column names to climate.iso
colnames(climate.iso) <- c("year","iso","date","Temp","Rad","Precip")
# Assignment to Global
climate.iso <<- climate.iso
return(climate.iso)
}
