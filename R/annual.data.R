annual.data <-
function(y,climate.iso)
{
annual.dataset <- matrix(NA,length(y),length(climate.iso[1,]))
annual.dataset[,1] <- y

for ( i in 1:length(y)){


	annual.dataset[i,2] <- mean(climate.iso[,2][climate.iso[,1]==y[i]],na.rm=TRUE)
	annual.dataset[i,3] <- max(climate.iso[,3][climate.iso[,1]==y[i]])-min(climate.iso[,3][climate.iso[,1]==y[i]])
	annual.dataset[i,4] <- mean(climate.iso[,4][climate.iso[,1]==y[i]],na.rm=TRUE)
	annual.dataset[i,5] <- mean(climate.iso[,5][climate.iso[,1]==y[i]],na.rm=TRUE)
	annual.dataset[i,6] <- sum(climate.iso[,6][climate.iso[,1]==y[i]])


}

# Attribute column names to climate.iso
colnames(annual.dataset) <- c("year","iso","date","Temp","Rad","Precip")
# Assignment to Global
annual.dataset <<- annual.dataset
return(annual.dataset)
}
