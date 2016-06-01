interpolation.tech <-
function(y,iso1,iso2,iso3)
{

# Find length of the final dataset
length.matrix <- matrix(NA,length(y),1)

# Find the minimum length of each year of measurements between the 3 time series 
for(k in 1:length(y) ){

	l1 <- length(iso1[,2][iso1[,2] == y[k]])
	l2 <- length(iso2[,2][iso2[,2] == y[k]])
	l3 <- length(iso3[,2][iso3[,2] == y[k]])

	length.matrix[k] <- min(l1,l2,l3)
}

# Attribute new total length from minimum lengths by year to the new interpolated dataset 
final.iso.dataset <- matrix(NA,sum(length.matrix),5)
# Create a index in the first column  
final.iso.dataset[,1] <- 1:length(final.iso.dataset[,1]) 

# set initial parameters for next loop
a <- 1
b <- length.matrix[1] 

# Loop to input coluwn 2 with the indication of the year to each new interpolated isotope value 
for(i  in 1: length(y)) {

	b <- sum(length.matrix[1:i])

	j <-  length.matrix[i]
	final.iso.dataset[,2][a:b] <- rep(y[i],j)

	a <-  b + 1

}


# Loop to interpolate values of isotopes for 3 raw times series
for(k in 1:length(y)){

  year <- y[k]
  
	# Number of measurements for year k  
	l1 <- length(iso1[,2][iso1[,2] == year])
	l2 <- length(iso2[,2][iso2[,2] == year])
	l3 <- length(iso3[,2][iso3[,2] == year])

	# Converstion in percentage of measurements for time serie 1
	perc1 <-  matrix(NA,l1,1)

	for ( i in 1:l1) {
		perc1[i] <- i/l1 * 100
	}

	# Converstion in percentage of measurements for time serie 2
	perc2 <-  matrix(NA,l2,1)
	for ( i in 1:l2) {
		perc2[i] <- i/l2 * 100
	}

	# Converstion in percentage of measurements for time serie 3
	perc3 <-  matrix(NA,l3,1)
	for ( i in 1:l3) {
		perc3[i] <- i/l3 * 100
	}

	# New objects containing the percentage of measurements for the 3 time series 
	iso1.norm <- data.frame(index = iso1[,1][iso1[,2] == year],year = iso1[,2][iso1[,2] == year],iso1 = iso1[,3][iso1[,2] == year],perc1)
	iso2.norm <- data.frame(index = iso2[,1][iso2[,2] == year],year = iso2[,2][iso2[,2] == year],iso2 = iso2[,3][iso2[,2] == year],perc2)
	iso3.norm <- data.frame(index = iso3[,1][iso3[,2] == year],year = iso3[,2][iso3[,2] == year],iso3 = iso3[,3][iso3[,2] == year],perc3)

	# values k of percentage regrouped in one object 
	inter.length <- c(l1,l2,l3)
	# Find which time serie has the minimum percentage value 
	Q.length <- which( inter.length  == min(l1,l2,l3) )
	# Attribute the time serie number (1 or 2 or 3) to new object
	Q.length2 <- min(Q.length) 

	# Conditions to find which time serie is the reference (minimum percentage value) and which are to be interpolated 
	if(Q.length2 == 1)  {
		norm.ref <- iso1.norm
		norm.var1 <- iso2.norm
		norm.var2 <- iso3.norm
	}
	if(Q.length2 == 2)  {
		norm.ref <- iso2.norm
		norm.var1 <- iso1.norm
		norm.var2 <- iso3.norm
	}
	if(Q.length2 == 3)  {
		norm.ref <- iso3.norm
		norm.var1 <- iso1.norm
		norm.var2 <- iso2.norm
	}

	# New temporary object to contain the interpolated values of isotope for one of the time series to be interpolated
	storage.var1 <- matrix(NA,min(l1,l2,l3),1)


	for(i in 1:min(l1,l2,l3)) {

		# Find j the minimum value of reference for each value of isotope to be interpolated
		j <- which(norm.var1[,4] <= norm.ref[,4][i])[length(which(norm.var1[,4] <= norm.ref[,4][i]))] 
		# Condition to find j at the last value of isotope from each year
		if(i == min(l1,l2,l3)) j <- j - 1

		# Attribute the interpolated values to the temporary object "storage.var1" 
		storage.var1[i] <- norm.var1[,3][j] + ((norm.ref[,4][i] - norm.var1[,4][j]) * norm.var1[,3][j + 1] -  (norm.ref[,4][i] - norm.var1[,4][j]) * norm.var1[,3][j]  ) / (norm.var1[,4][j + 1]-norm.var1[,4][j])


 	}
 
	# New temporary object to contain the interpolated values of isotope for the second of the time series to be interpolated
	storage.var2 <- matrix(NA,min(l1,l2,l3),1)

	for(i in 1:min(l1,l2,l3)) {
		# Find j the minimum value of reference for each value of isotope to be interpolated
		j <- which(norm.var2[,4] <= norm.ref[,4][i])[length(which(norm.var2[,4] <= norm.ref[,4][i]))] 
		# Condition to find j at the last value of isotope from each year
		if(i == min(l1,l2,l3)) j <- j - 1

		# Attribute the interpolated values to the temporary object "storage.var2" 
		storage.var2[i] <- norm.var2[,3][j] + ((norm.ref[,4][i] - norm.var2[,4][j]) * norm.var2[,3][j + 1] -  (norm.ref[,4][i] - norm.var2[,4][j]) * norm.var2[,3][j]  ) / (norm.var2[,4][j + 1]-norm.var2[,4][j])

 	}

 
	# Create the final interpolated dataset with the three time series in the case Iso1 is the reference
	if(Q.length2 == 1)  {
		final.std.iso <- data.frame(norm.ref[,c(1,2)],std.iso1 = norm.ref[,3],std.iso2 = storage.var1,std.iso3 = storage.var2)
		final.iso.dataset[,3][final.iso.dataset[,2] == year] <- norm.ref[,3]
		final.iso.dataset[,4][final.iso.dataset[,2] == year] <- storage.var1
		final.iso.dataset[,5][final.iso.dataset[,2] == year] <- storage.var2
	}
	# Create the final interpolated dataset with the three time series in the case Iso2 is the reference
	if(Q.length2 == 2)  {
		final.std.iso <- data.frame(norm.ref[,c(1,2)],std.iso1 = storage.var1,std.iso2 = norm.ref[,3],std.iso3 = storage.var2)
		final.iso.dataset[,3][final.iso.dataset[,2] == year] <- storage.var1
		final.iso.dataset[,4][final.iso.dataset[,2] == year] <- norm.ref[,3]
		final.iso.dataset[,5][final.iso.dataset[,2] == year] <- storage.var2
	}
	# Create the final interpolated dataset with the three time series in the case Iso3 is the reference
	if(Q.length2 == 3)  {
		final.std.iso <- data.frame(norm.ref[,c(1,2)],std.iso1 = storage.var1,std.iso2 = storage.var2,std.iso3 = norm.ref[,3])
		final.iso.dataset[,3][final.iso.dataset[,2] == year] <- storage.var1
		final.iso.dataset[,4][final.iso.dataset[,2] == year] <- storage.var2
		final.iso.dataset[,5][final.iso.dataset[,2] == year] <- norm.ref[,3]
	}
	# Creation of the object containing the final interpolated dataset
	isotope.data <<- final.iso.dataset
}
# Display the values in the console
return(isotope.data)
}
