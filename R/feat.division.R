feat.division <-
function(y,division,climate.iso)
{
# Divide Tree-ring by 3_________________________________________________________________


if(division == 1){

	# Object to contain the number of isotope measurements for each year divided by 3
	length.climate.iso2 <- matrix(NA,length(y),1)

	# Loop to fill length.climate.iso2
	for(i in 1:length(y)){
		length.climate.iso2[i] <- length(climate.iso[,2][climate.iso[,1] == y[i]])/3
	}

	# Object to contain the early measurements of isotopes 
	data.early <- matrix(NA,sum(round(length.climate.iso2)),6)
	# Attribute the first year values for column 1
	data.early[1:round(length.climate.iso2[1]),1] <- rep(y[1],round(length.climate.iso2[1]))

	# Constant used in next loop to set increment
	c <- 0

	# Loop to fill all values for column 1 (year) 
	for(i in 2: length(y)){

		a <- round(length.climate.iso2[i-1])+1 + c
		b<- a + round(length.climate.iso2[i])-1

		data.early[a:b,1] <- rep(y[i],round(length.climate.iso2[i]))

		c <- a-1
	}

	# Loop to fill all other columns in data.early
	for(i in 1:length(y)){
		for(j in 2:6){
			data.early[,j][data.early[,1] == y[i]] <- climate.iso[,j][climate.iso[,1] == y[i]][1:trunc(length.climate.iso2[i])]
		}
	}

	# # Object to contain the mid measurements of isotopes 
	data.mid<- matrix(NA,sum(round(length.climate.iso2)),6)
	# Attribute the first year values for column 1
	data.mid[,1] <- data.early[,1]

	# Loop to fill all other columns in data.mid
	for(i in 1:length(y)){

		start.mid <- round(length.climate.iso2[i])+1
		end.mid <- round(length.climate.iso2[i])+1+round(length.climate.iso2[i])

		for(j in 2:6){
			data.mid[,j][data.mid[,1] == y[i]] <- climate.iso[,j][climate.iso[,1] == y[i]][start.mid :end.mid]

		}
	}


	# Object to contain the number of isotope measurements for each year divided by 3
	length.climate.iso.late <- (length.climate.iso2*3)-round(length.climate.iso2)*2
	# Object to contain the late measurements of isotopes 
	data.late <- matrix(NA,sum(length.climate.iso.late) ,6)
	# Attribute the first year values for column 1
	data.late[1:round(length.climate.iso.late[1]),1] <- rep(y[1],round(length.climate.iso.late[1]))

	# Constant used in next loop to set increment
	c <- 0

	# Loop to fill all values for column 1 (year) 
	for(i in 2: length(y)){

		a <- round(length.climate.iso.late[i-1])+1 + c
		b<- a + round(length.climate.iso.late[i])-1

		data.late[a:b,1] <- rep(y[i],round(length.climate.iso.late[i]))

		c <- a-1
	}

	# Loop to fill all other columns in data.late
	for(i in 1:length(y)){

		start.late <- round(length.climate.iso2[i])*2+1
		end.late <- start.late+length.climate.iso.late[i] 
		for(j in 2:6){
			data.late[,j][data.late[,1] == y[i]] <- climate.iso[,j][climate.iso[,1] == y[i]][start.late :end.late]
		}
	}

	# Attribute column names to the dataset
	colnames(data.early) <- c("year","iso","date","temp","PAR","precip" )
	colnames(data.mid) <- c("year","iso","date","temp","PAR","precip" )
	colnames(data.late) <- c("year","iso","date","temp","PAR","precip" )

	# Assignment to Global
	data.early <<- data.early
	data.mid <<- data.mid
	data.late <<- data.late


}
# growing season / 3____________________________________________________________

if(division == 2){

	# Object to contain the number of isotope measurements for each year
	length.iso.year <- matrix(NA,length(y),4 )
	# Fill column 1 (year)
	length.iso.year[,1] <- y

	# Loop to consider all years
	for(i in 1:length(y)){

		# Set threshold values for division into 3 sections of growing season dates
		thresh.GS <- (climate.iso[,3][climate.iso[,1] == y[i]][length(climate.iso[,3][climate.iso[,1] == y[i]])]-climate.iso[,3][climate.iso[,1] == y[i]][1])/3

		# Attribute a threshold value for each section (early,mid,late)
		boundarie.early <- climate.iso[,3][climate.iso[,1] == y[i]][1] + thresh.GS*1
		boundarie.mid <- climate.iso[,3][climate.iso[,1] == y[i]][1] + thresh.GS*2
		boundarie.late <- climate.iso[,3][climate.iso[,1] == y[i]][1] + thresh.GS*3

		# Object to contain the total data from each year 
		iso.year <- matrix(NA,length(climate.iso[,2][climate.iso[,1] == y[i]]),6)

		# Loop to fill iso.year
		for(j in 1:6){
			iso.year[,j] <- climate.iso[,j][climate.iso[,1] == y[i]]
		}

		# Object to contain the early data from each year
		iso.year.early <- matrix(NA,length(iso.year[,2][iso.year[,3] < boundarie.early]),6)

		# Loop to fill iso.year.early
		for(j in 1:6){
			iso.year.early[,j] <- iso.year[,j][iso.year[,3] < boundarie.early]
		}

		# Temporary object to contain the mid data from each year
		iso.year.mid.temp <- matrix(NA,length(iso.year[,2][iso.year[,3] < boundarie.mid]),6)

		# Loop to fill iso.year.mid.temp
		for(j in 1:6){
			iso.year.mid.temp[,j] <- iso.year[,j][iso.year[,3] < boundarie.mid]
		}

		# Final object to contain the mid data from each year
		iso.year.mid <- matrix(NA,length(iso.year.mid.temp[,2][iso.year.mid.temp[,3] > boundarie.early]),6)

		# Loop to fill iso.year.mid
		for(j in 1:6){
			iso.year.mid[,j] <- iso.year.mid.temp[,j][iso.year.mid.temp[,3] > boundarie.early]  
		} 

		# Object to contain the late data from each year
		iso.year.late <- matrix(NA,length(iso.year[,2][iso.year[,3] > boundarie.mid]),6)

		# Loop to fill iso.year.late
		for(j in 1:6){
			iso.year.late[,j] <- iso.year[,j][iso.year[,3] > boundarie.mid] 
		}

		# Attribute number of observation for each section of GS in length.iso.year
		length.iso.year[i,2] <- length(iso.year.early[,1])
		length.iso.year[i,3] <- length(iso.year.mid[,1])
		length.iso.year[i,4] <- length(iso.year.late[,1])

		#____________________________________________________________ Saving
		setwd(.DATA)
		save(iso.year.early,file=paste("iso.year.early",y[i],".rda",sep=""))
		save(iso.year.mid,file=paste("iso.year.mid",y[i],".rda",sep=""))
		save(iso.year.late,file=paste("iso.year.late",y[i],".rda",sep=""))
	}

	# Object to contain the dataset for all years of early values
	data.early <- matrix(NA,sum(length.iso.year[,2]),6)

	# Load the first year of early values
	setwd(.DATA)
	load(paste("iso.year.early",y[1],".rda",sep=""))

	# Constants used in next loop to set increment
	a <- 1
	b <- length.iso.year[1,2]

	# Loop to fill the first year of data.early
	for(i in 1:6){
		data.early[a:b,i] <- iso.year.early[,i]
	}

	# Constant used in next loop to set increment
	c <- 0

	# Loop to fill all following years of data.early
	for(i in 2:length(y)){
		setwd(.DATA)
		load(paste("iso.year.early",y[i],".rda",sep=""))
		a <- length.iso.year[i-1,2]+1 + c
		b <- a + length.iso.year[i,2]-1
		for(j in 1:6){
			data.early[a:b,j] <- iso.year.early[,j]
		}
		c <- a-1
	}

	# Attribute column names to the dataset
	colnames(data.early) <- c("year","iso","date","temp","PAR","precip" )


	#MID
	# Object to contain the mid data from each year
	data.mid <- matrix(NA,sum(length.iso.year[,3]),6)

	# Load the first year of early values
	 setwd(.DATA)
	load(paste("iso.year.mid",y[1],".rda",sep=""))

	# Constants used in next loop to set increment
	a <- 1
	b <- length.iso.year[1,3]

	# Loop to fill the first year of data.mid
	for(i in 1:6){
		data.mid[a:b,i] <- iso.year.mid[,i]
	}

	# Constant used in next loop to set increment
	c <- 0

	# Loop to fill all following years of data.mid
	for(i in 2:length(y)){
	 	setwd(.DATA)
		load(paste("iso.year.mid",y[i],".rda",sep=""))
		a <- length.iso.year[i-1,3]+1 + c
		b <- a + length.iso.year[i,3]-1
		for(j in 1:6){
			data.mid[a:b,j] <- iso.year.mid[,j]
		}
		c <- a-1
	}

	# Attribute column names to the dataset
	colnames(data.mid) <- c("year","iso","date","temp","PAR","precip" )

	#Late
	# Object to contain the late data from each year
	data.late <- matrix(NA,sum(length.iso.year[,4]),6)

	# Load the first year of late values
	 setwd(.DATA)
	load(paste("iso.year.late",y[1],".rda",sep=""))

	# Constants used in next loop to set increment
	a <- 1
	b <- length.iso.year[1,4]

	# Loop to fill the first year of data.late
	for(i in 1:6){
		data.late[a:b,i] <- iso.year.late[,i]
	}

	# Constant used in next loop to set increment
	c <- 0

	# Loop to fill all following years of data.late
	for(i in 2:length(y)){
		 setwd(.DATA)
		load(paste("iso.year.late",y[i],".rda",sep=""))
		a <- length.iso.year[i-1,4]+1 + c
		b <- a + length.iso.year[i,4]-1
		for(j in 1:6){
			data.late[a:b,j] <- iso.year.late[,j]
		}
		c <- a-1
	}

	# Attribute column names to the dataset
	colnames(data.late) <- c("year","iso","date","temp","PAR","precip" )

	# Assignment to Global
	data.early <<- data.early
	data.mid <<- data.mid
	data.late <<- data.late


}
}
