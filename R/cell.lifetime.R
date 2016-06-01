cell.lifetime <-
function(){

# Object to contain the cell life time values for each percent of relative growth 
increment.division<- matrix(NA,100,2)
increment.enlargement <- matrix(NA,100,2)
increment.wallthick <- matrix(NA,100,2)
increment.total <- matrix(NA,100,2)

# Column 1 of increment.lifetime = percents
increment.division[,1] <- 1:100
increment.enlargement[,1] <- 1:100
increment.wallthick[,1] <- 1:100
increment.total[,1] <- 1:100

# Cell division increment life time = 0
increment.division[,2] <- rep(0,100)

# Cell enlargement increment life time = 23 days to 5 days
increment.enlargement[,2] <- c(rep(23,10),rep(21,10),rep(19,10),rep(17,10),rep(15,10),rep(13,10),rep(11,10),rep(9,10),rep(7,10),rep(5,10))

# secondary wall thickening increment life time = 15 days to 51 days
increment.wallthick[,2] <- c(rep(15,10),rep(19,10),rep(23,10),rep(27,10),rep(31,10),rep(35,10),rep(39,10),rep(43,10),rep(47,10),rep(51,10))

# total growth increment life time = 15+23 days to 5+51 days
increment.total[,2] <- c(rep(15+23,10),rep(19+21,10),rep(23+19,10),rep(27+17,10),rep(31+15,10),rep(35+13,10),rep(39+11,10),rep(43+9,10),rep(47+7,10),rep(51+5,10))

increment.division <<- increment.division 
increment.enlargement <<- increment.enlargement
increment.wallthick <<- increment.wallthick
increment.total <<- increment.total
}
