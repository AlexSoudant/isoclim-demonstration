corrected.iso <-
function(y,isotope.data,atm.cor)
{

#------------------------ EPS calculation --------------------------------------

	cor(isotope.data[,3],isotope.data[,4])
	cor(isotope.data[,3],isotope.data[,5])
	cor(isotope.data[,4],isotope.data[,5])

	rbar <- mean(c(1,1,1,cor(isotope.data[,3],isotope.data[,4]), cor(isotope.data[,3],isotope.data[,5]),cor(isotope.data[,4],isotope.data[,5])))
	n <- 3
	nrb <- n * rbar
	denom <-    1+(n-1)*rbar

	EPS <-  nrb / denom

#--------------- Calculation of the mean time serie ----------------------------

	mean.isotope <- matrix(NA,length(isotope.data[,3]),3)

	mean.isotope[,1] <- isotope.data[,1]
	mean.isotope[,2] <- isotope.data[,2]

	for(a in 1: length(isotope.data[,3])){
		mean.isotope[a,3] <- mean(c(isotope.data[a,3],isotope.data[a,4],isotope.data[a,5]))
	}

#--------------- Atmospheric correction ---------------------------------------- 

	cor.iso <- matrix(NA,length(mean.isotope[,3]),3)
	cor.iso[,1] <- mean.isotope[,1]
	cor.iso[,2] <- mean.isotope[,2]

	for(i in 1:length(y)){
		cor.iso[,3][cor.iso[,2]==y[i]]<-mean.isotope[,3][mean.isotope[,2]==y[i]] + atm.cor[,2][atm.cor[,1] == y[i]]
	}

	# assignment to global
	mean.isotope <<- mean.isotope
	cor.iso <<- cor.iso
	EPS <<- EPS
}
