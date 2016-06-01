\name{corrected.iso}
\alias{corrected.iso}
\title{
produces the atmospheric corrected values of isotopes 
}
\description{
set the common isotopic signal at a study site with EPS value and then apply the atmospheric correction to each intra-annual isotope value
}
\usage{
corrected.iso(y, isotope.data, atm.cor)
}
\arguments{
  \item{y}{
years to apply the correction to isotope values
}
  \item{isotope.data}{
standardised dataset for isotope values
}
  \item{atm.cor}{
values of atmopheric corrections to apply to the isotopic common signal
}
}
\value{
\item{mean.isotope}{common isotope signal}
\item{cor.iso}{corrected isotope signal}
\item{EPS}{Value of the EPS statistic}
}
\references{
Wigley et al., 1983
}
\author{
Alex Soudant
Maintainer: Alex Soudant <asoudant@hotmail.fr>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (y, isotope.data, atm.cor) 
{
    cor(isotope.data[, 3], isotope.data[, 4])
    cor(isotope.data[, 3], isotope.data[, 5])
    cor(isotope.data[, 4], isotope.data[, 5])
    rbar <- mean(c(1, 1, 1, cor(isotope.data[, 3], isotope.data[, 
        4]), cor(isotope.data[, 3], isotope.data[, 5]), cor(isotope.data[, 
        4], isotope.data[, 5])))
    n <- 3
    nrb <- n * rbar
    denom <- 1 + (n - 1) * rbar
    EPS <- nrb/denom
    mean.isotope <- matrix(NA, length(isotope.data[, 3]), 3)
    mean.isotope[, 1] <- isotope.data[, 1]
    mean.isotope[, 2] <- isotope.data[, 2]
    for (a in 1:length(isotope.data[, 3])) {
        mean.isotope[a, 3] <- mean(c(isotope.data[a, 3], isotope.data[a, 
            4], isotope.data[a, 5]))
    }
    cor.iso <- matrix(NA, length(mean.isotope[, 3]), 3)
    cor.iso[, 1] <- mean.isotope[, 1]
    cor.iso[, 2] <- mean.isotope[, 2]
    for (i in 1:length(y)) {
        cor.iso[, 3][cor.iso[, 2] == y[i]] <- mean.isotope[, 
            3][mean.isotope[, 2] == y[i]] + atm.cor[, 2][atm.cor[, 
            1] == y[i]]
    }
    mean.isotope <<- mean.isotope
    cor.iso <<- cor.iso
    EPS <<- EPS
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
