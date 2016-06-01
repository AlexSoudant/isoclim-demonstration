\name{interpolation.tech}
\alias{interpolation.tech}
\title{standardise raw isotope dataset}
\description{standardise raw isotope dataset to match same length 
according to ring width percentage}
\usage{
interpolation.tech(y, iso1, iso2, iso3)
}
\arguments{
  \item{y}{study period over which to standardise the raw isotope dataset}
  \item{iso1}{first raw isotope time series}
  \item{iso2}{second raw isotope time series}
  \item{iso3}{third raw isotope time series}
}
\details{
this interpolation technique consider the isotope time series with the smallest number of isotope measurements to be the reference for a specific year.
The two other time series isotope measurements are then degraded to match the reference. 
This procedure avoid the creation of an auto-correlated component in the standardised time series. 
}
\value{
  \item{isotope.data}{dataset with the three standardised isotope time series}
}
\author{Alex Soudant
Maintainer: Alex Soudant <asoudant@hotmail.fr>}
\keyword{ programming }
\keyword{ interpolation }
