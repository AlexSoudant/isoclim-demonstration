\name{gompertz.calc.example}
\alias{gompertz.calc.example}
\title{ calculate dates for radial growth onset, cessation and length}
\description{
 calculate dates for radial growth onset, cessation and length by three methods to choose from (NEE,ET or PAR/T thresholds)
values of thresholds to determine the radial growth period can be changed in the iso.clim.R file.
}
\usage{
gs.calc(y, GS.method, cor.iso, treshold.Rad, treshold.T, treshold.NEE.onset, treshold.NEE.cessation, treshold.ET.onset, treshold.ET.cessation)
}
\arguments{
  \item{y}{study period over which to calculate dates for radial growth onset, cessation and length}
  \item{GS.method}{which method to use to calculate dates for radial growth onset, cessation and length}
  \item{cor.iso}{atmospheric corrected isotope time series}
  \item{treshold.Rad}{Threshold to apply on the PAR variable to calculate dates for radial growth onset and cessation}
  \item{treshold.T}{Threshold to apply on the air temperature variable to calculate dates for radial growth onset and cessation}
  \item{treshold.NEE.onset}{Threshold to apply on the NEE to calculate dates for radial growth onset}
  \item{treshold.NEE.cessation}{Threshold to apply on the NEE variable to calculate dates for radial growth cessation}
  \item{treshold.ET.onset}{Threshold to apply on the ET variable to calculate dates for radial growth onset}
  \item{treshold.ET.cessation}{Threshold to apply on the ET variable to calculate dates for radial growth cessation}
}
\details{Threshold values can be changed in iso.clim.R}
\value{
\item{data.gs}{the dates for onset and cessation of radial growth and the method used}
}
\author{Alex Soudant
Maintainer: Alex Soudant <asoudant@hotmail.fr>}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
