\name{feat.division}
\alias{feat.division}
\title{
divide 'feat' in 3 sections (tree-rings or growing season)
}
\description{
divide 'feat' in 3 sections for statistical analysis purposes
}
\usage{
feat.division(y, division, climate.iso)
}
\arguments{
  \item{y}{
years to apply the division
}
  \item{division}{
'feat' to divide
}
  \item{climate.iso}{
time resolved and corrected intra-annual carbon isotopes dataset
}
}
\value{
\item{data.early}{early range isotope values}
\item{data.mid}{middle range isotope values}
\item{data.late}{late range isotope values}
}
\author{
Alex Soudant
Maintainer: Alex Soudant <asoudant@hotmail.fr>
}
\keyword{ division }
\keyword{ programming }
