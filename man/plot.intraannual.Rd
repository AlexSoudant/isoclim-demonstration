\name{plot.intraannual}
\alias{plot.intraannual}
\title{set the linear regressions between intra-annual stable isotopes and climate}
\description{fit and plot the linear regressions between intra-annual stable isotopes
depending on tree-ring or growing season sections and three climate variables}
\usage{
plot.intraannual(y, climate.iso, data.early, data.mid, data.late)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{study period over which to fit and plot the linear regressions}
  \item{climate.iso}{intra-annual and time resolved isotope time series}
  \item{data.early}{intra-annual isotopes during the early section of tree-rings or growing season}
  \item{data.mid}{intra-annual isotopes during the mid section of tree-rings or growing season}
  \item{data.late}{intra-annual isotopes during the late section of tree-rings or growing season}
}
\value{
\item{lr.prop}{linear regressions properties (R squared and p values) for each combination of sections and climate variables}
}
\author{Alex Soudant
Maintainer: Alex Soudant <asoudant@hotmail.fr>}
\keyword{programming}
\keyword{isotope}
\keyword{climate}
