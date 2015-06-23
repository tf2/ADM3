\name{mapBreak}
\alias{mapBreak}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ mapBreak. }
\description{
Trys to fine mapping break point locations.
}
\usage{
mapBreak(d,ind1,ind2,mr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{ - input data. }
  \item{ind1}{ - break point start position. }
  \item{ind2}{ - break point stop position. }
  \item{mr}{ - minimum mean ratio acceptable. }
}
\details{
example
}
\value{
Indices of fine mapped break points.
}
\author{ Tomas William Fitzgerald }
\note{ No merging of adjacent feature is done - this is expect to be done during post-processing. }
\examples{
#exampleADM3()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
